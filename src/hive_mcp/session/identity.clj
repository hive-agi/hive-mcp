(ns hive-mcp.session.identity
  "Session identity and the HCR ownership algebra wrap scopes itself with.

   Wrap used to have no session identity at all: crystal/session-id was the
   calendar date, so every concurrent session on a box shared one bucket and a
   single wrap could clear rows it never harvested. This namespace supplies the
   missing identity as DATA, and the rules for deciding which rows a wrap owns.

   It is PURE. Nothing here touches DataScript, the swarm store or the clock.
   Callers hand in a `world` snapshot (DIP: the store is a parameter, not a
   dependency) and get plain maps back, so the whole algebra is testable
   without a running swarm.

   A SessionRef:

     {:session/id        \"c-9f3a...\"   ;; stable for the session's life
      :session/kind      :coordinator   ;; :coordinator | :ling | :adhoc
      :session/parent-id \"c-9f3a...\"   ;; nil for a coordinator or a loose adhoc
      :session/depth     0              ;; 0 coordinator, 1+ ling
      :session/project-id \"hive\"
      :session/agent-id  \"swarm-worker-123\"}

   The world snapshot, as projected from the swarm store:

     {:slaves       {agent-id -> {:slave/id :slave/parent-id :slave/depth
                                  :slave/project-id :slave/session-id}}
      :coordinators {coordinator-id -> {:coordinator/id :coordinator/session-id
                                        :coordinator/project :coordinator/status}}}

   HCR rules encoded here (the same shape kanban and memory already use --
   child sees parent, parent aggregates descendants):

     - a coordinator owns its own session and every descendant session;
     - a ling never owns a sibling's rows, only its own;
     - an ad-hoc session (a bare editor session with no slave row) is adopted
       by the nearest LIVE coordinator sharing its project scope, and by
       exactly one, so it is consumed once;
     - walks are cycle-safe and bounded, so nesting terminates."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Schemas
;; =============================================================================

(def session-kinds
  "The closed set of session kinds. :adhoc is a session with no slave row --
   a bare editor session, which is the common case for a human's own shell."
  #{:coordinator :ling :adhoc})

(def SessionKind
  [:enum :coordinator :ling :adhoc])

(def SessionRef
  [:map
   [:session/id :string]
   [:session/kind SessionKind]
   [:session/parent-id {:optional true} [:maybe :string]]
   [:session/depth {:optional true} [:maybe :int]]
   [:session/project-id {:optional true} [:maybe :string]]
   [:session/agent-id {:optional true} [:maybe :string]]])

(def max-depth
  "Hard bound on any ancestor walk. A malformed :slave/parent cycle must not
   hang a wrap, and no real hierarchy is anywhere near this deep."
  32)

;; =============================================================================
;; Construction
;; =============================================================================

(defn- blank->nil [s]
  (when (and (string? s) (not (str/blank? s))) s))

(defn session-ref
  "Build a normalized SessionRef. Blank strings become nil, an unknown kind
   falls back to :adhoc, and a coordinator never keeps a parent."
  [{:keys [id kind parent-id depth project-id agent-id]}]
  (let [kind (if (contains? session-kinds kind) kind :adhoc)]
    {:session/id         (blank->nil id)
     :session/kind       kind
     :session/parent-id  (when (not= :coordinator kind) (blank->nil parent-id))
     :session/depth      (cond
                           (= :coordinator kind) 0
                           (int? depth)          depth
                           :else                 nil)
     :session/project-id (blank->nil project-id)
     :session/agent-id   (blank->nil agent-id)}))

(defn valid?
  "A SessionRef is usable only if it carries an id. Everything else may be nil
   -- an ad-hoc session legitimately knows nothing but its own id."
  [ref]
  (boolean (and (map? ref)
                (blank->nil (:session/id ref))
                (contains? session-kinds (:session/kind ref)))))

;; =============================================================================
;; Resolution against a world snapshot
;; =============================================================================

(defn- coordinator-for-project
  "The live coordinator bound to `project-id`, if there is one. Ties are broken
   by coordinator id so the choice is deterministic: adoption must not depend on
   map ordering, or two wraps could each believe they own the same ad-hoc rows."
  [{:keys [coordinators]} project-id]
  (when project-id
    (->> (vals coordinators)
         (filter #(= project-id (:coordinator/project %)))
         (filter #(not= :terminated (:coordinator/status %)))
         (sort-by :coordinator/id)
         first)))

(defn- slave-chain
  "Ancestor chain of slave maps for `agent-id`, nearest first, itself excluded.
   Cycle-safe: a repeated id or `max-depth` hops ends the walk."
  [{:keys [slaves]} agent-id]
  (loop [id   (get-in slaves [agent-id :slave/parent-id])
         seen #{agent-id}
         acc  []]
    (if (or (nil? id) (contains? seen id) (>= (count acc) max-depth))
      acc
      (let [s (get slaves id)]
        (if (nil? s)
          acc
          (recur (:slave/parent-id s) (conj seen id) (conj acc s)))))))

(defn root-slave
  "The depth-0 ancestor of `agent-id` (its coordinator), or nil when the chain
   never reaches one."
  [world agent-id]
  (->> (slave-chain world agent-id)
       (filter #(= 0 (:slave/depth %)))
       first))

(defn resolve-ref
  "Resolve the SessionRef for a caller described by `{:agent-id :project-id
   :session-id}` against a `world` snapshot.

   A caller with a slave row is a :coordinator at depth 0 and a :ling otherwise,
   and a ling's :session/parent-id is its root slave's session. A caller with no
   slave row is :adhoc: it keeps whatever id it was given (its own editor
   session uuid), and takes the live coordinator of its project as parent when
   there is one, which is what makes it adoptable.

   Never invents an id. A caller that supplies none and has no slave row comes
   back with :session/id nil, which `valid?` rejects -- silently inventing one
   would make every such session look like a distinct owner."
  [world {:keys [agent-id project-id session-id]}]
  (let [slave (get-in world [:slaves agent-id])]
    (cond
      (and slave (= 0 (:slave/depth slave)))
      (session-ref {:id         (or (:slave/session-id slave) session-id agent-id)
                    :kind       :coordinator
                    :depth      0
                    :project-id (or (:slave/project-id slave) project-id)
                    :agent-id   agent-id})

      slave
      (let [root (root-slave world agent-id)]
        (session-ref {:id         (or (:slave/session-id slave) session-id agent-id)
                      :kind       :ling
                      :parent-id  (:slave/session-id root)
                      :depth      (:slave/depth slave)
                      :project-id (or (:slave/project-id slave) project-id)
                      :agent-id   agent-id}))

      :else
      (let [coord (coordinator-for-project world project-id)]
        (session-ref {:id         (or session-id agent-id)
                      :kind       :adhoc
                      :parent-id  (:coordinator/session-id coord)
                      :project-id project-id
                      :agent-id   agent-id})))))

;; =============================================================================
;; Ownership (the HCR rules)
;; =============================================================================

(defn ancestor-session-ids
  "Session ids on the path from `session-id` up to its root, nearest first,
   itself excluded. `parent-of` maps a session id to its parent's id.
   Cycle-safe and bounded by `max-depth`."
  [parent-of session-id]
  (loop [id   (parent-of session-id)
         seen #{session-id}
         acc  []]
    (if (or (nil? id) (contains? seen id) (>= (count acc) max-depth))
      acc
      (recur (parent-of id) (conj seen id) (conj acc id)))))

(defn descendant-of?
  "True when `session-id` sits anywhere below `ancestor-id`. A session is NOT
   its own descendant -- `owns?` adds the reflexive case explicitly, because
   the two readings differ where it matters (clearing rows)."
  [parent-of session-id ancestor-id]
  (boolean (and session-id ancestor-id
                (some #{ancestor-id} (ancestor-session-ids parent-of session-id)))))

(defn owns?
  "Does the wrap running as `ref` own a row tagged `row-session-id`?

   A coordinator owns its own session and every descendant of it. Every other
   kind owns only its own session: a ling that could clear its siblings' rows
   is the bug this namespace exists to remove."
  [parent-of ref row-session-id]
  (let [self (:session/id ref)]
    (boolean
     (and self row-session-id
          (or (= self row-session-id)
              (and (= :coordinator (:session/kind ref))
                   (descendant-of? parent-of row-session-id self)))))))

(defn adoptable?
  "May the wrap running as `ref` adopt the ad-hoc session described by `cand`?

   Only a coordinator adopts, only within its own project scope, and only a
   session that is not already owned by a DIFFERENT live coordinator. An
   ad-hoc session whose parent is already this coordinator is adoptable; one
   pointing at another coordinator is that coordinator's to consume."
  [ref {:keys [session-id kind project-id parent-id]}]
  (boolean
   (and (= :coordinator (:session/kind ref))
        (= :adhoc kind)
        session-id
        (not= session-id (:session/id ref))
        (some? (:session/project-id ref))
        (= project-id (:session/project-id ref))
        (or (nil? parent-id) (= parent-id (:session/id ref))))))

(defn partition-rows
  "Split `rows` into {:own :adopted :foreign} for the wrap running as `ref`.

   `row->session` reads a row's session id, `row->candidate` describes it for
   the adoption test (nil for rows that are not ad-hoc sessions). Rows with no
   session id at all land in :foreign: they predate session tagging, and
   guessing at their owner is what caused the original data loss."
  [{:keys [parent-of row->session row->candidate]
    :or   {row->candidate (constantly nil)}} ref rows]
  (reduce
   (fn [acc row]
     (let [sid (row->session row)]
       (cond
         (owns? parent-of ref sid)                    (update acc :own conj row)
         (adoptable? ref (row->candidate row))        (update acc :adopted conj row)
         :else                                        (update acc :foreign conj row))))
   {:own [] :adopted [] :foreign []}
   rows))

(defn harvestable
  "The rows a wrap running as `ref` may read AND clear: its own subtree plus
   what it adopted. Deliberately the only place the two are joined, so a caller
   cannot clear :foreign by reaching for the wrong key."
  [opts ref rows]
  (let [{:keys [own adopted]} (partition-rows opts ref rows)]
    (into (vec own) adopted)))
