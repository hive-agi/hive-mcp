(ns hive-mcp.dispatch.branches
  "Runtime composition of consolidated tool trees: anyone may extend or wrap ANY
   path of ANY tool, at any depth, while the server runs.

   hive-addon.registry.commands mounts whole subdomains at a tool's first level,
   last write wins. hive-mcp.dispatch.verbs mounts one verb under a root that
   opted in. Neither lets addon B add a verb beneath addon A's subdomain, and
   neither can touch a handler that routes its own remainder. A BRANCH can:

     graft    `git belt hello` beside what `git belt` already routes
     around   middleware over one verb, or over everything below a path

   The tree a tool dispatches on is not edited. Branches live in their own store
   and `fold` applies them per call on top of whatever core and the addons
   contribute at that moment, so a branch survives a remount of the addon it
   extends, and a reload of either side is seen by the next call.

   A leaf that routes its own remainder (`code carto`) is SPLIT, never replaced:
   it becomes {:_handler leaf, seg branch}. The leaf still receives the whole
   :command for every verb it owns, so its owner changes nothing.

   Conflict rule, same as the verb seam: a graft never shadows what exists.
   Replacing behaviour is what `around` is for, and it is explicit: an around
   that does not call `next` replaces, and it is listed as an around.

   Stratified: the top half is pure; the registry is the boundary at the bottom."
  (:require [clojure.string :as str]
            [hive-mcp.dispatch.handler :as handler]
            [malli.core :as m]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Value objects
;; =============================================================================

(def Path
  "A route through a handler tree, as keywords. [] names the whole tool."
  [:vector :keyword])

(def Branch
  "One registered branch. A :graft carries :handler (a handler, a verb map, or a
   var holding either). An :around carries :wrap, (fn [next] handler)."
  [:map
   [:kind [:enum :graft :around]]
   [:path Path]
   [:owner :string]
   [:handler {:optional true} :any]
   [:wrap {:optional true} :any]
   [:order {:optional true} :int]
   [:params {:optional true} [:maybe :map]]
   [:summary {:optional true} [:maybe :string]]
   [:description {:optional true} [:maybe :string]]])

(def Refusal
  "Why a registered branch does not route right now."
  [:map
   [:kind [:enum :graft :around]]
   [:path Path]
   [:owner :string]
   [:reason [:enum :branch/occupied :branch/shadows-verb :branch/no-target]]])

;; =============================================================================
;; Pure: paths
;; =============================================================================

(defn parse-path
  "PATH as a vector of keywords: \"belt hello\", [\"belt\" :hello] or :belt. Blank
   or nil is [], which names the whole tool."
  [path]
  (let [seg->kw (fn [s] (if (keyword? s) s (keyword (str s))))]
    (cond
      (nil? path)        []
      (keyword? path)    [path]
      (string? path)     (mapv keyword (remove str/blank? (str/split (str/trim path) #"\s+")))
      (sequential? path) (mapv seg->kw path)
      :else              [(seg->kw path)])))

(m/=> parse-path [:=> [:cat :any] Path])

(defn path-str
  "PATH as the command string a caller sends."
  [path]
  (str/join " " (map name path)))

(m/=> path-str [:=> [:cat Path] :string])

(defn flatten-graft
  "Every [path leaf] a graft of NODE at PATH makes routable. A verb map becomes
   one entry per leaf, so two grafts merge with each other and with the tree key
   by key rather than one replacing the other. Read through `handler/current`,
   and a leaf is returned UNRESOLVED so a var stays a var."
  [path node]
  (let [cur (handler/current node)]
    (if (map? cur)
      (vec (mapcat (fn [[k v]] (flatten-graft (conj path (keyword k)) v)) cur))
      [[path node]])))

(m/=> flatten-graft [:=> [:cat Path :any] [:vector [:tuple Path :any]]])

(defn- key-in
  "The key SEG goes by in M: trees are keyed by keyword, a few by string."
  [m seg]
  (cond
    (contains? m seg)        seg
    (contains? m (name seg)) (name seg)
    :else                    seg))

;; =============================================================================
;; Pure: graft
;; =============================================================================

(defn- occupied?
  "Does the tree already route exactly PATH?"
  [tree path]
  (loop [node tree, p (seq path)]
    (let [cur (handler/current node)]
      (cond
        (empty? p)    (some? cur)
        (map? cur)    (let [k (key-in cur (first p))]
                        (and (contains? cur k) (recur (get cur k) (next p))))
        ;; a leaf IS its own default route
        :else         (and (handler/handler? cur) (= [:_handler] (vec p)))))))

(defn- shadows-declared-verb?
  "Would a graft below an opaque first-level root take a verb that root says it
   routes? The tree cannot enumerate such a root, so the question goes to the
   DECLARED port, (declared root-keyword) -> verb names or nil. Only that level
   is answerable: nothing declares what lives deeper in an opaque handler."
  [tree path declared]
  (when (and declared (= 2 (count path)))
    (let [[root verb] path]
      (when-not (map? (handler/current (get tree (key-in tree root))))
        (contains? (into #{} (map name) (declared root)) (name verb))))))

(defn- assoc-path
  "NODE with LEAF at PATH, every level read through `handler/current`. A leaf on
   the way down is split: it keeps routing as the :_handler of the new subtree,
   held as it was registered so a var stays reload-transparent. A level this
   creates below such a default inherits it (DEFAULT), or grafting `sub a b` would
   take `sub a c` away from the handler that routes it. Map metadata survives,
   which is what the verb seam's ::root lives in."
  ([node path leaf] (assoc-path node path leaf nil))
  ([node [seg & more :as path] leaf default]
   (if (empty? path)
     leaf
     (let [cur (handler/current node)
           m   (cond (map? cur)             cur
                     (handler/handler? cur) {:_handler node}
                     (some? default)        {:_handler default}
                     :else                  {})
           k   (key-in m seg)]
       (assoc m k (assoc-path (get m k) more leaf (or (:_handler m) default)))))))

;; =============================================================================
;; Pure: around
;; =============================================================================

(defn- wrap-leaves
  "NODE with WRAP-LEAF applied to every handler at or below it."
  [node wrap-leaf]
  (let [cur (handler/current node)]
    (cond
      (map? cur)             (reduce-kv (fn [m k v] (assoc m k (wrap-leaves v wrap-leaf))) cur cur)
      (handler/handler? cur) (wrap-leaf node)
      :else                  node)))

(defn- advise-path
  "NODE with WRAP-LEAF over everything PATH names, or ::no-target.

   A segment no map lists falls to that map's :_handler, and a leaf is split, in
   both cases keeping the verb's own route: the delegate receives the whole
   :command either way, so a verb that only an opaque handler knows about can be
   wrapped without its owner's help."
  [node [seg & more :as path] wrap-leaf]
  (let [cur (handler/current node)]
    (cond
      (empty? path)
      (wrap-leaves node wrap-leaf)

      (map? cur)
      (let [k       (key-in cur seg)
            target  (if (contains? cur k) (get cur k) (:_handler cur))
            advised (if (some? target) (advise-path target more wrap-leaf) ::no-target)]
        (if (= ::no-target advised) ::no-target (assoc cur k advised)))

      (handler/handler? cur)
      (let [advised (advise-path node more wrap-leaf)]
        (if (= ::no-target advised) ::no-target {:_handler node seg advised}))

      :else ::no-target)))

(defn- failure
  "The answer of a branch that threw: who, where, and how to take it out."
  [{:keys [kind path owner]} tool t]
  {:type    "text"
   :isError true
   :text    (str (name kind) " branch '" tool " " (path-str path) "' of " owner " threw: "
                 (or (ex-message t) (str t))
                 ". Take it out with: hot command=\"branch remove\" tool=\"" tool
                 "\" path=\"" (path-str path) "\" owner=\"" owner "\"")})

(defn- guarded
  "F, answering `failure` rather than throwing."
  [branch tool f]
  (fn [params]
    (try (f params)
         (catch Throwable t (failure branch tool t)))))

(defn- around-handler
  "BRANCH's wrap over NEXT. The wrap is called per dispatch through whatever holds
   it, so a var reaches a reload, and a failure in the wrap or below it is
   answered by `guarded`, naming the branch."
  [{:keys [wrap] :as branch} tool next-handler]
  (guarded branch tool (fn [params] ((wrap next-handler) params))))

;; =============================================================================
;; Pure: fold
;; =============================================================================

(defn- refuse [{:keys [kind path owner]} reason]
  {:kind kind :path path :owner owner :reason reason})

(defn- fold-graft
  "ACC {:tree :refusals} with BRANCH's leaves added, each leaf judged alone: the
   free ones route and the taken ones are refused."
  [acc {:keys [path handler] :as branch} tool {:keys [declared wrap-leaf]}]
  (reduce
   (fn [{:keys [tree] :as acc} [leaf-path leaf]]
     (let [leaf-branch (assoc branch :path leaf-path)
           reason      (cond
                         (occupied? tree leaf-path)
                         :branch/occupied
                         (shadows-declared-verb? tree leaf-path declared)
                         :branch/shadows-verb)]
       (if reason
         (update acc :refusals conj (refuse leaf-branch reason))
         (let [routed (guarded leaf-branch tool (wrap-leaf leaf-branch leaf))]
           (assoc acc :tree (assoc-path tree leaf-path routed))))))
   acc
   (flatten-graft path handler)))

(defn- fold-around
  [{:keys [tree] :as acc} {:keys [path] :as branch} tool]
  (let [advised (advise-path tree path (partial around-handler branch tool))]
    (if (= ::no-target advised)
      (update acc :refusals conj (refuse branch :branch/no-target))
      (assoc acc :tree advised))))

(defn fold
  "TREE with BRANCHES applied, for the tool named TOOL. Refusals go under
   ::refusals in the result's metadata; TREE's own metadata is kept.

   Grafts first, shallowest path first, so a graft at [:a] and one at [:a :b]
   compose whichever arrived first. Arounds after, so one can wrap a graft.
   Lowest :order is OUTERMOST; ties break by owner, so the result does not depend
   on arrival order.

   OPTS:
     :declared   (fn [root-keyword]) -> verbs an opaque root says it routes
     :wrap-leaf  (fn [branch leaf]) -> leaf as dispatched (metrics, drain gate)"
  [tree branches tool opts]
  (let [opts   (update opts :wrap-leaf #(or % (fn [_ leaf] leaf)))
        {:keys [graft around]} (group-by :kind branches)
        grafts (sort-by (juxt (comp count :path) (comp path-str :path) :owner) graft)
        wraps  (sort-by (juxt #(- (or (:order %) 0)) :owner) around)
        {tree' :tree refusals :refusals}
        (as-> {:tree tree :refusals []} acc
          (reduce #(fold-graft %1 %2 tool opts) acc grafts)
          (reduce #(fold-around %1 %2 tool) acc wraps))]
    (vary-meta tree' assoc ::refusals refusals)))

(m/=> fold [:=> [:cat :map [:sequential Branch] :string :map] :map])

(defn surface
  "What BRANCHES add to a tool's advertised surface: :names, the command strings
   the grafts make callable; :params, every declared param; :lines, one
   [command purpose] per branch. Pure, so it reports what was asked for and a
   refused branch shows up in `refusals` rather than being hidden here."
  [branches]
  (let [leaves (for [{:keys [kind path handler]} branches
                     :when (= :graft kind)
                     [p _] (flatten-graft path handler)]
                 (vec (remove #{:_handler} p)))]
    {:names  (vec (sort (distinct (map path-str (remove empty? leaves)))))
     :params (not-empty (apply merge (keep :params branches)))
     :lines  (vec (for [{:keys [kind path summary description]} (sort-by (comp path-str :path) branches)]
                    [(path-str path) (str (when (= :around kind) "[around] ")
                                          (or summary (some-> description str/split-lines first) ""))]))}))

(m/=> surface [:=> [:cat [:sequential Branch]]
               [:map [:names [:vector :string]] [:params [:maybe :map]] [:lines [:vector :any]]]])

;; =============================================================================
;; Boundary: the registry
;; =============================================================================

;; {tool {[kind path owner] Branch}}
(defonce ^:private registry (atom {}))

;; {listener-id f}, called with {:type :branch|:prune :tool-name t :addon-id owner}
(defonce ^:private listeners (atom {}))

;; {tool [Refusal]}, as of the tool's latest dispatch
(defonce ^:private refusals-now (atom {}))

;; #{[tool Refusal]} already logged
(defonce ^:private logged (atom #{}))

(defn owner-id
  "OWNER as the string a branch is filed under. A keyword addon id keeps its
   namespace, so :hive/agent and \"hive/agent\" are one owner."
  [owner]
  (cond (keyword? owner) (subs (str owner) 1)
        (nil? owner)     nil
        :else            (str owner)))

(m/=> owner-id [:=> [:cat :any] [:maybe :string]])

(defn add-listener!
  "Call F after every branch change. Idempotent by id. A listener that throws is
   ignored: it must never break a contribution."
  [listener-id f]
  (swap! listeners assoc listener-id f)
  listener-id)

(m/=> add-listener! [:=> [:cat :any ifn?] :any])

(defn remove-listener!
  "Drop a listener. Returns the id."
  [listener-id]
  (swap! listeners dissoc listener-id)
  listener-id)

(m/=> remove-listener! [:=> [:cat :any] :any])

(defn- notify! [event]
  (doseq [[_ f] @listeners]
    (try (f event) (catch Throwable _ nil))))

(defn of
  "Every branch registered on TOOL."
  [tool]
  (vec (vals (get @registry tool))))

(m/=> of [:=> [:cat :any] [:vector Branch]])

(defn tool-names
  "Every tool carrying a branch, sorted."
  []
  (vec (sort (keep (fn [[tool bs]] (when (seq bs) tool)) @registry))))

(m/=> tool-names [:=> [:cat] [:vector :string]])

(defn refusals
  "The branches of TOOL that did not route at its latest dispatch."
  [tool]
  (get @refusals-now tool []))

(m/=> refusals [:=> [:cat :any] [:vector Refusal]])

(defn note-refusals!
  "Record REFUSALS as TOOL's current ones, logging each once. Returns nil."
  [tool found]
  (swap! refusals-now assoc tool (vec found))
  (doseq [r found
          :let [k [tool r]]
          :when (not (contains? @logged k))]
    (swap! logged conj k)
    (log/warn "branch refused" (assoc r :tool tool)))
  nil)

(defn forget-fold!
  "Forget that TOOL folded, so `folded?` answers for the next dispatch. Returns nil."
  [tool]
  (swap! refusals-now dissoc tool)
  nil)

(m/=> forget-fold! [:=> [:cat :any] :nil])

(defn folded?
  "Has TOOL folded its branches since `forget-fold!`? A tool that took a call and
   did not fold does not dispatch through a tree, and no branch can reach it."
  [tool]
  (contains? @refusals-now tool))

(m/=> folded? [:=> [:cat :any] :boolean])

(m/=> note-refusals! [:=> [:cat :any [:sequential Refusal]] :nil])

(defn- other-graft-owner
  "Who else grafted any of the leaves GRAFT routes, on TOOL."
  [tool owner leaf-paths]
  (some (fn [{other :owner :as b}]
          (when (and (= :graft (:kind b)) (not= owner other)
                     (some (set leaf-paths) (map first (flatten-graft (:path b) (:handler b)))))
            other))
        (of tool)))

(defn- graftable?
  [node]
  (let [cur (handler/current node)]
    (or (handler/handler? cur)
        (and (map? cur) (seq cur)))))

(defn graft!
  "Route PATH of TOOL to SPEC's :handler, a handler or a verb map, on behalf of its
   :owner. Refused, with nothing written, when PATH is empty, the handler is not
   invocable, the owner is blank, or another owner grafted the same leaf.
   Re-grafting by the same owner replaces it (hot reload). A graft over something
   the tree already routes registers, and `refusals` names it: the tree may
   change, and the check is per dispatch.
   Returns {:ok? true ...} or {:ok? false :reason kw :message s}."
  [tool path {:keys [handler owner] :as spec}]
  (let [p     (parse-path path)
        o     (owner-id owner)
        label (str tool " " (path-str p))]
    (cond
      (empty? p)
      {:ok? false :reason :branch/empty-path
       :message (str tool ": a graft needs a path; to change the whole tool use around")}

      (not (graftable? handler))
      {:ok? false :reason :branch/not-invocable :message (str label ": handler is not invocable")}

      (str/blank? o)
      {:ok? false :reason :branch/no-owner :message (str label ": owner is required")}

      :else
      (if-let [other (other-graft-owner tool o (map first (flatten-graft p handler)))]
        {:ok? false :reason :branch/owned-by :owner other
         :message (str label " is already grafted by " other)}
        (do (swap! registry assoc-in [tool [:graft p o]]
                   (assoc (select-keys spec [:handler :params :summary :description])
                          :kind :graft :path p :owner o))
            (notify! {:type :branch :tool-name tool :addon-id o})
            {:ok? true :tool tool :path (path-str p) :owner o})))))

(m/=> graft! [:=> [:cat :string :any :map] [:map [:ok? :boolean]]])

(defn- self-locking?
  "An around over `hot branch` could stop the verb that takes it out."
  [tool path]
  (and (= "hot" tool)
       (or (empty? path) (= :branch (first path)))))

(defn around!
  "Wrap PATH of TOOL, and everything below it, in SPEC's :wrap, (fn [next] handler);
   [] wraps the whole tool. Several owners may wrap one path; lowest :order is
   outermost. Re-wrapping by the same owner replaces it. Refused for `hot branch`
   itself, which has to stay able to remove a broken around.
   Returns {:ok? true ...} or {:ok? false :reason kw :message s}."
  [tool path {:keys [wrap owner] :as spec}]
  (let [p     (parse-path path)
        o     (owner-id owner)
        label (str/trim (str tool " " (path-str p)))]
    (cond
      (not (handler/handler? wrap))
      {:ok? false :reason :branch/not-invocable
       :message (str label ": wrap must be (fn [next] handler)")}

      (str/blank? o)
      {:ok? false :reason :branch/no-owner :message (str label ": owner is required")}

      (self-locking? tool p)
      {:ok? false :reason :branch/self-locking
       :message (str label ": wrapping this could stop `hot branch remove` from working")}

      :else
      (do (swap! registry assoc-in [tool [:around p o]]
                 (assoc (select-keys spec [:wrap :order :params :summary :description])
                        :kind :around :path p :owner o))
          (notify! {:type :branch :tool-name tool :addon-id o})
          {:ok? true :tool tool :path (path-str p) :owner o}))))

(m/=> around! [:=> [:cat :string :any :map] [:map [:ok? :boolean]]])

(defn prune!
  "Withdraw OWNER's branches at exactly PATH of TOOL, of either kind. Returns how
   many went."
  [tool path owner]
  (let [p   (parse-path path)
        o   (owner-id owner)
        ks  (filter (partial contains? (get @registry tool {}))
                    [[:graft p o] [:around p o]])]
    (when (seq ks)
      (swap! registry update tool #(apply dissoc % ks))
      (notify! {:type :prune :tool-name tool :addon-id o}))
    (count ks)))

(m/=> prune! [:=> [:cat :string :any :any] :int])

(defn retract-owner!
  "Withdraw every branch OWNER registered, on every tool. Returns how many went."
  [owner]
  (let [o     (owner-id owner)
        found (vec (for [[tool bs] @registry
                         [k b] bs
                         :when (= o (:owner b))]
                     [tool k]))]
    (doseq [[tool k] found] (swap! registry update tool dissoc k))
    (doseq [tool (distinct (map first found))]
      (notify! {:type :prune :tool-name tool :addon-id o}))
    (count found)))

(m/=> retract-owner! [:=> [:cat :any] :int])

(defn clear!
  "Drop every branch. For tests."
  []
  (reset! registry {})
  (reset! refusals-now {})
  (reset! logged #{})
  nil)

(m/=> clear! [:=> [:cat] :nil])
