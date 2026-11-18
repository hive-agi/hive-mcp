(ns hive-mcp.crystal.consolidate
  "Same-day consolidation of wrap summaries (SYNTHESIS-HISTORY slice (b)).

   A re-wrap of the same project on the same local day folds INTO the summary
   already stored for that [scope, local-day] instead of fanning out another
   one: the entry is rewritten in place, so its id (and every KG edge hanging
   off it, including the :supersedes edge slice (a) wrote) survives.

     Collect   ISummaryChain/-prior-summaries   same-project summaries
     Promote   day-of, existing-for-day         pure choice of the entry to fold into
     Boundary  persist/upgrade-one!             the in-place rewrite (persist.clj)
     Pipeline  folds-for, apply-folds           one fold decision per project

   ## Why a second selector instead of `chain/previous-summary`

   That one asks 'which summary do I follow?' and answers the newest id
   strictly below the new one. Consolidation asks a different question, 'is a
   summary for TODAY already stored?', and a same-day re-wrap must NOT be read
   as a normal successor. Sharing the selector would make the two answers
   indistinguishable, so the day predicate lives here.

   ## The day key

   The day is the LOCAL CALENDAR DATE, read from the `session:<yyyy-MM-dd>` tag
   every wrap already carries (`hive-mcp.crystal.core/session-tags`, stamped on
   every wrap by `crystal.core` and `crystal.synthesis`). The tag is preferred
   over parsing the entry id because the tag records what the writer chose and
   an id is 14 digits whose first 8 happen to be a date; the id prefix is kept
   only as a fallback for a row that carries no `session:` tag. Note that this
   is `session-tag`, NOT the `session-uid:` tag: the date is a calendar day and
   explicitly not an identity -- every concurrent session on a box shares it.

   ## What is deliberately NOT done here

   Tags are not rewritten to re-stamp the day on a folded entry: the surviving
   entry keeps the tags the synthesis gave it, and because its id is still
   today's, the id fallback finds it on the next same-day wrap even if a
   future synthesiser stops emitting the `session:` tag. `persist/upgrade-one!`
   re-stamps :duration and :expires from the new wrap, so consolidation
   EXTENDS the surviving entry's life rather than shortening it (a wrap
   defaults to :short); nothing here drops or shortens a stored entry."
  (:require [clojure.string :as str]
            [hive-mcp.crystal.chain :as chain]
            [hive-mcp.crystal.persist :as persist]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Domain
;; =============================================================================

(def Day
  "A local calendar date, `yyyy-MM-dd`: the day half of a consolidation key."
  [:re {:gen/elements ["2026-11-01" "2026-11-17" "2026-12-25"]} #"\d{4}-\d{2}-\d{2}"])

(def WrapRef
  "The part of a `crystal.persist/Wrap` a fold decision reads, plus the
   `:consolidate-into` marker `apply-folds` stamps. A `:map` is open, so the
   marker is legal on both sides of the contract."
  [:map
   [:pid :any]
   [:entry {:optional true} [:maybe :map]]
   [:consolidate-into {:optional true} [:maybe :string]]])

(def FoldReport
  "`folds-for` result: the entry to fold into per project-id, how many project
   reads failed, and how many were attempted."
  [:map
   [:folds [:map-of :string :string]]
   [:failed [:int {:min 0}]]
   [:scanned [:int {:min 0}]]])

;; =============================================================================
;; Promote (pure)
;; =============================================================================

(def ^:private day-tag-prefix "session:")

(defn day-of
  "The local calendar day ENTRY belongs to, `yyyy-MM-dd`, or nil when it
   carries neither a `session:` day tag nor a usable id prefix.

   A `session-uid:` tag is not a day and is skipped (its value is an opaque
   id). A day tag wins over the id prefix because the tag is what the writer
   recorded, and the id prefix is an inference from a 14-digit id."
  [entry]
  (or (some (fn [t]
              (when (and (string? t)
                         (str/starts-with? t day-tag-prefix))
                (let [v (subs t (count day-tag-prefix))]
                  (when (re-matches #"\d{4}-\d{2}-\d{2}" v) v))))
            (:tags entry))
      (let [id (:id entry)]
        (when (and (string? id) (>= (count id) 8))
          (let [d (subs id 0 8)]
            (when (re-matches #"\d{8}" d)
              (str (subs d 0 4) "-" (subs d 4 6) "-" (subs d 6 8))))))))

(m/=> day-of [:=> [:cat [:maybe [:map [:id {:optional true} [:maybe :string]]
                                  [:tags {:optional true} [:maybe [:sequential :string]]]]]]
                   [:maybe Day]])

(defn existing-for-day
  "Id of the summary already stored for PROJECT-ID on TODAY, or nil.

   The greatest eligible id wins, so a day that somehow holds two summaries
   folds into the newer one -- and because a store is not ordered, the max is
   taken here rather than trusted from `:order-by`.

   Eligibility is slice (a)'s: a session summary (`chain/summary?`), in scope
   (`chain/in-scope?`, which matches by :project-id OR the `scope:project:<id>`
   tag), whose `day-of` is TODAY. Ids are yyyyMMddHHmmss-prefixed, so the max
   id of the day is that day's newest entry."
  [project-id today candidates]
  (reduce (fn [best {:keys [id] :as c}]
            (if (and (string? id)
                     (not (str/blank? id))
                     (chain/summary? c)
                     (chain/in-scope? project-id c)
                     (= today (day-of c))
                     (or (nil? best) (pos? (compare id best))))
              id
              best))
          nil
          candidates))

(m/=> existing-for-day [:=> [:cat :string Day [:sequential chain/Candidate]] [:maybe :string]])

;; =============================================================================
;; Pipeline
;; =============================================================================

(defn- wrap-project-id
  "The store :project-id a wrap is written under, or nil when it has none.

   Resolved through `persist/pid->project-id`, the one function that maps a
   fan-out pid to a store scope, so the umbrella sentinel is keyed by
   `multi-project` here exactly as `persist` writes it. A blank or nil pid has
   no key, and a wrap without a key is never folded -- it is written fresh."
  [{:keys [pid]}]
  (let [p (persist/pid->project-id pid)]
    (when-not (str/blank? p) p)))

(defn folds-for
  "Which projects in WRAPS already have a summary stored for TODAY:
   `{:folds {project-id id} :failed n :scanned n}`.

   WRAPS is what the wrap pipeline would persist (the `crystal.persist/Wrap`
   shape, `{:pid :entry}`). One read per distinct project, so a caller may pass
   the mechanical and the synthesised wrap sets together and get ONE decision
   shared by both.

   A read failure for one project is counted in `:failed` and folds nothing for
   it: a wrap is never dropped because the store could not answer, it is
   written fresh, which is the pre-consolidation behaviour. Never throws."
  [chain wraps today]
  (let [pids (distinct (keep wrap-project-id wraps))]
    (reduce (fn [acc pid]
              (let [acc (update acc :scanned inc)]
                (try
                  (if-let [id (existing-for-day
                               pid today
                               (vec (chain/-prior-summaries chain pid)))]
                    (assoc-in acc [:folds pid] id)
                    acc)
                  (catch Throwable _
                    (update acc :failed inc)))))
            {:folds {} :failed 0 :scanned 0}
            pids)))

(m/=> folds-for [:=> [:cat :any [:sequential WrapRef] Day] FoldReport])

(defn apply-folds
  "WRAPS with each wrap whose project is in FOLDS tagged
   `:consolidate-into <id>`, which is what `persist/persist-wraps!` rewrites
   in place instead of adding. A wrap with no entry in FOLDS is returned
   unchanged, so today's behaviour is the default."
  [wraps folds]
  (mapv (fn [{:keys [pid] :as w}]
          (if-let [id (get folds (wrap-project-id w))]
            (assoc w :consolidate-into id)
            w))
        wraps))

(m/=> apply-folds [:=> [:cat [:sequential WrapRef] [:map-of :string :string]]
                        [:vector WrapRef]])
