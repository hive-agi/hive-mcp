(ns hive-mcp.tools.catchup.bundle
  "Query orchestrators for the catchup workflow.

   Two entry points:
     - query-scoped-entries / query-expiring-entries / query-regular-conventions
       : per-type fork-joined scans used by spawn + catchup_session
     - query-catchup-bundle
       : single-pull metadata scan → type-bucket trim → batch hydrate,
         drives the catchup.clj main pipeline

   Depends on:
     hierarchy    — chunked project-id fan-out + metadata-projection
     scope_filter — pure scope filters + sort/dedupe helpers
     hydration    — phase-2 batch-get content pipeline"
  (:require [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.dns.result :refer [rescue]]
            [hive-mcp.tools.catchup.hierarchy :as hier]
            [hive-mcp.tools.catchup.scope-filter :as sf]
            [hive-mcp.tools.catchup.hydration :as hydr]
            [hive-weave.parallel :as wpar]
            [clojure.tools.logging :as log]
            [clojure.set :as set]
            [hive-mcp.resilience.store :refer [with-resilience]]
            [hive-mcp.tools.catchup.bundle-cache :as bc]
            [hive-mcp.tools.catchup.bucket-types :as bt]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private scoped-branch-budget-ms
  "Per-branch budget for the hierarchy / global-piercing fan-out. Each branch
   is its own Milvus RPC; giving each its own budget means a slow branch
   can't drag the faster one past the outer deadline. Sized for metadata-only
   scans: 300 rows at ~107ms/row (project-filtered, 5-field projection) lands
   around 32s. 60s leaves ~50% headroom for jitter + the fork-join wrapper."
  60000)

(def ^:private bundle-global-limit
  "Fetch cap for each global-pierce branch (`global-pierce-filters`)."
  200)

(def ^:private bundle-axioms-limit
  "Fetch cap for the unfiltered `type=axiom` branch, which catches axioms
   authored outside the hierarchy. Its rows pass the HCR gate: only
   `scope:global` axioms and those scoped to a visible project survive."
  200)

(def ^:private bundle-axiom-candidates-limit
  "Fetch cap for the unfiltered axiom-nomination branch; rows pass the HCR
   gate like axioms."
  25)

(def ^:private bundle-principles-limit
  "Fetch cap for the unfiltered `type=principle` branch; rows pass the HCR
   gate like axioms."
  100)

(def ^:private bundle-priority-principles-limit
  "Fetch cap for the catchup-priority principle branch. Sized to exceed the
   tagged population, not the display budget. [[20260817224345-1057fc80]]"
  500)

(def ^:private bundle-sessions-fresh-limit 25)

(def ^:private bundle-recent-wraps-limit
  "Fetch cap for the unfiltered `wrap-generated` note branch, ordered
   `:created :desc` and over-fetched ~20x the display cap of 10 because
   Milvus has no server-side ORDER BY. Rows pass the HCR gate."
  200)

;; =============================================================================
;; Telemetry wrapper — DI via hive-ttracking when present
;; =============================================================================

(def ^:private tt-timed-query-var
  "Late-bound reference to `hive-ttracking.core/timed-query`. Resolved lazily
   so this namespace still loads when the hive-ttracking addon is absent from
   the classpath (e.g. minimal CI builds). DI via classpath presence."
  (delay
    (rescue nil
            (require 'hive-ttracking.core)
            (resolve 'hive-ttracking.core/timed-query))))

(defn- timed-query-inline
  "Fallback telemetry wrapper used when hive-ttracking is absent.
   Matches tt/timed-query's single-arity signature."
  [label qfn]
  (fn []
    (let [t0 (System/currentTimeMillis)
          result (qfn)
          elapsed (- (System/currentTimeMillis) t0)
          n (count (or result []))]
      (if (zero? n)
        (log/warn "catchup bundle" label "returned 0 entries in" elapsed "ms"
                  "— may indicate Milvus stall, fork-join fallback, or genuinely empty scope")
        (log/info "catchup bundle" label ":" n "entries in" elapsed "ms"))
      result)))

(defn- timed-query
  "Dispatch to hive-ttracking.core/timed-query when available, else inline
   fallback. DI shape: addon decides telemetry emission; consumer only knows
   the label+thunk contract."
  [label qfn]
  (if-let [tt @tt-timed-query-var]
    (tt label qfn)
    (timed-query-inline label qfn)))

(defn- warn-if-saturated
  "Return `rows`, logging a :warn when `(count rows)` equals `limit`."
  [label limit rows]
  (when (and (seq rows) (= (count rows) limit))
    (log/warn "catchup bundle" label "returned exactly its limit of" limit
              "- results are TRUNCATED; entries beyond the cap never reach catchup"))
  rows)

;; =============================================================================
;; Per-type fork-joined queries
;; =============================================================================

(def ^:private global-pierce-filters
  "Store filters of the global-pierce fetches, keyed by branch: rows stored
   under project_id \"global\", and rows tagged `scope:global` wherever they
   were authored. Only `sf/scope-pierce-entries` kinds of them pierce."
  {:global        {:project-id "global"}
   :global-tagged {:tags ["scope:global"]}})

(defn- pierce-tasks
  "Fork-join tasks [k thunk []] for the `global-pierce-filters` fetches, each
   merged over OPTS."
  [store label opts]
  (for [[k filters] global-pierce-filters]
    [k (timed-query (str label "/" (name k))
                    #(with-resilience
                       (mem-proto/query-entries store (merge opts filters))))
     []]))

(defn query-scoped-entries
  "Query memory store entries filtered by project scope with hierarchy and
   scope-piercing. The hierarchy fetch and the `global-pierce-filters`
   fetches run concurrently via hive-weave fork-join with independent
   per-branch budgets so a slow cold-path query on one branch doesn't block
   the others. Branches that exceed their budget return [] (partial
   results). Every branch passes the HCR gate of `sf/hcr-view`."
  [entry-type tags project-id limit]
  (when (mem-proto/store-set?)
    (let [store (mem-proto/get-store)
          limit-val (or limit 20)
          in-project? (sf/in-project? project-id)
          hierarchy-ids (hier/compute-hierarchy-project-ids project-id)
          over-fetch-factor (if hierarchy-ids 3 4)
          tasks (cond-> [[:hierarchy
                          (timed-query "query-scoped/hierarchy"
                                       #(with-resilience
                                          (mem-proto/query-entries
                                            store
                                            {:type entry-type
                                             :project-ids hierarchy-ids
                                             :limit (min (* limit-val over-fetch-factor) 500)})))
                          []]]
                  in-project?
                  (into (pierce-tasks store "query-scoped" {:type entry-type :limit 100})))
          {:keys [hierarchy global global-tagged]} (apply wpar/fork-join
                                                          {:budget-ms scoped-branch-budget-ms}
                                                          tasks)
          view (sf/hcr-view project-id hierarchy-ids)
          scoped (sf/scope-filter-entries (or hierarchy []) (:scope-tags view) (:visible-ids view))
          scope-piercing (when in-project?
                           (sf/hcr-gate view (sf/scope-pierce-entries (concat global global-tagged)
                                                                      project-id)))
          scoped (sf/distinct-by :id (concat scoped scope-piercing))
          filtered (sf/filter-by-tags scoped tags)]
      (->> filtered
           sf/newest-first
           (take limit-val)))))

(defn query-expiring-entries
  "Query entries expiring within 7 days, scoped to project with scope-piercing
   (`global-pierce-filters`, fork-joined). The hierarchy and the pierced rows
   pass the HCR gate of `sf/hcr-view`."
  [project-id limit]
  (let [in-project? (sf/in-project? project-id)
        store (mem-proto/get-store)
        hierarchy-ids (hier/compute-hierarchy-project-ids project-id)
        entries ((timed-query "expiring/hierarchy"
                              #(with-resilience
                                 (mem-proto/query-entries store {:project-ids hierarchy-ids
                                                                 :limit 200}))))
        view (sf/hcr-view project-id hierarchy-ids)
        scoped (sf/scope-filter-entries (or entries []) (:scope-tags view) (:visible-ids view))
        scope-piercing (when in-project?
                         (let [{:keys [global global-tagged]}
                               (apply wpar/fork-join {:budget-ms scoped-branch-budget-ms}
                                      (pierce-tasks store "expiring" {:limit 100}))]
                           (sf/hcr-gate view (sf/scope-pierce-entries (concat global global-tagged)
                                                                      project-id))))
        scoped (sf/distinct-by :id (concat scoped scope-piercing))]
    (->> scoped
         (filter sf/entry-expiring-soon?)
         sf/newest-first
         (take (or limit 20)))))

(defn query-regular-conventions
  "Query conventions excluding axioms and priority ones."
  [project-id axiom-ids priority-ids]
  (let [all-conventions (query-scoped-entries bt/convention nil project-id 50)
        excluded-ids (set/union axiom-ids priority-ids)]
    (remove #(contains? excluded-ids (:id %)) all-conventions)))

;; =============================================================================
;; Collapsed catchup pull — ONE hierarchy + ONE global-pierce query, group by
;; :type in memory. Replaces 6-7 per-type Milvus RPCs to dodge type-filter
;; scalar-scan storms on the cold path.
;; =============================================================================

(def ^:private branch-order
  "Fork-join branch keys of `query-all-scoped`, in merge order."
  [:hierarchy :global :global-tagged :axioms-global :axiom-candidates-global
   :principles-priority :principles-global :sessions-fresh :recent-wraps-global])

(def ^:private always-gated
  "Branches scope-filtered at global scope too (see `sf/gate-branches`)."
  #{:hierarchy :sessions-fresh :recent-wraps-global})

(defn- scan-task
  "Fork-join task [k thunk fallback] for one metadata-projected store scan."
  [store k label opts]
  [k (timed-query label
                  #(with-resilience
                     (mem-proto/query-entries
                      store (assoc opts :output-fields hier/metadata-projection))))
   []])

(defn query-all-scoped
  "Single-shot scoped pull: chunked-parallel hierarchy query + the two
   global-pierce fetches (`global-pierce-filters`, in a project only) +
   axioms + axiom candidates + principles + priority principles + fresh
   sessions + recent wraps, each branch passed through the HCR gate of
   `sf/hcr-view` (`sf/gate-branches`), then merged + deduped. Returns

     {:by-type {\"axiom\" [...] \"decision\" [...] ...}
      :all     [<merged entries, newest-first>]}

   A pierced row reaches the result only when it is an
   `sf/scope-pierce-entries` kind (axiom, catchup-priority, session-summary).
   At global scope only `always-gated` branches are filtered.
   Metadata-only projection — `content` hydrated lazily by hydrate-buckets.
   Fork-joined with `scoped-branch-budget-ms`; a branch past budget
   contributes []. The `sessions-fresh` and `recent-wraps-global` branches
   carry `:created-after` = now minus `fresh-window-days`; every other branch
   is unbounded in time."
  [project-id]
  (when (mem-proto/store-set?)
    (let [store (mem-proto/get-store)
          in-project? (sf/in-project? project-id)
          hierarchy-ids (hier/compute-hierarchy-project-ids project-id)
          per-chunk-limit (max 50 (int (/ hier/bundle-hierarchy-limit
                                          (hier/chunk-count hierarchy-ids))))
          fresh-window-days 45
          fresh-window-cutoff (str (.minusDays (java.time.ZonedDateTime/now)
                                               fresh-window-days))
          fresh (fn [tag limit] {:type bt/note :tags [tag] :limit limit
                                 :created-after fresh-window-cutoff
                                 :order-by [:created :desc]})
          tasks (cond-> [[:hierarchy
                          (timed-query "hierarchy"
                                       #(hier/chunked-hierarchy-fetch store hierarchy-ids per-chunk-limit))
                          []]
                         (scan-task store :axioms-global "axioms-global"
                                    {:type bt/axiom :limit bundle-axioms-limit})
                         (scan-task store :axiom-candidates-global "axiom-candidates-global"
                                    {:type bt/axiom-candidate :limit bundle-axiom-candidates-limit})
                         (scan-task store :principles-global "principles-global"
                                    {:type bt/principle :limit bundle-principles-limit})
                         (scan-task store :principles-priority "principles-priority"
                                    {:type bt/principle :tags ["catchup-priority"]
                                     :limit bundle-priority-principles-limit})
                         (scan-task store :sessions-fresh "sessions-fresh"
                                    (fresh "session-summary" bundle-sessions-fresh-limit))
                         (scan-task store :recent-wraps-global "recent-wraps-global"
                                    (fresh "wrap-generated" bundle-recent-wraps-limit))]
                  in-project?
                  (into (for [[k filters] global-pierce-filters]
                          (scan-task store k (str "global-pierce/" (name k))
                                     (assoc filters :limit bundle-global-limit)))))
          {:keys [hierarchy global global-tagged axioms-global axiom-candidates-global
                  principles-global principles-priority
                  sessions-fresh recent-wraps-global]}
          (apply wpar/fork-join {:budget-ms scoped-branch-budget-ms} tasks)
          pierced (fn [label rows]
                    (when in-project?
                      (sf/scope-pierce-entries
                       (warn-if-saturated label bundle-global-limit (or rows []))
                       project-id)))
          gated (sf/gate-branches
                 (sf/hcr-view project-id hierarchy-ids)
                 always-gated
                 {:hierarchy               (or hierarchy [])
                  :global                  (pierced "global-pierce/global" global)
                  :global-tagged           (pierced "global-pierce/global-tagged" global-tagged)
                  :axioms-global           (warn-if-saturated "axioms-global" bundle-axioms-limit
                                                              (or axioms-global []))
                  :axiom-candidates-global (warn-if-saturated "axiom-candidates-global"
                                                              bundle-axiom-candidates-limit
                                                              (or axiom-candidates-global []))
                  :principles-global       (warn-if-saturated "principles-global" bundle-principles-limit
                                                              (or principles-global []))
                  :principles-priority     (warn-if-saturated "principles-priority"
                                                              bundle-priority-principles-limit
                                                              (or principles-priority []))
                  :sessions-fresh          (or sessions-fresh [])
                  :recent-wraps-global     (or recent-wraps-global [])})
          sorted (sf/newest-first (sf/distinct-by :id (mapcat gated branch-order)))]
      {:by-type (group-by #(some-> (:type %) name) sorted)
       :all     sorted})))

(defn- split-by-type
  "Phase-1 trim: pick per-type buckets from the grouped+sorted bundle."
  ([by-type all] (split-by-type by-type all {}))
  ([by-type all caps]
  (let [cap (fn [k fallback]
              (let [n (get caps k fallback)]
                (if (and (integer? n) (<= 0 n 1000)) n fallback)))
        take-type (fn [t limit] (vec (take limit (get by-type t []))))
        tagged    (fn [t tag limit]
                    (vec (take limit
                               (filter #(hydr/has-tag? % tag) (get by-type t [])))))
        has-priority? (some #(hydr/has-tag? % "catchup-priority") (get by-type bt/principle []))
        prio-principles (tagged bt/principle "catchup-priority" (cap :priority-principles 50))]
    ;; Axioms are inviolable: never capped by a lens or :catchup/caps (persona
    ;; profiles that zero a bucket must still deliver every axiom).
    {:axioms               (take-type bt/axiom 100)
     ;; Nominations awaiting human review. Small cap on purpose: this is a
     ;; to-do list for a person, not a context lane for an agent.
     :axiom-candidates     (take-type bt/axiom-candidate (cap :axiom-candidates 25))
     :priority-principles  (if has-priority?
                             prio-principles
                             (take-type bt/principle (cap :priority-principles 50)))
     :principles           (if has-priority?
                             (vec (take (cap :principles 50)
                                        (remove #(hydr/has-tag? % "catchup-priority")
                                                (get by-type bt/principle []))))
                             [])
     :priority-conventions (tagged    bt/convention "catchup-priority" (cap :priority-conventions 50))
     :sessions             (tagged    bt/note "session-summary" (cap :sessions 25))
     :recent-wraps         (tagged    bt/note "wrap-generated" (cap :recent-wraps 10))
     :decisions            (take-type bt/decision (cap :decisions 50))
     :conventions          (vec (take (cap :conventions 50)
                                      (remove #(hydr/has-tag? % "catchup-priority")
                                              (get by-type bt/convention []))))
     :snippets             (take-type bt/snippet (cap :snippets 20))
     :expiring             (vec (take (cap :expiring 20) (filter sf/entry-expiring-soon? all)))})))

(defn query-catchup-bundle
  "Fetch, select memory types, then hydrate. Personalized caps bypass the
   shared project cache so one caller's persona cannot affect another."
  ([project-id]
   (bc/cached-bundle project-id #(query-catchup-bundle project-id {})))
  ([project-id {:keys [caps]}]
   (let [{:keys [by-type all]} (or (query-all-scoped project-id)
                                  {:by-type {} :all []})]
     (-> (split-by-type by-type all caps)
         hydr/hydrate-buckets))))
