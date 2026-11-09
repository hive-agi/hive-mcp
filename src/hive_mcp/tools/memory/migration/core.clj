(ns hive-mcp.tools.memory.migration.core
  "Project-migration handlers: migrate memory entries between project-ids.
   Includes unified rename-project which orchestrates Chroma, KG edges, and
   .hive-project.edn updates."
  (:require [hive-mcp.tools.memory.core :refer [with-store]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.core :as core :refer [mcp-json mcp-error]]
            [hive-mcp.memory.temporal :as temporal]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.knowledge-graph.connection :as kg-conn]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.memory.migration.helpers :as helpers]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.dns.result :refer [rescue]]
            [hive-weave.parallel :as wpar]
            [taoensso.timbre :as log]
            [hive-mcp.vectordb.resilience :refer [with-resilience]]
            [malli.core :as m]
            [hive-spi.memory.ports :as ports]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn handle-migrate-project
  "Migrate memory from one project-id to another (Chroma + KG edges)."
  [{:keys [old-project-id new-project-id update-scopes]}]
  (log/info "mcp-memory-migrate-project:" old-project-id "->" new-project-id)
  (with-store
    (let [store (mem-proto/get-store)
          entries (with-resilience
                    (mem-proto/query-entries store {:project-id old-project-id :limit 10000}))
          migrated (atom 0)
          updated-scopes (atom 0)
          old-scope-tag (scope/make-scope-tag old-project-id)
          new-scope-tag (scope/make-scope-tag new-project-id)]
      (doseq [entry entries]
        (let [old-tags (vec (:tags entry))
              new-tags (if update-scopes
                         (scope/retag-scope old-tags old-scope-tag new-scope-tag)
                         old-tags)]
          (when (not= old-tags new-tags)
            (swap! updated-scopes inc))
          (with-resilience
            (mem-proto/update-entry! store (:id entry) {:project-id new-project-id
                                                        :tags new-tags}))
          ;; Temporal dual-write: record each migration
          (temporal/record-mutation-silent!
           {:entry-id       (:id entry)
            :op             :migrate
            :data           {:old-project-id old-project-id
                             :new-project-id new-project-id
                             :scopes-updated update-scopes}
            :previous-value {:project-id old-project-id}
            :project-id     new-project-id})
          (swap! migrated inc)))
      (let [kg-result (try
                        (kg-edges/migrate-edge-scopes! old-project-id new-project-id)
                        (catch Exception e
                          (log/warn "KG edge scope migration failed (non-blocking):"
                                    (.getMessage e))
                          {:migrated 0 :error (.getMessage e)}))]
        (mcp-json {:migrated @migrated
                   :updated-scopes @updated-scopes
                   :kg-edges-migrated (:migrated kg-result)
                   :kg-error (:error kg-result)
                   :old-project-id old-project-id
                   :new-project-id new-project-id})))))

(def ^:private migration-read-batch
  "Entry ids per batched backend read."
  512)

(def ^:private migration-write-concurrency
  "Concurrent scope rewrites against the backend."
  16)

(def ^:private migration-write-timeout-ms
  "Per-entry bound on one scope rewrite plus its audit record."
  120000)

(defn- resolve-migration-targets
  "Fetch TARGET-IDS as {:found [entry] :not-found [id]}, in the order asked for.

   Uses the store's batched read when it has one and falls back to per-id reads
   when it does not, so a store without IMemoryStoreBatch still migrates."
  [store target-ids]
  (let [by-id (if (mem-proto/batch-read-store? store)
                (into {}
                      (map (juxt :id identity))
                      (mapcat (fn [ids]
                                (with-resilience (mem-proto/get-entries store ids)))
                              (partition-all migration-read-batch target-ids)))
                (into {}
                      (keep (fn [eid]
                              (when-let [entry (with-resilience
                                                 (mem-proto/get-entry store eid))]
                                [eid entry])))
                      target-ids))]
    (reduce (fn [acc eid]
              (if-let [entry (get by-id eid)]
                (update acc :found conj entry)
                (update acc :not-found conj eid)))
            {:found [] :not-found []}
            target-ids)))

(defn- migrate-one-scope!
  "Rewrite ONE entry's project-id and scope tag in the store.

   A scope change leaves :content untouched, so it goes through the no-embed
   metadata write when the store has one. `update-entry!` re-embeds, and a
   migration is the case where that is pure waste: re-filing a document moves
   one entry per chunk.

   Total: answers {:migrated id} or {:error {:id id :error msg}}, never throws."
  [store entry {:keys [new-project-id old-scope-tag new-scope-tag]}]
  (try
    (let [new-tags (scope/retag-scope (:tags entry) old-scope-tag new-scope-tag)
          updates  {:project-id new-project-id :tags new-tags}]
      (with-resilience
        (if (mem-proto/metadata-write-store? store)
          (mem-proto/update-metadata! store (:id entry) updates)
          (mem-proto/update-entry! store (:id entry) updates)))
      {:migrated (:id entry)})
    (catch Exception e
      (log/warn "Failed to migrate entry" (:id entry) ":" (.getMessage e))
      {:error {:id (:id entry) :error (.getMessage e)}})))

(defn- apply-scope-migration!
  "Rewrite every entry's scope, bounded-parallel, one outcome per entry in order,
   then record the whole audit trail in ONE temporal transaction.

   An entry whose rewrite timed out comes back as an ERROR: bounded-pmap's
   fallback cannot name the item it stands for, so a nil outcome must be
   attributed here rather than dropped from both the migrated and error lists.

   Only entries that actually moved are audited."
  [store entries {:keys [old-project-id new-project-id] :as opts}]
  (let [raw      (wpar/bounded-pmap
                  {:concurrency migration-write-concurrency
                   :timeout-ms  migration-write-timeout-ms}
                  (fn [entry] (migrate-one-scope! store entry opts))
                  entries)
        outcomes (mapv (fn [entry outcome]
                         (or outcome
                             {:error {:id    (:id entry)
                                      :error (str "scope rewrite timed out after "
                                                  migration-write-timeout-ms "ms")}}))
                       entries raw)]
    (when-let [moved (seq (keep :migrated outcomes))]
      (rescue nil
        (temporal/record-mutations-batch!
         (mapv (fn [id]
                 ;; :migrate, not :migrate-scoped. record-mutation! asserts
                 ;; (contains? valid-ops op) and valid-ops declares :migrate,
                 ;; so every audit record this path wrote was an AssertionError
                 ;; swallowed by record-mutation-silent!.
                 {:entry-id       id
                  :op             :migrate
                  :data           {:old-project-id old-project-id
                                   :new-project-id new-project-id}
                  :previous-value {:project-id old-project-id}
                  :project-id     new-project-id})
               moved))))
    outcomes))

(def ^:private scan-page-size
  "Candidate entries per page of a tag-filter scan, and ids per migrated batch."
  500)

(def ^:private default-deadline-ms
  "Wall-clock budget of one migrate-scoped call. Past it the call stops BEFORE
   starting another page and answers what it did, :complete false, instead of
   holding the tool until an outer timeout drops the answer. A re-call with the
   same tag-filter resumes: moved entries no longer match the old project."
  60000)

(defn- exact-tag-ids
  "Ids of ROWS that carry TAG exactly (a store may match tags by substring)."
  [rows tag]
  (into [] (comp (filter (fn [e] (some #(= % tag) (:tags e)))) (map :id)) rows))

(defn- tag-filter-pages
  "Lazy seq of id pages: the entries of OLD-PROJECT-ID tagged TAG-FILTER.

   A store with IMemoryStorePage is walked by cursor, one bounded page per
   element, projected to id + tags, so no call holds more than PAGE-SIZE rows
   and nothing is read before the caller asks for the page. A store without it
   is read once, as before."
  [store old-project-id tag-filter page-size]
  (let [opts {:project-id old-project-id :tags [tag-filter]}]
    (if (satisfies? ports/IMemoryStorePage store)
      (letfn [(step [cursor]
                (lazy-seq
                 (let [{:keys [rows next-cursor]}
                       (with-resilience
                         (ports/query-page store (assoc opts
                                                        :limit page-size
                                                        :cursor cursor
                                                        :output-fields ["id" "tags"])))]
                   (cons (exact-tag-ids rows tag-filter)
                         (when next-cursor (step next-cursor))))))]
        (step nil))
      (lazy-seq
       (list (exact-tag-ids (with-resilience
                              (mem-proto/query-entries store (assoc opts :limit 10000)))
                            tag-filter))))))

(defn- migrate-in-pages!
  "Resolve and migrate the ids of ID-PAGES one page at a time.

   The deadline is checked before each page is REALIZED, so a slow scan stops
   between pages rather than in the middle of one. Returns
   {:found [id] :migrated [id] :skipped-ids [id] :errors [..] :pages n
    :complete? bool}; :complete? false means pages remained when the budget ran
   out. NOW-MS is injectable for tests."
  [store id-pages {:keys [dry-run deadline-ms now-ms] :as opts}]
  (let [now-ms   (or now-ms #(System/currentTimeMillis))
        deadline (+ (now-ms) (or deadline-ms default-deadline-ms))]
    (loop [pages id-pages
           acc   {:found [] :migrated [] :skipped-ids [] :errors [] :pages 0}]
      (if (>= (now-ms) deadline)
        (assoc acc :complete? (empty? pages))
        (if-let [s (seq pages)]
          (let [ids      (vec (first s))
                resolved (if (seq ids)
                           (resolve-migration-targets store ids)
                           {:found [] :not-found []})
                outcomes (if dry-run
                           []
                           (apply-scope-migration! store (:found resolved) opts))]
            (recur (rest s)
                   (-> acc
                       (update :found into (map :id) (:found resolved))
                       (update :migrated into (keep :migrated) outcomes)
                       (update :skipped-ids into (:not-found resolved))
                       (update :errors into (keep :error) outcomes)
                       (update :pages inc))))
          (assoc acc :complete? true))))))

(defn handle-migrate-scoped
  "Migrate specific memory entries by ID (or tag filter) from one project to another.
   Updates project-id and scope tags per-entry. Preserves KG edges — only updates
   edge scope for edges where BOTH endpoints are in the migrated set.

   Arguments:
     :entry-ids      - Vector of entry IDs to migrate (takes priority)
     :tag-filter     - Tag string to match entries (e.g. 'payment-flow'); used when entry-ids is empty
     :old-project-id - Source project-id (required for scope tag swap)
     :new-project-id - Target project-id (required)
     :dry-run        - Preview without modifying (default: false)
     :deadline-ms    - Wall-clock budget (default 60000); see `default-deadline-ms`

   Targets are resolved and moved in pages of `scan-page-size`: a tag-filter is
   walked by cursor on a store with IMemoryStorePage, never read as one
   10000-row scan. When the budget runs out between pages the answer is a
   PARTIAL report, :complete false, with the count done so far; calling again
   with the same arguments continues where it stopped.

   Resolving zero targets is an ERROR, not a successful no-op.

   Returns:
     {:migrated N :ids [...] :from old-project :to new-project :complete bool
      :pages N :kg-edges-migrated N :skipped-ids [...] :errors [...]}"
  [{:keys [entry-ids tag-filter old-project-id new-project-id dry-run deadline-ms]}]
  (cond
    (str/blank? new-project-id)
    (mcp-error "new-project-id is required")

    (str/blank? old-project-id)
    (mcp-error "old-project-id is required")

    (= old-project-id new-project-id)
    (mcp-error "old-project-id and new-project-id must be different")

    (and (empty? entry-ids) (str/blank? tag-filter))
    (mcp-error "Either entry-ids or tag-filter is required")

    :else
    (let [dry-run (boolean dry-run)]
      (log/info "migrate-scoped:" (if (seq entry-ids)
                                    (str (count entry-ids) " entry IDs")
                                    (str "tag-filter=" tag-filter))
                old-project-id "->" new-project-id
                (when dry-run "(dry-run)"))
      (with-store
        (let [store    (mem-proto/get-store)
              ;; The tag goes INTO the query: a store may refuse to enumerate a
              ;; whole project from :project-id alone, and post-filtering that
              ;; empty seq cannot tell an unsupported query from a real miss.
              id-pages (if (seq entry-ids)
                         (partition-all scan-page-size (vec entry-ids))
                         (tag-filter-pages store old-project-id tag-filter scan-page-size))
              run      (migrate-in-pages!
                        store id-pages
                        {:dry-run        dry-run
                         :deadline-ms    deadline-ms
                         :old-project-id old-project-id
                         :new-project-id new-project-id
                         :old-scope-tag  (scope/make-scope-tag old-project-id)
                         :new-scope-tag  (scope/make-scope-tag new-project-id)})]
          (if (and (:complete? run) (empty? (:found run)) (empty? (:skipped-ids run))
                   (empty? entry-ids))
            (mcp-error (str "tag-filter \"" tag-filter "\" matched no entries in project \""
                            old-project-id "\". Nothing was migrated. Tags match exactly — "
                            "no prefix or substring. Check what the tag actually selects with: "
                            "memory query :tags [\"" tag-filter "\"] :project-id \"" old-project-id "\""))
            (let [moved-ids      (if dry-run (:found run) (:migrated run))
                  migrated-set   (set moved-ids)
                  kg-edge-result (if (or dry-run (< (count migrated-set) 2))
                                   {:migrated 0}
                                   (try
                                     (let [edges     (kg-edges/find-edges-between migrated-set)
                                           to-update (filter #(= old-project-id (:kg-edge/scope %))
                                                             edges)
                                           tx-data   (vec (for [edge to-update
                                                                :let [eid (kg-conn/entid
                                                                           [:kg-edge/id (:kg-edge/id edge)])]
                                                                :when eid]
                                                            [:db/add eid :kg-edge/scope new-project-id]))]
                                       (when (seq tx-data)
                                         (kg-conn/transact! tx-data))
                                       {:migrated (count tx-data)})
                                     (catch Exception e
                                       (log/warn "KG edge scope migration failed (non-blocking):"
                                                 (.getMessage e))
                                       {:migrated 0 :error (.getMessage e)})))]
              (mcp-json (cond-> {:migrated          (count moved-ids)
                                 :ids               moved-ids
                                 :skipped-ids       (:skipped-ids run)
                                 :errors            (:errors run)
                                 :complete          (:complete? run)
                                 :pages             (:pages run)
                                 :kg-edges-migrated (:migrated kg-edge-result)
                                 :kg-error          (:error kg-edge-result)
                                 :dry-run           dry-run
                                 :from              old-project-id
                                 :to                new-project-id}
                          (not (:complete? run))
                          (assoc :reason (str "deadline reached after " (:pages run)
                                              " page(s); this is a partial count. Call again "
                                              "with the same arguments to continue.")))))))))))

;; =============================================================================
;; Contracts
;; =============================================================================
;;
;; Every migration handler answers one MCP text response (mcp-json report or
;; mcp-error refusal); with-store turns a store failure into mcp-error.

(m/=> handle-migrate-project [:=> [:cat [:maybe :map]] core/McpResponse])

(m/=> handle-rename-project [:=> [:cat [:maybe :map]] core/McpResponse])

(m/=> handle-migrate-scoped [:=> [:cat [:maybe :map]] core/McpResponse])

(defn handle-rename-project
  "Unified rename-project orchestrating Chroma, KG, .edn, and config migration."
  [{:keys [old-project-id new-project-id directory dry-run]}]
  (cond
    (str/blank? old-project-id)
    (mcp-error "old-project-id is required")

    (str/blank? new-project-id)
    (mcp-error "new-project-id is required")

    (= old-project-id new-project-id)
    (mcp-error "old-project-id and new-project-id must be different")

    :else
    (let [dry-run (boolean dry-run)]
      (log/info "rename-project:" old-project-id "->" new-project-id
                (when dry-run "(dry-run)") {:directory directory})

      (if dry-run
        (let [chroma-count (rescue 0
                                   (with-store
                                     (with-resilience
                                       (count (mem-proto/query-entries (mem-proto/get-store)
                                                                       {:project-id old-project-id
                                                                        :limit 10000})))))
              kg-count (rescue 0
                               (count (kg-edges/get-edges-by-scope old-project-id)))
              edn-config (when directory (helpers/read-hive-project-edn directory))
              current-aliases (or (:aliases edn-config) [])
              would-add-alias (not (some #{old-project-id} current-aliases))]
          (mcp-json {:status "dry-run"
                     :chroma {:would-migrate chroma-count}
                     :kg-edges {:would-migrate kg-count}
                     :edn {:exists (boolean edn-config)
                           :current-aliases current-aliases
                           :would-add-alias would-add-alias}
                     :old-project-id old-project-id
                     :new-project-id new-project-id
                     :directory directory}))

        (let [migrate-result (try
                               (let [raw (handle-migrate-project
                                          {:old-project-id old-project-id
                                           :new-project-id new-project-id
                                           :update-scopes true})
                                     text (or (get-in raw [:result 0 :text])
                                              (:text raw))
                                     parsed (json/read-str text :key-fn keyword)]
                                 parsed)
                               (catch Exception e
                                 (log/warn "Phase 1 (Chroma+KG migration) failed:"
                                           (.getMessage e))
                                 {:error (.getMessage e)
                                  :migrated 0
                                  :updated-scopes 0
                                  :kg-edges-migrated 0}))

              edn-result (if directory
                           (helpers/update-hive-project-edn! directory old-project-id new-project-id)
                           {:skipped "no directory provided"})

              config-registered
              (rescue false
                      (when-let [config (:config edn-result)]
                        (kg-scope/register-project-config! new-project-id config)
                        true))

              _cache-cleared (rescue nil
                                     (kg-scope/clear-config-cache!)
                                     (when-let [config (:config edn-result)]
                                       (kg-scope/register-project-config! new-project-id config)))]

          (mcp-json {:status "success"
                     :chroma {:migrated (:migrated migrate-result 0)
                              :updated-scopes (:updated-scopes migrate-result 0)}
                     :kg-edges {:migrated (:kg-edges-migrated migrate-result 0)
                                :error (:kg-error migrate-result)}
                     :edn (if (:success edn-result)
                            {:updated true
                             :aliases (get-in edn-result [:config :aliases])}
                            {:updated false
                             :reason (or (:error edn-result)
                                         (:skipped edn-result))})
                     :config-registered (boolean config-registered)
                     :old-project-id old-project-id
                     :new-project-id new-project-id
                     :directory directory}))))))
