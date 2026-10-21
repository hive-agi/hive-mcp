(ns hive-mcp.tools.memory.search
  "Semantic search handler for memory operations.

   Defensive timeout architecture (task 20260404125344-0365f8a8):
   Production hangs were observed from three failure modes —
   Milvus HTTP 'selector manager closed' (dead grpc client after OOM),
   Qdrant grpc 'INTERNAL: Panic!', and unreachable embedding providers.
   Each could leave a bare @future blocked indefinitely, starving the
   MCP handler pool and killing stdio.

   Defense in depth with structured surfacing:
   - Per-stage timeouts: vectordb (15s), post-filter/format (5s).
     Embedding is nested inside the store's search-similar; its budget
     is covered by the vectordb stage and surfaced via ex-classification
     (exceptions containing 'embed' in the message map to :stage :embed).
   - fork-join with a 30s collective budget runs the store and ingest
     queries concurrently, each with its own fallback. A failure on
     one side still yields a partial response.
   - Outer wpool/await! on memory-pool remains the hard ceiling.

   On timeout or throw, we return a Result err:
     {:error :memory/search-failed
      :stage :embed|:vectordb|:post
      :partial <whatever completed>
      :cause <ex-message>}

   Happy-path overhead: one safe-future-call allocation per stage
   (<1ms); the fork-join deadline check is O(tasks). Callers see no
   shape change on success — only a structured err body on failure."
  (:require [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.memory.domain :as domain]
            [hive-mcp.tools.core :as tcore :refer [coerce-int!]]
            [hive-mcp.tools.result-bridge :as rb]
            [hive-mcp.dns.result :as result :refer [rescue]]
            [hive-mcp.concurrency.pool :as pool]
            [hive-weave.pool :as wpool]
            [hive-weave.safe :as safe]
            [hive-weave.parallel :as parallel]
            [hive-mcp.context.request :as ctx]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.vectordb.resilience :refer [with-resilience]]
            [hive-mcp.memory.ingest-search :as ingest-search]
            [malli.core :as m]
            [hive-mcp.tools.memory.schema :as ms]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Timeout budgets
;; =============================================================================

(def ^:dynamic *timeout-budgets*
  "Timeout budgets for a memory search, in milliseconds.

     :total       collective ceiling for the whole search
     :embed       embedding round-trip, nested inside the vectordb stage;
                  used to classify a provider-side timeout as :stage :embed
     :vectordb    one vectordb query (store or ingest side); the two sides
                  run concurrently, so the wall clock is one budget, not two
     :post-filter merge/rerank/format

   Dynamic so a caller — a test, or an operator with a tighter SLA — can bind
   smaller budgets. The defaults accommodate a COLD embedder: first-call
   Ollama/Venice model load routinely takes 30-60s, which a 15s vectordb
   budget failed every time."
  {:total       90000
   :embed       10000
   :vectordb    60000
   :post-filter 5000})

(defn- budget
  "Milliseconds allowed for STAGE, per the current `*timeout-budgets*`."
  [stage]
  (get *timeout-budgets* stage))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- extract-title
  "Extract a one-line title from content or document text.
   Skips the 'Type:/Tags:/Content:' metadata prefix in document text."
  [document metadata]
  (let [content (get metadata :content)
        text    (or content document "")]
    (when (seq text)
      (let [;; Skip metadata prefix if present
            clean (if (str/starts-with? text "Type:")
                    (let [idx (str/index-of text "Content:")]
                      (if idx (subs text (+ idx 9)) text))
                    text)
            first-line (first (str/split-lines (str/trim clean)))]
        (when (seq first-line)
          (subs first-line 0 (min 120 (count first-line))))))))

(defn- format-search-result
  "Format a single search result — compact by default.
   Returns id, type, top 5 tags (excluding scope/agent), distance, one-line title."
  [{:keys [id document metadata distance]}]
  (let [raw-tags (when-let [t (get metadata :tags)]
                   (when (not= t "")
                     (str/split t #",")))
        clean-tags (filterv #(not (or (str/starts-with? % "agent:")
                                      (str/starts-with? % "scope:")
                                      (= % "carto")))
                            (or raw-tags []))]
    {:id       id
     :type     (get metadata :type)
     :tags     (vec (take 5 clean-tags))
     :distance distance
     :title    (extract-title document metadata)}))

(defn- content->text
  "Coerce a stored :content value into a string for downstream string ops.
   Kanban (and other structured) entries persist :content as a map (the
   Milvus read path JSON-decodes the round-tripped JSON string back into
   a map per try-parse-json contract); prefer the :title key when present,
   otherwise pr-str so format-search-result's string ops never see a map."
  [c]
  (cond
    (nil? c)    ""
    (string? c) c
    (map? c)    (or (:title c) (pr-str c))
    :else       (str c)))

(defn- store-entry->normalized
  "Map a store-protocol search entry (Milvus flat shape) into the
   common {:id :document :metadata :distance} shape consumed by
   format-search-result and merge-and-rerank."
  [entry]
  (let [tags-vec  (cond
                    (sequential? (:tags entry)) (vec (:tags entry))
                    (string? (:tags entry))    (vec (str/split (:tags entry) #","))
                    :else                      [])
        tags-str  (str/join "," tags-vec)
        tp        (:type entry)
        content   (content->text (:content entry))]
    {:id       (:id entry)
     :document content
     :distance (:distance entry)
     :metadata {:tags       tags-str
                :type       (cond-> tp (keyword? tp) name)
                :content    content
                :project-id (:project-id entry)}}))

(defn- entry-tag-set
  "The tags of a normalized search entry as a set of strings. The
   normalized shape carries them comma-joined under [:metadata :tags]; a
   sequential value is accepted too, so an ingest hit that kept its vector
   is read the same way."
  [entry]
  (let [t (get-in entry [:metadata :tags])]
    (cond
      (string? t)     (into #{} (remove str/blank?) (str/split t #","))
      (sequential? t) (into #{} (map str) t)
      :else           #{})))

(defn filter-required-tags
  "Keep the normalized search ENTRIES that carry EVERY tag in TAGS (AND
   semantics, the same as `memory query`). A nil or empty TAGS keeps every
   entry. Takes one map, {:entries [...] :tags [...]}.

   The vector stores rank by similarity only; the `tags` param of a search
   used to be accepted and then dropped, so a tag-scoped search answered
   untagged hits (kanban 20260713154842-68993ce0)."
  [{:keys [entries tags]}]
  (let [required (into #{} (comp (map str) (remove str/blank?)) tags)]
    (if (empty? required)
      (vec entries)
      (filterv #(every? (entry-tag-set %) required) entries))))

(def ^:private tag-overfetch
  "How many times the requested limit to fetch from the store when a tag
   filter is applied after ranking, so the filter has hits to keep."
  10)

(defn- record-co-access!
  "Fire-and-forget co-access recording for search results."
  [formatted project-id created-by]
  (when (>= (count formatted) 2)
    (future
      (rescue nil
              (kg-edges/record-co-access!
               (mapv :id formatted)
               {:scope project-id :created-by created-by})))))

;; =============================================================================
;; Stage classification
;; =============================================================================

(defn- classify-stage
  "Classify which pipeline stage a failure belongs to, by inspecting the
   exception message. Used to populate :stage in the structured err body.

   Heuristic:
   - `embed` / `embedding` substring → :embed (OOM / unreachable provider)
   - anything else coming from vectordb stage → :vectordb
   Default is :vectordb because the embedding call is nested inside
   mem-proto/search-similar and we cannot distinguish at the API boundary."
  [^String msg]
  (if (and msg (or (.contains msg "embed")
                   (.contains msg "Embed")
                   (.contains msg "embedding")))
    :embed
    :vectordb))

(defn- stage-err
  "Construct a structured :memory/search-failed err with stage classification."
  [stage cause-msg partial]
  (result/err :memory/search-failed
              (cond-> {:stage   stage
                       :cause   (or cause-msg "unknown")
                       :message (str "memory search failed at stage " stage
                                     (when cause-msg (str ": " cause-msg)))}
                (some? partial) (assoc :partial partial))))

(defn- unwrap-safe-future
  "Translate the Result returned by safe-future-call into a
   `{:stage :X :value V}` map, preserving the stage label. Returns
   `{:stage :X :err <message>}` on timeout or exception.

   Keeps the happy path allocation-free aside from the outer map."
  [stage r]
  (cond
    (result/ok? r)  {:stage stage :value (:ok r)}
    ;; safe-future returned :weave/timeout or :weave/exception
    :else           {:stage stage
                     :err   (or (:message r)
                                (:name r)
                                (str (:error r)))}))

;; =============================================================================
;; Stages — each wrapped with its own safe-future-call + timeout
;; =============================================================================

(defn- run-store-query
  "Vectordb stage A: the primary store search + one fallback retry
   without exclude-tags (for false-negative recovery).

   Wrapped in `safe-future-call` with the :vectordb budget. A dropped
   HTTP transport kicks the Milvus heal loop via with-resilience."
  [query limit-val type visible-project-ids effective-excludes]
  (safe/safe-future-call
   {:timeout-ms (budget :vectordb) :name "memory-search/store"}
   (fn []
     (let [first-pass (with-resilience
                        (mem-proto/search-similar
                         (mem-proto/get-store) query
                         {:limit        (* limit-val 2)
                          :type         type
                          :project-ids  visible-project-ids
                          :exclude-tags effective-excludes}))]
       (if (and (empty? first-pass) (seq effective-excludes))
         (do (log/warn "search fallback: retrying without exclude-tags for query:" query)
             (with-resilience
               (mem-proto/search-similar
                (mem-proto/get-store) query
                {:limit       (* limit-val 2)
                 :type        type
                 :project-ids visible-project-ids})))
         first-pass)))))

(defn- run-ingest-query
  "Vectordb stage B: ingest cross-collection search (carto snippets).
   Optional — when no ingest search-fn is registered, returns an empty
   vector. Wrapped in safe-future-call with the :vectordb budget."
  [query limit-val]
  (safe/safe-future-call
   {:timeout-ms (budget :vectordb) :name "memory-search/ingest"}
   (fn []
     (if-let [search-fn (ingest-search/resolve-ingest-search)]
       (rescue []
               (let [raw (search-fn query {:limit limit-val})]
                 (if (and (map? raw) (:ok raw))
                   (ingest-search/normalize-ingest-results (:ok raw))
                   [])))
       []))))

(defn- run-post-filter
  "Post-filter / aggregation stage: normalize, merge, rerank, format.
   Pure CPU but bounded by the :post-filter budget so a pathological
   reducer (e.g. million-row dedupe) cannot wedge the handler.

   REQUIRED-TAGS (may be empty) drops every hit, store or ingest, that does
   not carry all of them, before the merge cuts to LIMIT-VAL.
   Hits without IDs are dropped before merging and counted separately."
  [store-results ingest-results limit-val required-tags]
  (safe/safe-future-call
   {:timeout-ms (budget :post-filter) :name "memory-search/post"}
   (fn []
     (let [store-hits (mapv store-entry->normalized (or store-results []))
           ingest-hits (vec (or ingest-results []))
           dropped-null-hits (+ (count (remove (comp some? :id) store-hits))
                                (count (remove (comp some? :id) ingest-hits)))
           normalized-store (filter-required-tags
                             {:entries (filterv (comp some? :id) store-hits)
                              :tags    required-tags})
           ingest-kept (filter-required-tags
                         {:entries (filterv (comp some? :id) ingest-hits)
                          :tags required-tags})
           merged (ingest-search/merge-and-rerank normalized-store ingest-kept limit-val)]
       {:results (mapv format-search-result merged)
        :dropped-null-hits dropped-null-hits}))))

(def ^:private default-exclude-tags
  "Tags excluded from semantic search by default.
   Carto (L1/L2 codebase-mapping snippets) drowns out high-level knowledge,
   and so does an ingested corpus: one RFC series is hundreds of thousands of
   spec fragments competing with every axiom and decision in the store. The
   document-level entries an ingest also writes are tagged ingestion-document
   and stay visible — they are few and high-signal."
  ["carto" "ingestion-chunk"])

(defn resolve-exclude-tags
  "Pure: the exclusion set a search runs with. Takes one map,
   {:exclude-tags <caller value or nil> :default-excludes [...] :required-tags [...]}.

   A caller-supplied EXCLUDE-TAGS (even empty) is used as given. Without one the
   DEFAULT-EXCLUDES apply, minus any tag the caller REQUIRES: a search with
   tags=[\"ingestion-chunk\"] asked for those hits by name, and the default
   exclusion would otherwise make that search contradict itself and answer
   nothing (kanban 20260519145016-4561f6bc)."
  [{:keys [exclude-tags default-excludes required-tags]}]
  (if (some? exclude-tags)
    (vec exclude-tags)
    (let [required (set required-tags)]
      (vec (remove required default-excludes)))))

(defn search-payload
  "Pure: the ok payload of a semantic search.

   A default-on exclusion filter must be loud: every payload echoes the
   :excluded-tags the store query ran with, and :excludes-source says
   whether they are the built-in default (:default), the caller's own set
   (:caller) or nothing (:none). A caller can then tell an empty corpus
   from a filtered one, and knows to pass exclude_tags=[] to see the rest
   (kanban 20260728020157-1407f421).
   :dropped-null-hits counts store and ingest hits missing an ID before ranking."
  [{:keys [results query scope exclude-tags default-excludes dropped-null-hits]}]
  (let [excluded (vec exclude-tags)]
    {:results         results
     :count           (count results)
     :dropped-null-hits (or dropped-null-hits 0)
     :query           query
     :scope           scope
     :excluded-tags   excluded
     :excludes-source (cond
                        (empty? excluded)                    :none
                        (= (set excluded) (set default-excludes)) :default
                        :else                                :caller)}))

;; =============================================================================
;; Composite search-store* (fork-join across store + ingest)
;; =============================================================================

(defn- search-store*
  "Search the configured memory store + ingest collections concurrently
   with a fork-join collective budget (30s) and per-stage safe-future
   timeouts. Returns a Result.

   On total timeout or unrecoverable error, yields a structured err:
     {:error :memory/search-failed
      :stage :embed|:vectordb|:post
      :partial <partial results if any>
      :cause <ex-message>}

   Budget rationale:
   - store and ingest run in parallel under the collective :total budget.
   - each vectordb side has its own 15s cap, so one hung backend cannot
     eat the whole budget (the other side still races to complete).
   - post-filter gets 5s — bounded CPU, independent of network health.

   VISIBLE-PROJECT-IDS is the project filter pushed to the store (nil = none);
   `domain/scope->search-project-ids` computes it from the caller's scope.

   REQUIRED-TAGS: when non-empty, both sides over-fetch by `tag-overfetch`
   and the post stage keeps only hits carrying every tag."
  [query limit-val type project-id visible-project-ids exclude-tags required-tags]
  (let [effective-excludes (vec exclude-tags)
        fetch-limit (if (seq required-tags) (* limit-val tag-overfetch) limit-val)
        fj (parallel/fork-join
            {:budget-ms (budget :total)}
            [:store
             #(unwrap-safe-future :vectordb
                                  (run-store-query query fetch-limit type
                                                   visible-project-ids
                                                   effective-excludes))
             {:stage :vectordb :err "fork-join store timeout"}]
            [:ingest
             #(unwrap-safe-future :vectordb
                                  (run-ingest-query query fetch-limit))
             {:stage :vectordb :value []}])
        store-side  (:store fj)
        ingest-side (:ingest fj)]
    (cond
      ;; Store side failed or timed out — surface structured err.
      (contains? store-side :err)
      (let [msg (:err store-side)
            stage (classify-stage msg)]
        (log/warn (ex-info "memory search: store stage failed"
                           {:stage stage :cause msg :query query}))
        (stage-err stage msg nil))

      :else
      (let [store-results  (:value store-side)
            ingest-results (:value ingest-side [])
            post (run-post-filter store-results ingest-results limit-val required-tags)]
        (cond
          (result/ok? post)
          (let [{:keys [results dropped-null-hits]} (:ok post)]
            (record-co-access! results project-id "system:semantic-search")
            (result/ok (search-payload {:results          results
                                        :dropped-null-hits dropped-null-hits
                                        :query            query
                                        :scope            project-id
                                        :exclude-tags     effective-excludes
                                        :default-excludes default-exclude-tags})))

          :else
          (let [msg (or (:message post) (:name post) "post-filter failed")]
            (log/warn (ex-info "memory search: post stage failed"
                               {:stage :post :cause msg :query query}))
            (stage-err :post msg
                       {:store-count  (count store-results)
                        :ingest-count (count ingest-results)})))))))

;; =============================================================================
;; Dispatch
;; =============================================================================

(defn- search-semantic*
  "Pure search logic returning Result. Validates inputs and dispatches to the
   default IMemoryStore via search-store*.
   exclude_tags defaults to `default-exclude-tags` — pass a tag vector to
   replace that default with your own exclusion set. A tag named in `tags` is
   dropped from the default (see `resolve-exclude-tags`).
   tags, when given, is a required set: only hits carrying every tag are
   returned (AND, as in `memory query`).
   scope: an EXPLICIT project scope (anything but nil/\"all\"/\"global\") is a
   strict filter — only that project (+ its descendants) is returned, not its
   ancestors or global. nil auto-derives from directory and keeps the
   hierarchy; \"all\" lifts the filter.
   include_descendants defaults to true — pass false to restrict to current project."
  [{:keys [query limit type directory include_descendants scope exclude_tags tags]}]
  (let [directory (or directory (ctx/current-directory))
        include-descendants? (if (some? include_descendants)
                               (boolean include_descendants)
                               true)
        limit-val (coerce-int! limit :limit 10)
        required-tags (cond
                        (string? tags)     [tags]
                        (sequential? tags) (vec tags)
                        :else              [])
        store-ready? (and (mem-proto/store-set?)
                          (mem-proto/supports-semantic-search? (mem-proto/get-store)))]
    (log/info "mcp-memory-search-semantic:" query "type:" type "directory:" directory
              "scope:" scope "exclude_tags:" exclude_tags "tags:" required-tags
              "include_descendants:" include-descendants?)
    (if-not store-ready?
      (result/err :memory/store-not-configured
                  {:message (str "Semantic search not configured. "
                                 "Configure a memory store with an embedding provider.")})
      (let [project-id (scope/get-current-project-id directory)
            sf (domain/parse-scope scope project-id)
            [effective-pid _in-project?] (domain/scope->effective sf)
            visible-project-ids (domain/scope->search-project-ids
                                 sf {:include-descendants? include-descendants?
                                     :strict?              true})]
        (search-store* query limit-val type effective-pid visible-project-ids
                       (resolve-exclude-tags {:exclude-tags     exclude_tags
                                              :default-excludes default-exclude-tags
                                              :required-tags    required-tags})
                       required-tags)))))

(defn handle-search-semantic
  "Search project memory using semantic similarity (vector search).
   HCR Wave 4: Pass include_descendants=true to include child project memories.

   Runs on the dedicated memory-pool so concurrent search traffic is
   isolated from the shared io-pool. Inner stage timeouts (embed/vdb/post)
   provide per-boundary defensive budgets; this outer wpool/await! is
   the hard ceiling that ensures the MCP response cannot hang even if
   a stage wrapper itself is buggy."
  [params]
  (let [timeout-sentinel ::timeout
        raw (wpool/await!
             (pool/memory-pool)
             #(try (search-semantic* params)
                   (catch Throwable t {::caught t}))
             {:timeout-ms (budget :total)
              :fallback   timeout-sentinel
              :name       "memory-search"})]
    (cond
      (= raw timeout-sentinel)
      (rb/result->mcp
       (result/err :memory/search-failed
                   {:stage   :vectordb
                    :cause   (str "outer pool timeout after "
                                  (budget :total) "ms")
                    :message (str "memory search timed out after "
                                  (budget :total) "ms")}))

      (and (map? raw) (contains? raw ::caught))
      (rb/result->mcp
       (rb/try-result :memory/search-failed
                      (fn [] (throw (::caught raw)))))

      :else
      (rb/result->mcp raw))))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> filter-required-tags
      [:=> [:cat [:map [:entries {:optional true} [:maybe [:sequential ms/SearchHit]]]
                  [:tags {:optional true} [:maybe [:sequential ms/Tag]]]]]
       [:vector ms/SearchHit]])

(m/=> resolve-exclude-tags
      [:=> [:cat [:map [:exclude-tags {:optional true} [:maybe [:sequential ms/Tag]]]
                  [:default-excludes {:optional true} [:maybe [:sequential ms/Tag]]]
                  [:required-tags {:optional true} [:maybe [:sequential ms/Tag]]]]]
       [:vector :string]])

(m/=> search-payload
      [:=> [:cat [:map [:results {:optional true} [:maybe [:sequential {:gen/max 3} any?]]]

                  [:query {:optional true} [:maybe :string]]
                  [:scope {:optional true} [:maybe :string]]
                  [:exclude-tags {:optional true} [:maybe [:sequential ms/Tag]]]
                  [:default-excludes {:optional true} [:maybe [:sequential ms/Tag]]]
                  [:dropped-null-hits {:optional true} [:maybe nat-int?]]]]
       ms/SearchPayload])

(m/=> handle-search-semantic [:=> [:cat ms/McpParams] tcore/McpResponse])
