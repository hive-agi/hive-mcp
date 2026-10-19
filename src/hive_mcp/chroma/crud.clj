(ns hive-mcp.chroma.crud
  "Core CRUD operations for Chroma memory entries."
  (:require [hive-mcp.chroma.client :as chroma]
            [hive-mcp.chroma.connection :as conn]
            [hive-mcp.embeddings.active :as emb]
            [hive-mcp.chroma.gate :as gate]
            [hive-mcp.chroma.helpers :as h]
            [hive-mcp.embeddings.service :as embedding-service]
            [taoensso.timbre :as log] [hive-dsl.result :refer [rescue]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- evict-expired-async!
  "Async deletion of expired entry IDs from Chroma."
  [expired-ids]
  (when (seq expired-ids)
    (future
      (try
        (let [coll (conn/get-or-create-collection)]
          (gate/deref-write (chroma/delete coll :ids (vec expired-ids)))
          (log/info "Lazy eviction: deleted" (count expired-ids) "expired entries"))
        (catch Exception e
          (log/warn "Lazy eviction failed (non-blocking):" (.getMessage e)))))))

(defn- transient-key?
  "True for a key that names the transient embed-text field under any
   spelling: the keyword :embed-text in any namespace, the string
   \"embed-text\" or the symbol embed-text. Chroma metadata and JSON content
   write every key by its name, so each spelling lands in the same field."
  [k]
  (and (or (keyword? k) (string? k) (symbol? k))
       (= "embed-text" (name k))))

(defn strip-transient
  "X without its transient embed-text keys, at every depth: dropped from X
   when X is a map and from every map nested in X, through maps, sequences
   and sets. :embed-text is embedding input only; persisting it under any
   spelling would store the plaintext a sealing decorator encrypted. A value
   with nothing to drop comes back identical, and a collection holding no
   collection is not walked, so an embedding vector is never rebuilt."
  [x]
  (cond
    (map? x)
    (reduce-kv (fn [m k v]
                 (if (transient-key? k)
                   (dissoc m k)
                   (let [v' (strip-transient v)]
                     (if (identical? v v') m (assoc m k v')))))
               x x)

    (and (coll? x) (some coll? x))
    (cond
      (vector? x) (mapv strip-transient x)
      (set? x)    (into (empty x) (map strip-transient) x)
      :else       (apply list (map strip-transient x)))

    :else x))

(defn embed-input
  "The text to embed for ENTRY: its transient :embed-text when the caller gave
   a string one (a sealing decorator whose :content is ciphertext), else
   DOC-TEXT, the document built from the stored content. A given :embed-text
   is never traded for the document, not even a blank one: indexing the
   ciphertext is what the :embed-text capability exists to prevent."
  [entry doc-text]
  (let [embed-text (:embed-text entry)]
    (if (string? embed-text) embed-text doc-text)))

(def EntryIn
  "A memory entry to index. Only :content is read unconditionally; every
   other field is optional and dropped from metadata when nil."
  [:map
   [:id {:optional true} [:maybe :string]]
   [:content {:optional true} :any]
   [:type {:optional true} [:maybe [:or :string :keyword]]]
   [:tags {:optional true} [:maybe [:sequential :any]]]])

(def Entry
  "A memory entry read back from Chroma metadata (helpers/metadata->entry)."
  [:map [:id :any] [:type :any] [:content :any] [:tags [:maybe [:vector :string]]]])

(def EntryId
  "A stored entry id."
  :string)

(def StalenessOpts
  "Staleness fields, under the port names or their short spellings."
  [:map
   [:staleness-alpha {:optional true} :any]
   [:staleness-beta {:optional true} :any]
   [:staleness-source {:optional true} :any]
   [:staleness-depth {:optional true} :any]
   [:alpha {:optional true} :any]
   [:beta {:optional true} :any]
   [:source {:optional true} :any]
   [:depth {:optional true} :any]])

(def StalenessUpdates
  "Entry updates carrying only the port-named staleness fields."
  [:map {:closed true}
   [:staleness-alpha {:optional true} some?]
   [:staleness-beta {:optional true} some?]
   [:staleness-source {:optional true} some?]
   [:staleness-depth {:optional true} some?]])

(defn index-memory-entry!
  "Index a memory entry in Chroma, returns entry ID.

   A transient string :embed-text is embedded in place of the document built
   from :content (see `embed-input`) and is never persisted, under any
   spelling or depth (see `strip-transient`)."
  [entry]
  (emb/require-embedding!)
  (let [{:keys [id content type tags created updated duration expires
                content-hash access-count helpful-count unhelpful-count project-id
                kg-outgoing kg-incoming abstraction-level
                grounded-at grounded-from knowledge-gaps source-hash source-file
                staleness-alpha staleness-beta staleness-source staleness-depth]
         :as stored} (strip-transient entry)
        entry-id (or id (h/generate-id))
        now (h/iso-timestamp)
        doc-text (h/memory-to-document stored)
        embed-text (embed-input entry doc-text)
        ;; Size-aware resolution: oversized content auto-escalates to a
        ;; bigger-context provider (and its collection) — same resolution
        ;; drives collection + provider + embedding, so dimension stays
        ;; coherent. No-embed types (structurally-addressed blobs) skip the
        ;; provider call and store a zero placeholder vector.
        no-embed? (embedding-service/no-embed-type? type)
        resolved (rescue nil (embedding-service/resolve-provider-for-type+size type embed-text))
        _ (when (and resolved (not no-embed?))
            (embedding-service/validate-content-size! embed-text resolved))
        coll (if resolved
               (conn/get-or-create-named-collection
                 (:collection-name resolved) (:dimension resolved))
               (conn/get-or-create-collection))
        provider (if resolved (:provider resolved) (emb/get-embedding-provider))
        embed-1 (fn [p] (emb/embed-text p embed-text))
        embedding (if no-embed?
                    (vec (repeat (or (:dimension resolved) 768) 0.0))
                    ;; Embed via the resolved provider; on failure (e.g. Venice
                    ;; slow/unreachable) fail over once to a same-dimension
                    ;; sibling (venice<->openrouter qwen3, both 4096-d) so the
                    ;; write is never silently stored unvectorized.
                    (try
                      (embed-1 provider)
                      (catch Throwable e1
                        (if-let [alt (and resolved (embedding-service/sibling-failover resolved))]
                          (do (log/warn "Embed via" (:provider-key resolved)
                                        "failed for entry" entry-id "— failing over to"
                                        (:provider-key alt) "(" (.getMessage e1) ")")
                              (try (embed-1 (:provider alt))
                                   (catch Throwable e2
                                     (log/error "Embed failover also failed for entry"
                                                entry-id "providers" (:provider-key resolved)
                                                "/" (:provider-key alt) "—" (.getMessage e2))
                                     (throw e2))))
                          (do (log/error "Embed FAILED (no failover sibling) for entry"
                                         entry-id "via" (and resolved (:provider-key resolved))
                                         "—" (.getMessage e1))
                              (throw e1))))))
        provided {:type type :tags (h/join-tags tags) :content (h/serialize-content content)
                  :content-hash content-hash :created (or created now) :updated (or updated now)
                  :duration duration :expires expires :access-count access-count
                  :helpful-count helpful-count :unhelpful-count unhelpful-count
                  :project-id project-id
                  :kg-outgoing (h/join-tags kg-outgoing)
                  :kg-incoming (h/join-tags kg-incoming)
                  :abstraction-level abstraction-level
                  :grounded-at grounded-at
                  :grounded-from grounded-from
                  :knowledge-gaps (h/join-tags knowledge-gaps)
                  :source-hash source-hash
                  :source-file source-file
                  :staleness-alpha staleness-alpha
                  :staleness-beta staleness-beta
                  :staleness-source (when staleness-source (name staleness-source))
                  :staleness-depth staleness-depth}
        meta (merge h/metadata-defaults (into {} (remove (comp nil? val) provided)))]
    (gate/deref-write (chroma/add coll [{:id entry-id :embedding embedding :document doc-text :metadata meta}]
                                      :upsert? true))
    (log/debug "Indexed memory entry:" entry-id)
    entry-id))

(m/=> index-memory-entry! [:=> [:cat EntryIn] EntryId])

(defn index-memory-entries!
  "Index multiple memory entries in batch with full metadata.
   Uses the shared provider gate for the complete embed-batch call. Entries should have :id pre-set.
   A transient string :embed-text is embedded in place of the document and never persisted."
  [entries]
  (emb/require-embedding!)
  (let [coll (conn/get-or-create-collection)
        now  (h/iso-timestamp)
        stored (mapv strip-transient entries)
        docs (mapv h/memory-to-document stored)
        embeddings (emb/embed-batch (emb/get-embedding-provider) (mapv embed-input entries docs))
        records (mapv (fn [entry doc emb-vec]
                        (let [provided {:type          (:type entry)
                                        :tags          (h/join-tags (:tags entry))
                                        :content       (h/serialize-content (:content entry))
                                        :content-hash  (:content-hash entry)
                                        :created       (or (:created entry) now)
                                        :updated       (or (:updated entry) now)
                                        :duration      (:duration entry)
                                        :project-id    (:project-id entry)
                                        :access-count  (:access-count entry)
                                        :source-file   (:source-file entry)}
                              meta (merge h/metadata-defaults
                                          (into {} (remove (comp nil? val) provided)))]
                          {:id        (:id entry)
                           :embedding emb-vec
                           :document  doc
                           :metadata  meta}))
                      stored docs embeddings)]
    (gate/deref-write (chroma/add coll records :upsert? true))
    (log/info "Indexed" (count entries) "memory entries in batch")
    (mapv :id entries)))

(m/=> index-memory-entries! [:=> [:cat [:sequential EntryIn]] [:vector [:maybe EntryId]]])

(defn get-entry-by-id
  "Get a memory entry by ID from Chroma. Searches default + large collections."
  [id]
  (emb/require-embedding!)
  (let [coll (conn/get-or-create-collection)
        results (gate/deref-read (chroma/get coll :ids [id] :include #{:documents :metadatas}))
        entry (some-> (first results) h/metadata->entry)]
    (or entry
        ;; Try local 1024-d (qwen3-embedding:0.6b) collection
        (rescue nil (let [md-coll (conn/get-or-create-named-collection "hive-mcp-memory-1024d" 1024)
                          md-results (gate/deref-read (chroma/get md-coll :ids [id] :include #{:documents :metadatas}))]
                      (some-> (first md-results) h/metadata->entry)))
        ;; Try large-content collection if not found in default
        (rescue nil (let [lg-coll (conn/get-or-create-named-collection "hive-mcp-memory-4096d" 4096)
                lg-results (gate/deref-read (chroma/get lg-coll :ids [id] :include #{:documents :metadatas}))]
            (some-> (first lg-results) h/metadata->entry))))))

(m/=> get-entry-by-id [:=> [:cat :any] [:maybe Entry]])

(defn read-collections
  "Collections a read of TYPE must visit. With no embedder provider configured
   the type routing names no collection at all, yet writes still land in the
   default one, so an empty routing answer means the default collection, never
   nothing."
  [type]
  (let [colls (try (let [coll-names (embedding-service/type->collection-names type)]
                     (mapv (fn [cn]
                             (let [resolved (rescue nil (embedding-service/resolve-provider-for-type
                                                         (or type "note")))]
                               (if (and resolved (= cn (:collection-name resolved)))
                                 (conn/get-or-create-named-collection cn (:dimension resolved))
                                 (conn/get-or-create-collection))))
                           coll-names))
                   (catch Exception _ []))]
    (if (seq colls) (vec (distinct colls)) [(conn/get-or-create-collection)])))

(m/=> read-collections [:=> [:cat [:maybe [:or :string :keyword]]] [:vector {:min 1} chroma/Coll]])

(defn query-entries
  "Query memory entries from Chroma with filtering.
   :exclude-tags — seq of tags to exclude via $not_contains."
  [& {:keys [type project-id project-ids tags exclude-tags limit include-expired?]
      :or {limit 100 include-expired? false}}]
  (emb/require-embedding!)
  (let [colls (read-collections type)
        base-clause (cond-> {}
                      type (assoc :type type)
                      project-ids (assoc :project-id {:$in (vec project-ids)})
                      (and project-id (not project-ids)) (assoc :project-id project-id))
        tag-conditions (when (seq tags)
                         (mapv (fn [tag] {:tags {:$contains tag}}) tags))
        exclude-conditions (when (seq exclude-tags)
                             (mapv (fn [tag] {:tags {:$not_contains tag}}) exclude-tags))
        all-extra (concat tag-conditions exclude-conditions)
        where (cond
                (seq all-extra)
                (let [base-conditions (mapv (fn [[k v]] {k v}) base-clause)]
                  {:$and (into base-conditions all-extra)})
                (seq base-clause) base-clause
                :else nil)
        fetch-limit (if include-expired? limit (+ limit 50))
        ;; PERF: include only :metadatas — see docs/carto-tag-query-perf.md
        results (mapcat (fn [coll]
                          (try (gate/deref-read (chroma/get coll
                                                           :where where
                                                           :include #{:metadatas}
                                                           :limit fetch-limit))
                               (catch Exception _ [])))
                        colls)
        entries (map h/metadata->entry results)
        {expired true live false} (group-by #(boolean (h/expired? %)) entries)]
    (when-not include-expired?
      (evict-expired-async! (mapv :id expired)))
    (->> (if include-expired? entries live)
         (sort-by :created #(compare %2 %1))
         (take limit)
         vec)))

(m/=> query-entries [:=> [:cat [:* :any]] [:vector Entry]])

(defn query-grounded-from
  "Query entries grounded from a specific disc path."
  [disc-path]
  (emb/require-embedding!)
  (let [coll (conn/get-or-create-collection)
        results (gate/deref-read (chroma/get coll
                                            :where {:grounded-from disc-path}
                                            :include #{:documents :metadatas}))]
    (map h/metadata->entry results)))

(m/=> query-grounded-from [:=> [:cat :any] [:sequential Entry]])

(defn update-entry!
  "Update a memory entry in Chroma by ID. The merged entry is re-indexed, so
   it is always re-embedded: from a transient string :embed-text in UPDATES
   when given (also when UPDATES carries no :content), else from the
   document of the merged content. :embed-text is never persisted."
  [id updates]
  (emb/require-embedding!)
  (when-let [existing (get-entry-by-id id)]
    (let [merged (merge existing updates {:id id :updated (h/iso-timestamp)})
          _ (index-memory-entry! merged)]
      (get-entry-by-id id))))

(m/=> update-entry! [:=> [:cat :any [:maybe :map]] [:maybe Entry]])

(defn staleness-updates
  "Entry field updates for staleness OPTS. The port names the fields
   :staleness-alpha, :staleness-beta, :staleness-source and :staleness-depth;
   the short :alpha, :beta, :source and :depth spellings are still accepted,
   and lose to the port names when both are given."
  [opts]
  (into {}
        (keep (fn [[field short-k]]
                (let [v (if (some? (get opts field)) (get opts field) (get opts short-k))]
                  (when (some? v) [field v]))))
        [[:staleness-alpha :alpha]
         [:staleness-beta :beta]
         [:staleness-source :source]
         [:staleness-depth :depth]]))

(m/=> staleness-updates [:=> [:cat [:maybe StalenessOpts]] StalenessUpdates])

(defn update-staleness!
  "Update staleness fields for a Chroma entry."
  [entry-id opts]
  (emb/require-embedding!)
  (update-entry! entry-id (staleness-updates opts)))

(m/=> update-staleness! [:=> [:cat :any [:maybe StalenessOpts]] [:maybe Entry]])

(defn delete-entry!
  "Delete a memory entry from the Chroma index. Tries all collections."
  [id]
  (let [coll (conn/get-or-create-collection)]
    (gate/deref-write (chroma/delete coll :ids [id]))
    ;; Also try local 1024-d collection (qwen3-embedding:0.6b)
    (rescue nil (let [md-coll (conn/get-or-create-named-collection "hive-mcp-memory-1024d" 1024)]
                  (gate/deref-write (chroma/delete md-coll :ids [id]))))
    ;; Also try deleting from large collection (entry might be there)
    (rescue nil (let [lg-coll (conn/get-or-create-named-collection "hive-mcp-memory-4096d" 4096)]
        (gate/deref-write (chroma/delete lg-coll :ids [id]))))
    (log/debug "Deleted entry from Chroma:" id)
    id))

(m/=> delete-entry! [:=> [:cat :any] :any])

(defn find-duplicate
  "Find entry with matching content-hash in the given type.
   Pushes content-hash into the Chroma metadata filter so large stores
   (>1000 entries per type) still dedupe correctly instead of silently
   falling off the query window."
  [type content-hash & {:keys [project-id]}]
  (emb/require-embedding!)
  (let [colls (read-collections type)
        base-clause (cond-> {:content-hash content-hash}
                      type       (assoc :type type)
                      project-id (assoc :project-id project-id))
        where (if (< 1 (count base-clause))
                {:$and (mapv (fn [[k v]] {k v}) base-clause)}
                base-clause)
        results (mapcat (fn [coll]
                          (try (gate/deref-read (chroma/get coll
                                                            :where where
                                                            :include #{:metadatas}
                                                            :limit 1))
                               (catch Exception _ [])))
                        colls)]
    (first (map h/metadata->entry results))))

(m/=> find-duplicate [:=> [:cat [:maybe [:or :string :keyword]] [:maybe :string] [:* :any]] [:maybe Entry]])

(defn collection-stats
  "Get statistics about the Chroma collection."
  []
  (try
    (let [coll (conn/get-or-create-collection)
          all-entries (gate/deref-read (chroma/get coll :include [:metadatas]))]
      {:count (count all-entries)
       :types (frequencies (map #(get-in % [:metadata :type]) all-entries))})
    (catch Exception e
      {:error (str e)})))

(m/=> collection-stats [:=> [:cat] [:or
                                    [:map [:count [:int {:min 0}]] [:types [:map-of :any pos-int?]]]
                                    [:map [:error :string]]]])

(defn delete-all-entries!
  "Remove every entry from every collection a read could visit. Returns the
   number of entries removed. With no embedding provider configured no entry
   can have been written, so there is nothing to remove and this answers 0
   rather than throwing."
  []
  (if-not (emb/embedding-configured?)
    0
    (reduce (fn [n coll]
              (let [ids (mapv :id (gate/deref-read (chroma/get coll :include #{:metadatas})))]
                (when (seq ids)
                  (gate/deref-write (chroma/delete coll :ids ids)))
                (+ n (count ids))))
            0
            (read-collections nil))))

<<<<<<< HEAD
(def ^:private scan-page-size 1000)

(def ^:private scan-attempts 3)

(def ^:private legacy-collections
  "Collections get-entry-by-id and delete-entry! also visit by name, written
   by earlier embedder routings even when no configured provider names them."
  ["hive-mcp-memory-1024d" "hive-mcp-memory-4096d"])

(defn- scan-collections
  "Every collection an entry can live in: those a read visits plus the legacy
   named ones that already exist. A scan never creates a collection."
  []
  (let [existing (keep (fn [n] (rescue nil (gate/deref-read (chroma/get-collection n))))
                       legacy-collections)]
    (vals (into {} (map (juxt :name identity)) (concat (read-collections nil) existing)))))

(defn- collection-rows
  "Every row of COLL, paged by offset until a short page. Throws when a page
   cannot be read: a scan is complete or it fails."
  [coll]
  (loop [offset 0 acc []]
    (let [page (vec (gate/deref-read (chroma/get coll :include #{:metadatas}
                                                 :offset offset :limit scan-page-size)))
          acc  (into acc page)]
      (if (< (count page) scan-page-size)
        acc
        (recur (+ offset scan-page-size) acc)))))

(defn- scan-pass [include-expired?]
  (->> (scan-collections)
       (mapcat collection-rows)
       (remove #(and (not include-expired?) (h/expired? (:metadata %))))
       (keep :id)
       distinct
       vec))

(defn scan-ids
  "Every entry id across every collection an entry can live in, each once.
   Opts: :include-expired? (default false). Offset paging over a collection
   that changes under it can skip a row, so two passes must agree on the id
   set; after `scan-attempts` disagreeing pairs this throws rather than answer
   a list that may be short. With no embedding provider configured nothing can
   have been written, so the answer is empty."
  [{:keys [include-expired?]}]
  (if-not (emb/embedding-configured?)
    []
    (loop [n 1]
      (let [a (scan-pass include-expired?)
            b (scan-pass include-expired?)]
        (cond
          (= (set a) (set b)) b
          (< n scan-attempts) (recur (inc n))
          :else (throw (ex-info "chroma scan-ids: consecutive passes never agreed"
                                {:attempts n :first (count a) :second (count b)})))))))
=======
(m/=> delete-all-entries! [:=> [:cat] [:int {:min 0}]])
>>>>>>> origin/staging
