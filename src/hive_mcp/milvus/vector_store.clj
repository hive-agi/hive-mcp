(ns hive-mcp.milvus.vector-store
  "Milvus as an `IVectorCollectionStore`.

   This is the ADAPTER, and the one place allowed to know both sides: the port
   in `hive-mcp.protocols.vector` and the vendor transport in `milvus-clj.api`.
   It lives under `milvus.*` on purpose, because a provider is where naming a
   vendor is correct. Callers depend on the port.

   The vendor is NOT declared in hive-mcp's deps.edn (\"NO CONCRETE BACKEND
   BELONGS HERE\"): every `milvus-clj.api` call is late-bound through
   `requiring-resolve`, so a deployment without the jar fails at the call
   with a reason instead of failing to compile. The transport itself must
   already be dialed — the `hive.milvus` addon owns the connection and
   milvus-clj keeps it in its singleton; this adapter never dials.

   Collection naming: Milvus has no free-form metadata on a collection, so
   the embedding dimension is encoded in the name, `<name>-<dim>d`, the same
   convention hive-milvus's memory collections already use
   (`dim-from-chroma-name`). `-get-collection` recovers the dimension from
   the suffix via `list-collections`, which is what lets the port's
   dimension-change detection survive a JVM restart.

   Schema (per collection, generic): `id` (varchar PK), `embedding`
   (float-vector, dim), `document` (varchar), `metadata` (varchar, JSON
   string). Metadata is stored as a JSON STRING rather than a JSON column so
   the insert path needs no vendor-specific value coercion; `:where` filters
   are therefore applied as a post-filter over fetched rows, sized with an
   overfetch factor.

   Distances: the collections this adapter creates are indexed HNSW/COSINE,
   and Milvus reports COSINE proximity as a SIMILARITY under the `:distance`
   key. The port promises an ASCENDING distance, so `1 - similarity` is
   applied once, here — the same conversion hive-milvus's search pipeline
   performs at its only layer that knows the metric."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [hive-mcp.protocols.vector :as vp]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private read-timeout-ms 15000)
(def ^:private write-timeout-ms 30000)

(def ^:private overfetch-factor
  "How many nearest rows to fetch before applying a `:where` post-filter.
   Pure equality filters (the only kind presets issues) are cheap to apply
   client-side; 3x keeps precision on small collections without a vendor
   expression language for JSON-in-varchar."
  3)

(def ^:private default-dimension 768)

;; Sym -> resolved milvus-clj.api var, memoized.
(defonce ^:private api-cache (atom {}))

(defn- api*
  "Late-bound milvus-clj fn. A bare keyword resolves in `milvus-clj.api`; a
   fully-qualified symbol resolves as-is (schema helpers live in
   `milvus-clj.schema`). Memoized; a failed resolve memoizes nil."
  [sym]
  (or (get @api-cache sym)
      (let [q (if (qualified-symbol? sym) sym (symbol "milvus-clj.api" (name sym)))
            v (requiring-resolve q)]
        (swap! api-cache assoc sym v)
        v)))

(defn- await!
  "milvus-clj.api returns futures; bound-deref them and translate a timeout
   or IO failure into a reason-carrying throw."
  [future* timeout-ms op]
  (let [result (deref future* timeout-ms ::timeout)]
    (when (= result ::timeout)
      (throw (ex-info (str "milvus " op " timed out")
                      {:type ::vendor-timeout :op op :timeout-ms timeout-ms})))
    result))

(defn- call!
  "await! + rescue: vendor throws are rethrown as ex-info naming the op."
  [op timeout-ms & args]
  (try
    (await! (apply (api* op) args) timeout-ms op)
    (catch clojure.lang.ExceptionInfo e
      (throw e))
    (catch Throwable t
      (throw (ex-info (str "milvus " op " failed: " (.getMessage t))
                      {:type ::vendor-failure :op op}
                      t)))))

;;; ============================================================
;;; Dimension-aware naming
;;; ============================================================

(def ^:private dim-suffix-re #"_(\d+)d$")

(defn- milvus-name
  "The vendor-side name for a port collection of DIM. Milvus collection
   names admit only letters, numbers and underscores (same rule
   hive-milvus.collections enforces), so hyphens become underscores."
  [name dim]
  (str (str/replace name "-" "_") "_" dim "d"))

(defn- strip-dim
  "Normalize a name: hyphens -> underscores, drop any dimension suffix."
  [name]
  (-> (str/replace (str name) dim-suffix-re "")
      (str/replace "-" "_")))

;; name (suffix-stripped) -> dimension of the vendor collection this process
;; created. Needed because the collection dimension must survive to
;; `-get-collection`'s handle, and the HTTP transport does not implement
;; `-list-collections`, so suffix discovery by enumeration is unavailable.
;; Miss after a restart simply re-creates the collection on the next
;; `-create-collection` (which probes `has-collection` and reuses it).
(defonce ^:private dim-registry (atom {}))

(defn- resolve-existing
  "The vendor collection belonging to port collection NAME, per the registry,
   verified with a `has-collection` probe.
   Returns {:milvus-name ... :dimension dim} | nil."
  [name]
  (let [bare   (strip-dim name)
        dim    (get @dim-registry bare)
        m-name (milvus-name bare dim)]
    (when (and dim (call! :has-collection read-timeout-ms m-name))
      {:milvus-name m-name :dimension dim})))

(defn- handle
  "The port handle for a resolved collection."
  [name {:keys [milvus-name' dimension]}]
  {:collection-name name
   :metadata        {:dimension dimension
                     :milvus-name (or milvus-name'
                                      (milvus-name (strip-dim name) dimension))}})

;;; ============================================================
;;; Schema
;;; ============================================================

(defn- generic-fields
  "Field definitions for a port collection of DIM. The transport builds the
   schema itself from these (do-create-collection calls
   milvus-clj.schema/collection-schema on the raw vector)."
  [dim]
  [{:name "id" :type :varchar :primary? true :max-length 256}
   {:name "embedding" :type :float-vector :dimension dim}
   {:name "document" :type :varchar :max-length 65535}
   {:name "metadata" :type :varchar :max-length 65535}])

(defn- ensure-collection!
  "Create (if absent) + index + load the vendor collection for NAME at DIM.
   The index mirrors hive-milvus's default-memory-index (HNSW/COSINE) so the
   similarity->distance conversion above cannot drift from the metric."
  [name dim]
  (let [bare   (strip-dim name)
        m-name (milvus-name bare dim)
        has?   (call! :has-collection read-timeout-ms m-name)]
    (when-not has?
      (call! :create-collection write-timeout-ms m-name
             {:schema        (generic-fields dim)
              :index         {:field-name "embedding"
                              :index-type :hnsw
                              :metric-type :cosine
                              :extra-params {:M 16 :efConstruction 256}}
              :description   "hive-mcp vector-collection port"})
      (log/info "Created Milvus vector-collection:" m-name "dim:" dim))
    (call! :load-collection write-timeout-ms m-name)
    (swap! dim-registry assoc bare dim)
    m-name))

;;; ============================================================
;;; Record shaping
;;; ============================================================

(defn- record->row
  "Port record -> Milvus row. Metadata serializes to a JSON string."
  [{:keys [id embedding document metadata]}]
  {:id       (str id)
   :embedding (vec embedding)
   :document  (or document "")
   :metadata  (json/write-str (or metadata {}))})

(defn- row->record
  "Milvus row -> port record. Keeps every field the vendor returned (search
   rows carry :distance — dropping it would corrupt the port's ordering
   contract); metadata parses back from JSON."
  [row]
  (when row
    (update row :metadata (fn [m]
                            (let [parsed (if (string? m)
                                           (try (json/read-str m :key-fn keyword)
                                                (catch Throwable _ {}))
                                           (walk/keywordize-keys (or m {})))]
                              (or parsed {}))))))

(defn- where->pred
  "Port `:where` (flat equality map) -> predicate over parsed metadata."
  [where]
  (when (seq where)
    (fn [metadata]
      (every? (fn [[k v]] (= (get metadata k) v)) where))))

(defn- output-fields
  []
  ["id" "document" "metadata"])

;;; ============================================================
;;; The adapter record
;;; ============================================================

(defrecord MilvusVectorCollectionStore []
  vp/IVectorCollectionStore
  (-configure [this _opts] this)

  (-get-collection [_this name]
    (when-let [existing (resolve-existing name)]
      (handle name {:dimension (:dimension existing)
                    :milvus-name' (:milvus-name existing)})))

  (-create-collection [_this name opts]
    (let [dim (or (get-in opts [:metadata :dimension]) default-dimension)
          get-or-create? (:get-or-create? opts)
          existing (resolve-existing name)]
      (cond
        (and existing get-or-create?
             (= (:dimension existing) dim))
        (handle name existing)

        ;; exists at the wrong dim: drop + recreate, mirroring the chroma
        ;; adapter's contract that -create-collection is authoritative.
        existing
        (do (call! :drop-collection write-timeout-ms (:milvus-name existing))
            (let [m-name (ensure-collection! name dim)]
              (handle name {:dimension dim :milvus-name' m-name})))

        :else
        (let [m-name (ensure-collection! name dim)]
          (handle name {:dimension dim :milvus-name' m-name})))))

  (-delete-collection [_this coll]
    (let [name (if (map? coll) (:collection-name coll) coll)
          m-name (or (and (map? coll) (get-in coll [:metadata :milvus-name]))
                     (:milvus-name (resolve-existing name)))]
      (when m-name
        (call! :drop-collection write-timeout-ms m-name))
      nil))

  (-add [_this coll records opts]
    (let [m-name (get-in coll [:metadata :milvus-name])
          rows (mapv record->row records)]
      (call! :add write-timeout-ms m-name rows :upsert? (boolean (:upsert? opts)))
      nil))

  (-get [_this coll {:keys [ids where limit]}]
    (let [m-name (get-in coll [:metadata :milvus-name])
          pred (where->pred where)]
      (cond->> (if (seq ids)
                 (call! :get read-timeout-ms m-name ids
                        :include (output-fields))
                 (call! :query-scalar read-timeout-ms m-name
                        {:filter "id != \"\""
                         :output-fields (output-fields)
                         :limit (or limit 100)}))
        true   (mapv row->record)
        pred   (filterv #(pred (:metadata %)))
        limit  (take limit))))

  (-query [_this coll embedding {:keys [n-results where]}]
    (let [m-name (get-in coll [:metadata :milvus-name])
          pred (where->pred where)
          limit (or n-results 10)
          rows (call! :query read-timeout-ms m-name
                      {:vector (vec embedding)
                       :limit (if pred (* limit overfetch-factor) limit)
                       :metric-type :cosine
                       :output-fields (output-fields)})]
      (cond->> (mapv (fn [row]
                       (-> (row->record row)
                           (update :distance #(max 0.0 (- 1.0 (double (or % 2.0)))))))
                     rows)
        pred (filterv #(pred (:metadata %)))
        true (take limit))))

  (-delete [_this coll {:keys [ids]}]
    (when (seq ids)
      (let [m-name (get-in coll [:metadata :milvus-name])]
        (call! :delete write-timeout-ms m-name (vec ids))))
    nil)

  (-update [_this coll records]
    (let [m-name (get-in coll [:metadata :milvus-name])
          rows (mapv record->row records)]
      (call! :add write-timeout-ms m-name rows :upsert? true)
      nil)))

(defn milvus-vector-store
  "Create the Milvus-backed `IVectorCollectionStore`. No connection is made
   here — calls fail loudly until the `hive.milvus` addon has dialed."
  []
  (->MilvusVectorCollectionStore))
