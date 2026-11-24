(ns hive-mcp.agent.transcript-source
  "Read port over the places agent transcripts are persisted.

   Contract: every fn of `TranscriptSource` returns a Result.
     list-transcripts -> Result<[{:agent-id :source ...}]>
     read-entries     -> Result<[entry]> ordered by turn, or
                         (err :transcript/not-found {:agent-id})

   Implementations: `JsonlSource` (legacy /tmp/hive-transcripts/<id>.jsonl),
   `DatalevinSource` (hive-agent layout <root>/<project-id>/<agent-id>, the
   store headless hive-agent lings write), and `composite` over several."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-dsl.result :as r]
            [malli.core :as m]
            [hive-mcp.resilience.swallow :as swallow]))

(defprotocol TranscriptSource
  (list-transcripts [this] "Result<[{:agent-id :source ...}]>.")
  (read-entries [this agent-id] "Result<[entry]> ordered by turn."))

(defn entry-turn
  "Turn of a JSONL (:turn) or hive-agent (:transcript/turn) entry, 0 when absent."
  [entry]
  (or (:turn entry) (:transcript/turn entry) 0))

(defn- not-found [agent-id source]
  (r/err :transcript/not-found
         {:agent-id agent-id :source source
          :message (str "No transcript for agent " agent-id)}))

;; =============================================================================
;; JSONL
;; =============================================================================

(defrecord JsonlSource [dir]
  TranscriptSource
  (list-transcripts [_]
    (r/try-effect
     (let [d (io/file dir)]
       (->> (when (.isDirectory d) (.listFiles d))
            (filter #(str/ends-with? (.getName ^java.io.File %) ".jsonl"))
            (mapv (fn [^java.io.File f]
                    {:agent-id (str/replace (.getName f) #"\.jsonl$" "")
                     :source   :jsonl
                     :size-kb  (/ (.length f) 1024.0)
                     :modified (.lastModified f)}))))))
  (read-entries [_ agent-id]
    (let [f (io/file dir (str agent-id ".jsonl"))]
      (if-not (.isFile f)
        (not-found agent-id :jsonl)
        (r/try-effect
         (->> (str/split-lines (slurp f))
              (remove str/blank?)
              (mapv #(json/read-str % :key-fn keyword))))))))

;; =============================================================================
;; Datalevin (hive-agent layout)
;; =============================================================================

(def unopened-errors
  "Errors of a run whose store was deliberately NOT opened: a writer of this
   process holds it (busy), or nothing can establish that none does (unknown)."
  #{:transcript/store-busy :transcript/store-unknown})

(defn unopened?
  "True when `res` is an err for a store that was not opened."
  [res]
  (contains? unopened-errors (when (map? res) (:error res))))

(m/=> unopened? [:=> [:cat :any] :boolean])

(def ^:private skipped-project-dirs
  "Segments under the root that are not <project-id> partitions."
  #{"datahike" "tmp"})

(defn- store-dir? [^java.io.File d]
  (and (.isDirectory d) (.isFile (io/file d "data.mdb"))))

(defn- store-dirs
  "[{:agent-id :project-id :dir}] for every store under `root`."
  [root]
  (let [r (io/file root)]
    (for [^java.io.File p (when (.isDirectory r) (.listFiles r))
          :when (and (.isDirectory p) (not (skipped-project-dirs (.getName p))))
          ^java.io.File a (.listFiles p)
          :when (store-dir? a)]
      {:agent-id (.getName a) :project-id (.getName p) :dir a})))

(defrecord DatalevinSource [root read-dir]
  ;; read-dir: (fn [dir agent-id]) -> Result<[entry]> for one store dir.
  TranscriptSource
  (list-transcripts [_]
    (r/try-effect
     (mapv (fn [{:keys [agent-id project-id ^java.io.File dir]}]
             {:agent-id   agent-id
              :project-id project-id
              :source     :datalevin
              :size-kb    (/ (reduce + 0 (map #(.length ^java.io.File %)
                                              (filter #(.isFile ^java.io.File %) (file-seq dir))))
                             1024.0)
              :modified   (.lastModified (io/file dir "data.mdb"))})
           (store-dirs root))))
  (read-entries [_ agent-id]
    (let [dirs (filter #(= agent-id (:agent-id %)) (store-dirs root))]
      (if (empty? dirs)
        (not-found agent-id :datalevin)
        ;; A store that was not opened answers its own err, never a throw, so
        ;; callers can show the run as busy/unknown instead of dropping it.
        (let [read (r/try-effect (mapv (fn [{:keys [dir]}] [dir (read-dir (str dir) agent-id)]) dirs))]
          (if-not (r/ok? read)
            read
            (or (some (fn [[_ res]] (when (unopened? res) res)) (:ok read))
                (r/try-effect
                 (->> (:ok read)
                      (mapcat (fn [[dir res]]
                                (if (r/ok? res)
                                  (:ok res)
                                  (throw (ex-info (str "Datalevin read failed: " (pr-str res))
                                                  {:dir (str dir) :result res})))))
                      (sort-by entry-turn)
                      vec)))))))))

(defn- canonical [path]
  (.getCanonicalPath (io/file (str path))))

(defn- legacy-holds-fn
  "(fn [dir]) -> holders of `dir` in a pre-registry hive-agent's per-path
   handle counts, matched by CANONICAL path (they are keyed by the writer's
   own spelling). nil when that hive-agent exposes no counts."
  []
  (when-let [counts (swallow/attempt
                     (some-> (requiring-resolve 'hive-agent.loop.transcript.datalevin-store/handle-counts) deref))]
    (when (instance? clojure.lang.IDeref counts)
      (fn [dir]
        (let [k (canonical dir)]
          (reduce-kv (fn [n path c] (if (= k (canonical path)) (+ n (long c)) n))
                     0 @counts))))))

(defn- registry-holds-fn
  "(fn [dir]) -> holders of `dir` in hive-agent's shared connection registry
   (keyed by canonical dir), or nil when the loaded hive-agent has none:
   `acquire-conn!` is how a registry-era hive-agent is recognised."
  []
  (when (swallow/attempt (requiring-resolve 'hive-agent.loop.transcript.datalevin-store/acquire-conn!))
    (when-let [count-fn (swallow/attempt (requiring-resolve 'hive-agent.loop.transcript.datalevin-store/open-handle-count))]
      (fn [dir] (long (count-fn (str dir)))))))

(defn- live-writers-fn
  "(fn [agent-id]) -> the swarm registry rows of this process whose agent
   writes the store of `agent-id` (hive-agent names a store dir
   <root>/<project-id>/<agent-id>, so the dir's last segment is its writer),
   or nil when the swarm registry cannot be read. Every row the registry
   still returns counts as a writer, finished or not: a false 'busy' only
   hides a row, a false 'free' crashes the server."
  []
  (fn [agent-id]
    (when-let [slaves (swallow/attempt
                       (vec ((requiring-resolve 'hive-mcp.swarm.datascript.queries/get-all-slaves))))]
      (filterv #(= agent-id (:slave/id %)) slaves))))

(defn store-decision
  "Whether the store at `dir` may be opened, from the probes in `ops`:
     :registry-holds (fn [dir]) -> holders in hive-agent's shared registry, or absent
     :legacy-holds   (fn [dir]) -> holders in a pre-registry hive-agent, or absent
     :live-writers   (fn [agent-id]) -> swarm rows writing that store, nil = unknown

   :shared  the registry holds the dir: reading shares the writer's connection.
   :free    no live agent of this process writes the dir (the swarm registry
            answered and names none, and no hive-agent count holds it).
   :busy    a holder or a live writer is known.
   :unknown nothing can establish that no writer holds it.

   Opening a dir a writer of this process holds, without the registry,
   re-runs Datalevin's DBI-open commit on the live env: the SIGSEGV. A
   registry with NO holder for a dir a live agent writes (a hot reload left
   old writers outside it) is not trusted: :busy."
  [{:keys [registry-holds legacy-holds live-writers]} dir]
  (let [agent-id (.getName (io/file (str dir)))
        writers  (when live-writers (live-writers agent-id))]
    (cond
      ;; A pre-registry count outranks the registry: after a hot reload an
      ;; old writer may sit in it beside a fresh registry entry.
      (and legacy-holds (pos? (long (legacy-holds dir))))     :busy
      (and registry-holds (pos? (long (registry-holds dir)))) :shared
      (nil? writers)                                          :unknown
      (seq writers)                                           :busy
      :else                                                   :free)))

(defn read-dir-with
  "Read one store dir with hive-agent's store fns `ops` {:make :query :close}
   plus the probes of `store-decision`. Opens ONLY on :shared or :free,
   decided twice, the second time right before the open; otherwise answers
   (err :transcript/store-busy) or (err :transcript/store-unknown) and opens
   nothing. The window between that last check and the open stays until a
   hive-agent with the shared registry is loaded."
  [{:keys [make query close] :as ops} dir agent-id]
  (let [refuse  (fn [d]
                  (r/err (if (= d :busy) :transcript/store-busy :transcript/store-unknown)
                         {:dir dir :agent-id agent-id
                          :message (if (= d :busy)
                                     "Store is held by a live writer; not opened"
                                     "Cannot establish that no writer holds this store; not opened")}))
        safe?   #{:shared :free}
        d       (store-decision ops dir)]
    (if-not (safe? d)
      (refuse d)
      (let [d (store-decision ops dir)]
        (if-not (safe? d)
          (refuse d)
          (let [opened (make {:agent-id agent-id :dir dir})]
            (if-not (r/ok? opened)
              opened
              (let [store (:ok opened)]
                (try (query store agent-id)
                     (finally (close store)))))))))))

(defn hive-agent-read-dir
  "Read one hive-agent Datalevin store dir through hive-agent's own store.
   Result<[entry]>; an err when hive-agent is not on the classpath, and
   :transcript/store-busy or :transcript/store-unknown when the dir is not
   safe to open (see `store-decision`)."
  [dir agent-id]
  (if-let [make (swallow/attempt (requiring-resolve 'hive-agent.loop.transcript.datalevin-store/make-store))]
    (read-dir-with {:make           make
                    :query          (requiring-resolve 'hive-agent.loop.transcript.store/query-by-agent)
                    :close          (requiring-resolve 'hive-agent.loop.transcript.store/close!)
                    :registry-holds (registry-holds-fn)
                    :legacy-holds   (legacy-holds-fn)
                    :live-writers   (live-writers-fn)}
                   dir agent-id)
    (r/err :transcript/hive-agent-absent
           {:message "hive-agent is not on the classpath; Datalevin transcripts are unreadable"})))

(m/=> read-dir-with [:=> [:cat [:map-of :any :any] :any :any] [:map-of :any :any]])

(m/=> store-decision [:=> [:cat [:map-of :any :any] :any] [:enum :shared :free :busy :unknown]])

(defn hive-agent-datalevin-root
  "hive-agent's configured transcript root, or its documented default."
  []
  (or (swallow/attempt (some-> (requiring-resolve 'hive-agent.config/resolve-transcript-datalevin-root)
                   (apply [])))
      (str (System/getProperty "user.home") "/.local/share/hive-agent/transcripts")))

;; =============================================================================
;; Composite
;; =============================================================================

(defrecord CompositeSource [sources]
  TranscriptSource
  (list-transcripts [_]
    (r/try-effect
     (vec (mapcat #(let [res (list-transcripts %)]
                     (if (r/ok? res) (:ok res) []))
                  sources))))
  (read-entries [_ agent-id]
    ;; First source holding the agent wins; a non-not-found error is returned.
    (reduce (fn [acc s]
              (let [res (read-entries s agent-id)]
                (if (= :transcript/not-found (:error res)) acc (reduced res))))
            (not-found agent-id :any)
            sources)))

(defn composite
  "One source reading each of `sources` in order."
  [sources]
  (->CompositeSource (vec sources)))

(defn default-source
  "JSONL under /tmp/hive-transcripts, then hive-agent's Datalevin root."
  []
  (composite [(->JsonlSource "/tmp/hive-transcripts")
              (->DatalevinSource (hive-agent-datalevin-root) hive-agent-read-dir)]))

;; =============================================================================
;; Parent index — who spawned a ling
;; =============================================================================

(defprotocol ParentIndex
  (parent-of [this agent-id] "The spawning coordinator/ling id of `agent-id`, or nil."))

(defn row-parent
  "Parent id of a slave registry row: :slave/parent (an id or a pulled
   {:slave/id} map), else :slave/parent-id. nil when the row names none."
  [row]
  (let [p (:slave/parent row)
        p (if (map? p) (:slave/id p) p)]
    (if (string? p) p (:slave/parent-id row))))

(m/=> row-parent [:=> [:cat [:maybe [:map-of :any :any]]] [:maybe :string]])

(defn default-parent-index
  "A ParentIndex over the swarm slave registry. `parent-of` answers nil when
   the registry or the row is absent."
  []
  (reify ParentIndex
    (parent-of [_ agent-id]
      (swallow/attempt (when-let [get-slave (requiring-resolve 'hive-mcp.swarm.datascript.queries/get-slave)]
          (row-parent (get-slave agent-id)))))))

;; Defaults to 0, so never nil.
(m/=> entry-turn [:=> [:cat [:maybe [:map-of :any :any]]] :some])

(m/=> hive-agent-read-dir
      [:=> [:cat :any :any] [:map-of :any :any]])

(m/=> hive-agent-datalevin-root [:=> [:cat] :string])

(def CompositeSourceValue
  "A CompositeSource. Generates an empty composite so output schemas sample."
  [:fn {:gen/elements [(->CompositeSource [])]}
   #(instance? hive_mcp.agent.transcript_source.CompositeSource %)])

(def ParentIndexValue
  "A ParentIndex. Generates the (stateless) default index."
  [:fn {:gen/elements [(default-parent-index)]}
   #(satisfies? ParentIndex %)])

(m/=> composite [:=> [:cat [:sequential :any]] CompositeSourceValue])

(m/=> default-source [:=> [:cat] CompositeSourceValue])

(m/=> default-parent-index [:=> [:cat] ParentIndexValue])
