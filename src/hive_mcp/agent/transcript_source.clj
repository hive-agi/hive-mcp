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
        (r/try-effect
         (->> dirs
              (mapcat (fn [{:keys [dir]}]
                        (let [res (read-dir (str dir) agent-id)]
                          (if (r/ok? res)
                            (:ok res)
                            (throw (ex-info (str "Datalevin read failed: " (pr-str res))
                                            {:dir (str dir) :result res}))))))
              (sort-by entry-turn)
              vec))))))

(defn- canonical [path]
  (.getCanonicalPath (io/file (str path))))

(defn- held-count-fn
  "(fn [dir]) -> live holders of `dir` in hive-agent's per-path handle
   counts, matched by CANONICAL path: those counts are keyed by the writer's
   path string, and the listing spells the same dir its own way. nil when
   hive-agent exposes no counts."
  []
  (when-let [counts (swallow/attempt
                     (some-> (requiring-resolve 'hive-agent.loop.transcript.datalevin-store/handle-counts) deref))]
    (when (instance? clojure.lang.IDeref counts)
      (fn [dir]
        (let [k (canonical dir)]
          (reduce-kv (fn [n path c] (if (= k (canonical path)) (+ n (long c)) n))
                     0 @counts))))))

(defn read-dir-with
  "Read one store dir with hive-agent's store fns `ops`
   {:make :query :close :held-count :shared?}.

   Without a shared connection registry in hive-agent (`:shared?` false),
   opening a dir another holder in this process already has open re-runs
   Datalevin's DBI-open commit on the live env, which segfaulted the host
   JVM. That read is refused as :transcript/store-busy and never opened."
  [{:keys [make query close held-count shared?]} dir agent-id]
  (if (and (not shared?) held-count (pos? (long (held-count dir))))
    (r/err :transcript/store-busy
           {:dir dir :agent-id agent-id
            :message "Store is held open by a live writer; hive-agent cannot share it yet"})
    (let [opened (make {:agent-id agent-id :dir dir})]
      (if-not (r/ok? opened)
        opened
        (let [store (:ok opened)]
          (try (query store agent-id)
               (finally (close store))))))))

(defn hive-agent-read-dir
  "Read one hive-agent Datalevin store dir through hive-agent's own store.
   Result<[entry]>; an err when hive-agent is not on the classpath, or
   :transcript/store-busy when a writer holds the dir and this hive-agent
   has no shared connection registry (see `read-dir-with`)."
  [dir agent-id]
  (if-let [make (swallow/attempt (requiring-resolve 'hive-agent.loop.transcript.datalevin-store/make-store))]
    (read-dir-with {:make       make
                    :query      (requiring-resolve 'hive-agent.loop.transcript.store/query-by-agent)
                    :close      (requiring-resolve 'hive-agent.loop.transcript.store/close!)
                    :held-count (held-count-fn)
                    :shared?    (some? (swallow/attempt (requiring-resolve 'hive-agent.loop.transcript.datalevin-store/acquire-conn!)))}
                   dir agent-id)
    (r/err :transcript/hive-agent-absent
           {:message "hive-agent is not on the classpath; Datalevin transcripts are unreadable"})))

(m/=> read-dir-with [:=> [:cat [:map-of :any :any] :any :any] [:map-of :any :any]])

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
