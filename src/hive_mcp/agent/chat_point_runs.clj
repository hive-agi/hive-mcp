(ns hive-mcp.agent.chat-point-runs
  "Read port over hive-agent's chat-point run store: the recorded runs a
   spawn's `resume {run_id}` can continue.

   Contract: every fn of `RunIndex` returns a Result.
     list-runs -> Result<[RunRow]>
                  RunRow {:run-id str :agent-id str? :project-id str?
                          :model str? :created epoch-ms?
                          :parent-point {:run-id :seq :turn}?}
     run-tip   -> Result<{:seq :turn :message}|nil>, the last message of
                  the run's lineage; nil when the run is unknown or empty.

   `hive-agent-run-index` adapts an ops map over hive-agent.loop.chat-point;
   with no ops (hive-agent absent) it is an empty index."
  (:require [hive-dsl.result :as r]))

;; =============================================================================
;; L0: port
;; =============================================================================

(defprotocol RunIndex
  (list-runs [this] "Result<[RunRow]>.")
  (run-tip [this run-id] "Result<{:seq :turn :message}|nil>."))

;; =============================================================================
;; L1: pure calculations
;; =============================================================================

(defn run-row
  "RunRow of a hive-agent Run map {:run/id :run/parent :run/meta}. Pure."
  [run]
  (let [meta (:run/meta run)]
    {:run-id       (:run/id run)
     :agent-id     (:agent-id meta)
     :project-id   (:project-id meta)
     :model        (:model meta)
     :created      (:created meta)
     :parent-point (:run/parent run)}))

(defn latest-run-by-agent
  "{agent-id run-id}: each agent's newest run by :created (absent = 0).
   Rows without an agent are skipped. Pure."
  [rows]
  (->> rows
       (filter :agent-id)
       (sort-by #(or (:created %) 0))
       (reduce (fn [m row] (assoc m (:agent-id row) (:run-id row))) {})))

(defn ended?
  "True when `message` is an assistant reply with no tool calls left to
   answer, i.e. the run stopped on its own final text. Pure."
  [message]
  (and (= "assistant" (some-> (:role message) name))
       (empty? (:tool_calls message))))

;; =============================================================================
;; L2: hive-agent adapter
;; =============================================================================

(defrecord HiveAgentRunIndex [ops]
  ;; ops: {:store (fn [] store) :list (fn [store] [Run])
  ;;       :lineage (fn [store run-id] [Message]|nil)}, or nil.
  RunIndex
  (list-runs [_]
    (if-not ops
      (r/ok [])
      (r/try-effect (mapv run-row ((:list ops) ((:store ops)))))))
  (run-tip [_ run-id]
    (if-not ops
      (r/ok nil)
      (r/try-effect
       (some-> ((:lineage ops) ((:store ops)) run-id)
               seq vec peek
               (select-keys [:seq :turn :message]))))))

(defn hive-agent-run-index
  "RunIndex over `ops` (see HiveAgentRunIndex); nil ops is an empty index."
  [ops]
  (->HiveAgentRunIndex ops))

(defn- soft-resolve [sym]
  (try (requiring-resolve sym)
       (catch Throwable _hive-agent-absent nil)))

(defn hive-agent-ops
  "Ops over hive-agent's configured chat-point store, or nil when
   hive-agent is not on the classpath. An unknown run's lineage is nil."
  []
  (when-let [store-fn (soft-resolve 'hive-agent.config/resolve-chat-point-store)]
    (let [list-fn    (soft-resolve 'hive-agent.loop.chat-point/list-runs)
          get-run    (soft-resolve 'hive-agent.loop.chat-point/get-run)
          lineage-fn (soft-resolve 'hive-agent.loop.chat-point/lineage)]
      (when (and list-fn get-run lineage-fn)
        {:store   store-fn
         :list    list-fn
         :lineage (fn [store run-id]
                    (when (get-run store run-id) (lineage-fn store run-id)))}))))

(defn default-run-index
  "The RunIndex over hive-agent's configured chat-point store."
  []
  (hive-agent-run-index (hive-agent-ops)))
