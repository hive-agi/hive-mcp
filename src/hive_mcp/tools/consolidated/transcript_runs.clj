(ns hive-mcp.tools.consolidated.transcript-runs
  "Wire projections of chat-point runs for the transcript tool: the `runs`
   page and each listing row's latest `run_id`.

   Reads go through a `RunIndex` (hive-mcp.agent.chat-point-runs) and a
   `ParentIndex` (hive-mcp.agent.transcript-source), both injected."
  (:require [hive-dsl.result :as r]
            [hive-mcp.agent.chat-point-runs :as cpr]
            [hive-mcp.agent.transcript-insight :as ti]
            [hive-mcp.agent.transcript-source :as src]
            [taoensso.timbre :as log]))

(defn latest-runs
  "{agent-id run-id} read through `runs`; {} when the store is unreadable."
  [runs]
  (let [res (cpr/list-runs runs)]
    (if (r/ok? res)
      (cpr/latest-run-by-agent (:ok res))
      (do (log/warn "[transcript] chat-point runs unreadable:" (pr-str res))
          {}))))

(defn- point-wire [{:keys [run-id seq turn]}]
  {:run_id run-id :seq seq :turn turn})

(defn- parent-of [parents agent-id]
  (when agent-id (src/parent-of parents agent-id)))

(defn- run-wire
  "Wire projection of one RunRow, its tip read through `runs`."
  [runs parents row]
  (let [tip-res (cpr/run-tip runs (:run-id row))
        tip     (when (r/ok? tip-res) (:ok tip-res))]
    {:run_id       (:run-id row)
     :agent        (:agent-id row)
     :project      (:project-id row)
     :parent       (or (:parent row) (parent-of parents (:agent-id row)))
     :parent_point (some-> (:parent-point row) point-wire)
     :model        (:model row)
     :created      (some-> (:created row) ti/iso)
     :tip          (when tip {:seq (:seq tip) :turn (:turn tip)})
     :ended        (cpr/ended? (:message tip))}))

(defn runs-page
  "Result<{:runs [row] :count n :matched n}>: runs newest first, filtered by
   `opts` (ti/select-listing opts; :parent is the spawning coordinator) and
   capped at `limit`. A row: run_id agent project parent parent_point model
   created tip {seq turn} ended."
  [runs parents opts limit]
  (r/map-ok (cpr/list-runs runs)
            (fn [rows]
              (let [rows    (map #(cond-> (assoc % :modified (:created %))
                                    (:parent opts) (assoc :parent (parent-of parents (:agent-id %))))
                                 rows)
                    matched (ti/select-listing rows opts)
                    page    (take limit matched)]
                {:runs    (mapv #(run-wire runs parents %) page)
                 :count   (count page)
                 :matched (count matched)}))))
