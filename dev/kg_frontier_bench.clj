(ns kg-frontier-bench
  "Scratch-only Datahike benchmark. Never opens the production database."
  (:require [datahike.api :as d]
            [hive-datahike.kg.store :as store]
            [hive-spi.kg.protocol :as kg]
            [hive-mcp.knowledge-graph.protocols :as p]
            [hive-mcp.knowledge-graph.queries :as q]
            [hive-mcp.knowledge-graph.frontier-batch-test :as reference]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn query-reader [s calls]
  (let [read-edges (fn [attr ids batch?]
                     (swap! calls inc)
                     (let [query (if batch?
                                   [:find [(list 'pull '?e '[*]) '...]
                                    :in '$ '[?n ...] :where ['?e attr '?n]]
                                   [:find [(list 'pull '?e '[*]) '...]
                                    :in '$ '?n :where ['?e attr '?n]])
                           rows (kg/query s query [ids])]
                       (if batch? (group-by attr rows) rows)))]
    (reify p/IEdgeReader
      (get-edges-from [_ id] (read-edges :kg-edge/from id false))
      (get-edges-from [_ id _] (read-edges :kg-edge/from id false))
      (get-edges-to [_ id] (read-edges :kg-edge/to id false))
      (get-edges-to [_ id _] (read-edges :kg-edge/to id false))
      (batch-get-edges-from [_ ids] (read-edges :kg-edge/from ids true))
      (batch-get-edges-from [_ ids _] (read-edges :kg-edge/from ids true))
      (batch-get-edges-to [_ ids] (read-edges :kg-edge/to ids true))
      (batch-get-edges-to [_ ids _] (read-edges :kg-edge/to ids true)))))

(defn synthetic-edges [width background]
  (map-indexed
   (fn [i e] (assoc e :kg-edge/id (str "edge-" i)
                    :kg-edge/scope "bench" :kg-edge/confidence 0.8
                    :kg-edge/created-at (java.util.Date. 0)))
   (concat
    (mapcat (fn [i] [(reference/edge "root" (str "n" i) :refines)
                     (reference/edge (str "n" i) (str "leaf" i) :refines)]) (range width))
    (map (fn [i] (reference/edge (str "bg" (quot i 8)) (str "bg" (mod (+ i 37) background))
                               :depends-on)) (range background)))))

(defn run! []
  (let [s (store/create-store {:backend :mem :id (java.util.UUID/randomUUID)
                              :keep-history? false})
        calls (atom 0)
        reader (query-reader s calls)
        opts {:edge-reader reader :max-depth 2 :direction :both}]
    (try
      (kg/ensure-conn! s)
      (kg/transact! s (:tx-data (edn/read-string (slurp (io/resource "hive_mcp/norms/kg/001-kg-edge-schema.edn")))))
      (doseq [batch (partition-all 1000 (synthetic-edges 300 30000))]
        (kg/transact! s (vec batch)))
      ;; Warm both shapes, then alternate AB / BA to expose order bias.
      (dotimes [_ 2] (reference/scalar-traverse "root" opts) (q/traverse "root" opts))
      (let [measure (fn [label f]
                      (reset! calls 0)
                      (let [t (System/nanoTime) rows (f "root" opts)]
                        {:label label :ms (/ (- (System/nanoTime) t) 1e6)
                         :queries @calls :rows (count rows)
                         :result (set rows)}))
            runs (mapv (fn [i]
                         (let [pair (if (even? i)
                                      [(measure :before reference/scalar-traverse)
                                       (measure :after q/traverse)]
                                      [(measure :after q/traverse)
                                       (measure :before reference/scalar-traverse)])]
                           (assert (apply = (map :result pair)))
                           (mapv #(dissoc % :result) pair))) (range 6))]
        {:edges 30600 :width 300 :direction :both :runs runs})
      (finally (kg/close! s)))))
