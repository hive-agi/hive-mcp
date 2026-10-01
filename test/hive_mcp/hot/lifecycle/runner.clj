(ns hive-mcp.hot.lifecycle.runner
  "Drive any IHotHost through a Script and record what it did.

   Pipeline stratum (CPPB): the only effects are the host's own, reached
   through the port. The same script run against the model and against a live
   host yields two Traces that can be compared value for value."
  (:require [hive-mcp.hot.lifecycle.port :as port]))

(defn run-script
  "Apply SCRIPT to HOST op by op. Returns a Trace."
  [host script]
  (let [baseline (port/observe host)]
    {:trace/baseline baseline
     :trace/steps
     (reduce (fn [steps op]
               (let [outcome (port/apply-op! host op)]
                 (conj steps {:step/op op :step/outcome outcome :step/obs (port/observe host)})))
             []
             script)}))

(defn canonical
  "TRACE with every tool table sorted, for comparing hosts whose advertised
   order differs. Duplicates survive sorting, so :no-duplicate-tools still
   sees them."
  [trace]
  (let [sort-obs #(update % :obs/tools (comp vec sort))]
    (-> trace
        (update :trace/baseline sort-obs)
        (update :trace/steps (partial mapv #(update % :step/obs sort-obs))))))
