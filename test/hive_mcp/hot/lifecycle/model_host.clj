(ns hive-mcp.hot.lifecycle.model-host
  "The model as an IHotHost: the stub every runner test drives.

   Boundary stratum only in the smallest sense: it holds the model state in
   an atom so it can answer the port. All behaviour is the pure model's."
  (:require [hive-mcp.hot.lifecycle.model :as model]
            [hive-mcp.hot.lifecycle.port :as port]))

(defrecord ModelHost [state]
  port/IHotHost
  (apply-op! [_ op]
    (let [[outcome state'] (model/step @state op)]
      (reset! state state')
      outcome))
  (observe [_]
    (model/observation @state)))

(defn model-host
  "A fresh ModelHost for SPEC, every addon mounted."
  [spec]
  (->ModelHost (atom (model/initial-state spec))))
