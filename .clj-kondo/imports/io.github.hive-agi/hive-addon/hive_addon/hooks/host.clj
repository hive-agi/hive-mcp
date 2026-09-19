(ns hive-addon.hooks.host
  (:require [clj-kondo.hooks-api :as api]))

(defn defsoft [{:keys [node]}]
  (let [[_ name-node & rest-nodes] (:children node)]
    {:node (api/list-node
            (list* (api/token-node 'do)
                   (api/list-node [(api/token-node 'declare) name-node])
                   rest-nodes))}))
