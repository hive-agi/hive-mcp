(ns hive-mcp.events.effects.memory-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.effects.memory :as memory]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema memory-write-handler-injection
  #'memory/set-memory-write-handler!
  {:in [:cat :nil]
   :out :nil
   :rel (fn [_ result] (nil? result))
   :num-tests 100})

(deftest handler-seam-accepts-callable
  (let [handler (fn [_] {:ok true})]
    (try
      (is (identical? handler (memory/set-memory-write-handler! handler)))
      (finally
        (memory/set-memory-write-handler! nil)))))
