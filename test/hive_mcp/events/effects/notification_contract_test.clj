(ns hive-mcp.events.effects.notification-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.effects.notification :as notification]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema shout-payload-merge
  #'notification/shout-effect->shout-data
  {:in [:map
        [:message {:optional true} :string]
        [:task {:optional true} :string]
        [:data {:optional true} [:map
                                  [:message {:optional true} :string]
                                  [:task {:optional true} :string]]]]
   :out [:maybe :map]
   :rel (fn [effect result]
          (= (let [merged (merge (select-keys effect [:message :task]) (:data effect))]
               (if (contains? effect :data) merged (not-empty merged)))
             result))
   :num-tests 500})

(deftest explicit-data-overrides-top-level
  (is (= {:message "specific" :task "job"}
         (notification/shout-effect->shout-data
          {:message "general" :task "job" :data {:message "specific"}})))
  (is (nil? (notification/shout-effect->shout-data {}))))
