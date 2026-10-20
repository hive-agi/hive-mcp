(ns hive-mcp.events.context-contract-test
  "Contracts and generative properties for pure event context updates."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.context :as context]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema coeffect-association
  #'context/assoc-coeffect
  {:in [:cat [:map [:coeffects [:map-of :keyword :int]]] :keyword :int]
   :out [:map [:coeffects [:map-of :keyword :int]]]
   :rel (fn [[ctx key value] result]
          (and (= value (get-in result [:coeffects key]))
               (= (dissoc ctx :coeffects) (dissoc result :coeffects))
               (= (dissoc (:coeffects ctx) key)
                  (dissoc (:coeffects result) key))))
   :num-tests 500})

(deftrifecta-from-schema effect-association
  #'context/assoc-effect
  {:in [:cat [:map [:effects [:map-of :keyword :int]]] :keyword :int]
   :out [:map [:effects [:map-of :keyword :int]]]
   :rel (fn [[ctx key value] result]
          (and (= value (get-in result [:effects key]))
               (= (dissoc ctx :effects) (dissoc result :effects))
               (= (dissoc (:effects ctx) key)
                  (dissoc (:effects result) key))))
   :num-tests 500})

(deftest absent-values-and-variadic-updates
  (is (= :missing (context/get-coeffect {} :none :missing)))
  (is (= :missing (context/get-effect {} :none :missing)))
  (is (= {:coeffects {:count 5}}
         (context/update-coeffect {:coeffects {:count 2}} :count + 3)))
  (is (= {:effects {:count 5}}
         (context/update-effect {:effects {:count 2}} :count + 3))))

(coverage/deftest-contract-coverage event-context-contract-coverage
  {:paths ["src/hive_mcp/events/context.clj"]})
