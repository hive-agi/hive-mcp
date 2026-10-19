(ns hive-mcp.events.contributions-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.contributions :as contributions]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema manifest-shape
  #'contributions/read-manifest
  {:in [:cat]
   :out [:map [:handlers [:vector :map]] [:effects [:vector :map]]]
   :rel (fn [_ manifest]
          (and (every? #(contains? % :install!) (:handlers manifest))
               (every? #(contains? % :install!) (:effects manifest))))
   :num-tests 100})

(deftest removable-contribution
  (let [key :m10/temporary]
    (try
      (is (= key (contributions/contribute! :effects key {:install! (fn [] nil)})))
      (is (contains? (contributions/contributed :effects) key))
      (finally (contributions/forget! :effects key)))
    (is (not (contains? (contributions/contributed :effects) key)))))
