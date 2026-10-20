(ns hive-mcp.protocols.registry-contract-test
  "Contract facets for the slot and registry builders and the slot watches.
   Every slot here is built by the test, so no shared state is touched."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.protocols.registry :as reg]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema single-slot-seeds-its-initial-value
  #'reg/single-slot
  {:in [:cat [:maybe [:map [:initial {:optional true} [:maybe :int]]]]]
   :out reg/SlotValue
   :rel (fn [[cfg] out]
          (and (= (:initial cfg) (reg/current out))
               (= (some? (:initial cfg)) (reg/present? out))))
   :num-tests 100})

(deftrifecta-from-schema multi-slot-seeds-its-initial-map
  #'reg/multi-slot
  {:in [:cat [:maybe [:map [:initial {:optional true} [:maybe [:map-of :keyword :int]]]]]]
   :out reg/RegistryValue
   :rel (fn [[cfg] out] (= (or (:initial cfg) {}) (reg/reg-snapshot out)))
   :num-tests 100})

(defn- watched? [slot k] (contains? (.getWatches ^clojure.lang.IRef (:state slot)) k))

(deftrifecta-from-schema watch-slot-registers-under-its-key
  #'reg/watch-slot!
  {:in [:cat reg/WatchedSlot :keyword ifn?]
   :out reg/WatchedSlot
   :rel (fn [[slot k] out] (and (identical? slot out) (watched? out k)))
   :num-tests 60})

(deftrifecta-from-schema unwatch-slot-removes-the-key
  #'reg/unwatch-slot!
  {:in [:cat reg/WatchedSlot :keyword]
   :out reg/WatchedSlot
   :rel (fn [[slot k] out] (and (identical? slot out) (not (watched? out k))))
   :num-tests 60})

(golden/deftest-golden slot-semantics-golden
  "test/golden/hive-mcp/protocols/registry-semantics.edn"
  (let [torn (atom [])
        seen (atom [])
        single (reg/single-slot {:validate keyword? :on-empty (constantly :fallback)
                                 :teardown #(swap! torn conj %)})
        _ (reg/watch-slot! single ::w (fn [old new] (swap! seen conj [old new])))
        s-empty [(reg/current single) (reg/present? single)]
        s-installed (reg/install! single :impl-a)
        s-rejected (try (reg/install! single "not-a-keyword") :accepted
                        (catch AssertionError _ :rejected))
        s-cleared (reg/clear! single)
        _ (reg/unwatch-slot! single ::w)
        _ (reg/install! single :impl-b)
        multi (reg/multi-slot {:validate int? :on-missing (fn [k snap] [:missing k (count snap)])
                               :initial {:a 1}})
        m-put (reg/reg-put! multi :b 2)
        m-merged (sort (reg/reg-merge! multi {:c 3 :d 4}))
        m-missing (reg/reg-get multi :zz)
        m-removed (reg/reg-remove! multi :a)]
    {:single {:empty s-empty :installed s-installed :rejected s-rejected :cleared s-cleared
              :torn @torn :transitions @seen :after-unwatch (reg/current single)}
     :multi {:put m-put :merged m-merged :missing m-missing :removed m-removed
             :snapshot (into (sorted-map) (reg/reg-snapshot multi))
             :cleared (reg/reg-clear! multi) :after-clear (reg/reg-snapshot multi)}}))

(deftest the-nil-config-builds-a-nil-on-empty-slot
  (let [s (reg/single-slot nil)]
    (is (nil? (reg/current s)))
    (is (false? (reg/present? s)))))
