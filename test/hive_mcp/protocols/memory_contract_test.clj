(ns hive-mcp.protocols.memory-contract-test
  "Contract facets for the host-side memory fns: the role predicates, the
   projected batch read and the :default store lifecycle helpers.

   The lifecycle tests own the :default slot: the prior store is unregistered
   (never disconnected) for the test and registered again afterwards."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.protocols.memory :as mem]
            [hive-mcp.protocols.memory-liveness :as ml]
            [hive-mcp.recall.golden :as rg]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.memory.stub :as stub]
            [hive-test.golden :as golden])
  (:import [hive_spi.memory.stub StubMemoryStore]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-the-default-slot [f]
  (let [prior (:default (mem/registered-stores))]
    (mem/unregister-store! :default)
    (try (f)
         (finally (if prior
                    (mem/register-store! :default prior)
                    (mem/unregister-store! :default))))))

(use-fixtures :each own-the-default-slot)

;; A stub implements every role but routing and temporal; the golden store
;; implements IMemoryStore alone; an int implements nothing.
(def ^:private AnyStore [:or mem/MemoryStore rg/GoldenStore :int])

(defn- stub? [x] (instance? StubMemoryStore x))

(deftrifecta-from-schema batch-store-pred
  #'mem/batch-store?
  {:in [:cat AnyStore]
   :out :boolean
   :rel (fn [[x] out] (= out (stub? x)))
   :num-tests 60})

(deftrifecta-from-schema batch-read-store-pred
  #'mem/batch-read-store?
  {:in [:cat AnyStore]
   :out :boolean
   :rel (fn [[x] out] (= out (stub? x)))
   :num-tests 60})

(deftrifecta-from-schema analytics-store-pred
  #'mem/analytics-store?
  {:in [:cat AnyStore]
   :out :boolean
   :rel (fn [[x] out] (= out (stub? x)))
   :num-tests 60})

(deftrifecta-from-schema metadata-write-store-pred
  #'mem/metadata-write-store?
  {:in [:cat AnyStore]
   :out :boolean
   :rel (fn [[x] out] (= out (stub? x)))
   :num-tests 60})

(deftrifecta-from-schema staleness-store-pred
  #'mem/staleness-store?
  {:in [:cat AnyStore]
   :out :boolean
   :rel (fn [[x] out] (= out (stub? x)))
   :num-tests 60})

(deftrifecta-from-schema routing-store-pred
  #'mem/routing-store?
  {:in [:cat AnyStore]
   :out :boolean
   :rel (fn [_ out] (false? out))
   :num-tests 60})

(deftrifecta-from-schema temporal-store-pred
  #'mem/temporal-store?
  {:in [:cat AnyStore]
   :out :boolean
   :rel (fn [_ out] (false? out))
   :num-tests 60})

(deftrifecta-from-schema liveness-store-pred
  #'ml/liveness-store?
  {:in [:cat AnyStore]
   :out :boolean
   :rel (fn [[x] out] (= out (stub? x)))
   :num-tests 60})

(def ^:private PopulatedStore
  "A stub store holding the generated ids, each a note whose content names it."
  [:fn {:gen/schema [:vector {:min 1 :max 4} [:enum "a" "b" "c" "d"]]
        :gen/fmap (fn [ids]
                    (let [s (stub/create-store)]
                      (doseq [i ids] (mem/add-entry! s {:id i :type "note" :content (str "c-" i)}))
                      s))}
   stub?])

;; Vectors only: the stub's get-entries runs `distinct` over the ids, which
;; throws on a set (a hive-spi stub defect, filed separately); the contract
;; itself admits sets, as real backends take them.
(def ^:private Wanted [:vector [:enum "a" "b" "c" "z"]])

(defn- present [s ids] (count (filter #(mem/get-entry s %) (distinct ids))))

(deftrifecta-from-schema projected-read-without-opts-is-the-batch-read
  #'mem/get-entries-projected
  {:in [:cat PopulatedStore Wanted]
   :out mem/StoreEntries
   :rel (fn [[s ids] out]
          (and (every? #(contains? (set ids) (:id %)) out)
               (= (present s ids) (count out))
               (every? :content out)))
   :num-tests 60})

(deftrifecta-from-schema projected-read-trims-to-the-output-fields
  #'mem/get-entries-projected
  {:in [:cat PopulatedStore Wanted
        [:or [:map [:output-fields [:vector {:min 1 :max 3} [:enum "id" "type" "content"]]]]
         [:map [:output-fields [:maybe [:vector {:max 0} :string]]]]
         :nil]]
   :out mem/StoreEntries
   :rel (fn [[s ids opts] out]
          (let [fs (seq (:output-fields opts))]
            (and (= (present s ids) (count out))
                 (if fs
                   (every? #(= (set (map keyword fs)) (set (keys %))) out)
                   (every? :content-hash out)))))
   :classify (fn [[_ _ opts] _] (if (seq (:output-fields opts)) :projected :full))
   :classify-domain #{:projected :full}
   :num-tests 100})

(golden/deftest-golden default-store-lifecycle-golden
  "test/golden/hive-mcp/protocols/memory-default-lifecycle.edn"
  (let [before {:set? (mem/store-set?) :healthy? (mem/active-store-healthy?)
                :status (mem/active-store-status)}
        s (stub/create-store)
        _ (mem/register-store! :default s)
        during {:set? (mem/store-set?) :healthy? (mem/active-store-healthy?)
                :status (dissoc (mem/active-store-status) :capabilities)}
        reset (mem/reset-active-store!)]
    {:before before :during during :reset reset
     :after {:set? (mem/store-set?) :healthy? (mem/active-store-healthy?)
             :disconnected? (false? (mem/connected? s))}}))

(deftest a-throwing-health-check-reads-unhealthy
  (let [s (stub/create-store)
        broken (reify hive-spi.memory.ports/IMemoryStore
                 (health-check [_] (throw (ex-info "down" {})))
                 (store-status [_] {:backend "broken"})
                 (disconnect! [_] (throw (ex-info "already gone" {}))))]
    (is (true? (mem/connected? s)))
    (mem/register-store! :default broken)
    (is (false? (mem/active-store-healthy?)))
    (is (= {:backend "broken" :healthy? false :errors ["down"]} (mem/active-store-status)))
    (is (nil? (mem/reset-active-store!)) "a throwing disconnect is swallowed")
    (is (false? (mem/store-set?)))))
