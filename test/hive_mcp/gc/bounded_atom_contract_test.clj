(ns hive-mcp.gc.bounded-atom-contract-test
  "Contract facets for the bounded-atom registry, and the coverage gate for
   src/hive_mcp/gc.

   The registry is process-global. Every test here OWNS it: it saves the
   registry, starts from an empty one and puts the saved one back, so a golden
   never sees an atom another test registered, and no other suite loses one."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.gc.bounded-atom :as ba]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-the-registry [f]
  (let [saved @ba/*registry]
    (ba/reset-registry!)
    (try (f) (finally (reset! ba/*registry saved)))))

(use-fixtures :each own-the-registry)

(deftest-contract-coverage gc-contracts-gate
  {:paths ["src/hive_mcp/gc"]
   :exempt {}})

(deftest every-gc-contract-generates-both-sides
  (doseq [[fn-sym {:keys [schema]}] (get (m/function-schemas) 'hive-mcp.gc.bounded-atom)
          arity (let [s (m/schema schema)] (if (= :function (m/type s)) (m/children s) [s]))
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str fn-sym " " (pr-str (m/form side))))))

;; The property runs inside one fixture call, so each case clears the registry
;; itself before it registers.
(def ^:private Id [:enum :alpha :beta :gamma])

(deftrifecta-from-schema register-answers-the-id-and-fills-the-name
  #'ba/register!
  {:in [:cat Id ba/Spec]
   :out :keyword
   :rel (fn [[id spec] out]
          (and (= id out)
               (= (or (:name spec) (name id)) (:name (ba/get-spec id)))
               (ba/registered? id)))
   :num-tests 60})

(deftrifecta-from-schema deregister-reports-whether-it-was-there
  #'ba/deregister!
  {:in [:cat Id]
   :out :boolean
   :rel (fn [[id] out] (and (boolean? out) (not (ba/registered? id))))
   :num-tests 30})

(deftrifecta-from-schema sweep-all-totals-the-per-atom-stats
  #'ba/sweep-all!
  {:in [:cat]
   :out ba/SweepStats
   :rel (fn [_ {:keys [total-evicted errors per-atom atom-count]}]
          (and (= total-evicted (reduce + 0 (map :evicted per-atom)))
               (= errors (count (filter :error per-atom)))
               (= atom-count (count per-atom))))
   :num-tests 20})

(defn- spec-of [a max-entries evict]
  {:atom-ref a :max-entries max-entries
   :count-fn (fn [x] (count @x))
   :evict-fn evict})

(defn- drop-over [a {:keys [max-entries]}]
  (let [n (count @a) d (max 0 (- n max-entries))]
    (swap! a #(into (sorted-map) (drop d) %))
    d))

(golden/deftest-golden registry-lifecycle-golden
  "test/golden/hive-mcp/gc/bounded-atom-lifecycle.edn"
  (let [big (atom (into (sorted-map) (map (fn [i] [i i])) (range 7)))
        small (atom (sorted-map :a 1))
        broken (atom {})
        r1 (ba/register! :big (spec-of big 3 drop-over))
        r2 (ba/register! :small (assoc (spec-of small 10 (fn [_ _] nil)) :name "small-cache"))
        r3 (ba/register! :broken (spec-of broken 5 (fn [_ _] (throw (ex-info "evict exploded" {})))))
        ids (sort (ba/registered-ids))
        names (into (sorted-map) (map (fn [id] [id (:name (ba/get-spec id))])) ids)
        sweep (dissoc (ba/sweep-all!) :duration-ms)
        sweep (update sweep :per-atom #(vec (sort-by :atom-id %)))
        dereg [(ba/deregister! :broken) (ba/deregister! :broken)]
        after (sort (ba/registered-ids))
        reset (ba/reset-registry!)]
    {:registered [r1 r2 r3] :ids ids :names names :sweep sweep
     :big-after @big :deregister dereg :after after :reset reset
     :empty-after-reset (ba/registered-ids)}))

(deftest register-refuses-a-spec-without-its-functions
  ;; The :pre refuses it; with the contract instrumented the contract refuses
  ;; it first. Either way nothing is registered.
  (is (thrown? Throwable (ba/register! :x {:atom-ref (atom {}) :max-entries 1})))
  (is (thrown? Throwable (ba/register! :x (spec-of (atom {}) 0 drop-over))))
  (is (empty? (ba/registered-ids))))

(deftest a-sweep-arm-validates-only-as-itself
  (let [ok {:atom-id :a :name "a" :evicted 2 :remaining 1 :error nil}
        failed {:atom-id :a :name "a" :evicted 0 :remaining -1 :error "boom"}]
    (is (m/validate ba/AtomSweep ok))
    (is (m/validate ba/AtomSweep failed))
    (is (not (m/validate ba/AtomSweep (dissoc ok :error))) "every row names its error, nil included")
    (testing "an empty registry sweeps to zero"
      (is (= {:total-evicted 0 :errors 0 :per-atom [] :atom-count 0}
             (dissoc (ba/sweep-all!) :duration-ms))))))
