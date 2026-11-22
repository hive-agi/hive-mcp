(ns hive-mcp.resilience.with-resilience-test
  "Trifecta for the `with-resilience` macro (hive-mcp.resilience.store), the
   handler-side retry wrapper with 12 production callers.

     golden    : outcome table per scripted body (value, retried, thrown)
     property  : a body that succeeds is evaluated exactly once and its value
                 is returned unchanged
     mutation  : the macro expands to `call-with-resilience`; mutants that
                 swallow the failure or drop the retry are caught

   No store is registered, so a transient failure retries without a heal kick:
   no clock, no sleep, no redefinition."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.resilience.store :as store]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private throwables
  {:transient (fn [] (java.io.IOException. "Connection reset"))
   :schema    (fn [] (ex-info "Milvus code=1804" {:milvus-clj.client/transport :http
                                                  :code 1804 :message "dim"}))
   :fatal     (fn [] (ex-info "too large" {:err/tag :embedder/input-too-large}))})

(defn- run-script
  "Run `script` (a value is returned, a `throwables` key is thrown; the last
   step repeats) through `with-resilience`. Answers what happened as data."
  [script]
  (let [calls (atom 0)
        res   (try {:value (store/with-resilience
                             (let [step (nth script (min (dec (count script)) @calls))]
                               (swap! calls inc)
                               (if-let [mk (and (keyword? step) (throwables step))]
                                 (throw (mk))
                                 step)))}
                   (catch Throwable t
                     {:thrown (.getSimpleName (class t))
                      :tag    (:err/tag (ex-data t))}))]
    (assoc res :calls @calls)))

(def ^:private scripts
  {:ok                 [:fine]
   :transient-recovers [:transient :recovered]
   :transient-twice    [:transient]
   :schema-mismatch    [:schema]
   :fatal              [:fatal]})

(deftest-golden with-resilience-outcomes-golden
  "test/golden/hive-mcp/resilience/with-resilience.edn"
  (into (sorted-map) (for [[k s] scripts] [k (run-script s)])))

(defspec a-succeeding-body-runs-once-and-returns-its-value 200
  (prop/for-all [v (gen/one-of [gen/small-integer gen/string gen/keyword
                                (gen/vector gen/small-integer)])]
    (let [calls (atom 0)
          out   (store/with-resilience (swap! calls inc) v)]
      (and (= v out) (= 1 @calls)))))

(defspec a-fatal-body-is-never-retried 100
  (prop/for-all [msg gen/string-alphanumeric]
    (let [calls (atom 0)
          out   (try (store/with-resilience
                       (swap! calls inc)
                       (throw (ex-info msg {:err/tag :embedder/input-too-large})))
                     (catch clojure.lang.ExceptionInfo e (ex-message e)))]
      (and (= msg out) (= 1 @calls)))))

(deftest a-transient-body-is-retried-once
  (let [calls (atom 0)]
    (is (= :recovered
           (store/with-resilience
             (if (= 1 (swap! calls inc))
               (throw (java.io.IOException. "Connection reset"))
               :recovered))))
    (is (= 2 @calls))))

(hmut/deftest-mutations with-resilience-mutations
  hive-mcp.resilience.store/call-with-resilience
  [["swallows failures"   (fn [f & _] (try (f) (catch Throwable _ nil)))]
   ["never retries"       (fn [f & _] (f))]
   ["retries any failure" (fn [f & _] (try (f) (catch Throwable _ (f))))]
   ["returns nil"         (fn [& _] nil)]]
  (fn []
    (let [calls (atom 0)]
      (is (= :recovered
             (store/with-resilience
               (if (= 1 (swap! calls inc))
                 (throw (java.io.IOException. "Connection reset"))
                 :recovered)))))
    (let [calls (atom 0)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (store/with-resilience
                     (swap! calls inc)
                     (throw (ex-info "fatal" {:err/tag :embedder/input-too-large}))))
          "a fatal failure propagates")
      (is (= 1 @calls) "and is not retried"))))
