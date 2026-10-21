(ns hive-mcp.resilience.runner-test
  "Name-matched facets for `hive-mcp.resilience.runner`.

     decide-only  trifecta over the contract's own Throwable_ generator
                  (every classifier arm, under 0-3 wrapper links)
     run          golden over scripted thunks: each thunk answers or throws
                  from a fixed per-call script, and the injected kick! port
                  records its calls, so the snapshot pins the Result AND the
                  number of attempts and kicks per classification. No clock,
                  no store."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.resilience.classify :as classify]
            [hive-mcp.resilience.policy :as policy]
            [hive-mcp.resilience.runner :as runner]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema decide-only-is-policy-of-classify
  #'runner/decide-only
  {:in [:cat classify/Throwable_]
   :out policy/RetryDecision
   :rel (fn [[t] out]
          (= out (if (= :err/transient (:adt/variant (classify/classify t)))
                   {:retry? true :backoff-ms policy/default-budget-ms :escalate? false}
                   {:retry? false :backoff-ms 0 :escalate? true})))
   :classify (fn [_ out] (if (:retry? out) :retry :escalate))
   :classify-domain #{:retry :escalate}
   :num-tests 300})

(def ^:private throwables
  {:transient (fn [] (java.io.IOException. "Connection reset"))
   :schema    (fn [] (ex-info "Milvus code=1804" {:milvus-clj.client/transport :http
                                                  :code 1804 :message "dim"}))
   :fatal     (fn [] (ex-info "too large" {:err/tag :embedder/input-too-large}))
   :unknown   (fn [] (RuntimeException. "something else"))})

(defn- scripted
  "Run `runner/run` over a thunk whose n-th call follows `script` (a value
   is returned, a keyword of `throwables` is thrown; the last entry repeats).
   Throwables in the Result are replaced by class + message for EDN."
  [script with-kick?]
  (let [calls (atom 0)
        kicks (atom 0)
        thunk (fn []
                (let [step (nth script (min (dec (count script)) @calls))]
                  (swap! calls inc)
                  (if-let [mk (and (keyword? step) (throwables step))]
                    (throw (mk))
                    step)))
        res   (if with-kick?
                (runner/run thunk {:kick! #(swap! kicks inc)})
                (runner/run thunk))
        show  (fn [^Throwable t] (str (.getSimpleName (class t)) ": " (.getMessage t)))]
    {:result (cond-> res
               (:throwable res) (update :throwable show)
               (:class res)     (update :class :adt/variant))
     :calls @calls
     :kicks @kicks
     :valid? (m/validate runner/Result res)}))

(deftest-golden run-scripted-thunks-golden
  "test/golden/hive-mcp/resilience/runner-run.edn"
  (into (sorted-map)
        {:ok-first            (scripted [42] true)
         :transient-recovers  (scripted [:transient :recovered] true)
         :transient-twice     (scripted [:transient] true)
         :transient-no-kick   (scripted [:transient :recovered] false)
         :transient-then-fatal (scripted [:transient :fatal] true)
         :schema-mismatch     (scripted [:schema] true)
         :fatal               (scripted [:fatal] true)
         :unknown             (scripted [:unknown] true)}))

(deftest only-transient-retries
  (doseq [[k script] {:schema [:schema] :fatal [:fatal] :unknown [:unknown]}]
    (is (= 1 (:calls (scripted script true))) (str k " runs the thunk once"))
    (is (zero? (:kicks (scripted script true))) (str k " never kicks"))))
