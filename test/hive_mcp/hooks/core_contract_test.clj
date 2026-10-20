(ns hive-mcp.hooks.core-contract-test
  "Contract facets for the hook registry, and the coverage gate for
   src/hive_mcp/hooks. Every registry is built by the test; no shared state."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.hooks.core :as hooks]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage hooks-contracts-gate
  {:paths ["src/hive_mcp/hooks"]
   :exempt {}})

(deftest every-hooks-contract-generates-both-sides
  (doseq [[fn-sym {:keys [schema]}] (get (m/function-schemas) 'hive-mcp.hooks.core)
          arity (let [s (m/schema schema)] (if (= :function (m/type s)) (m/children s) [s]))
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str fn-sym " " (pr-str (m/form side))))))

(deftrifecta-from-schema create-registry-has-an-empty-slot-per-event
  #'hooks/create-registry
  {:in [:cat]
   :out hooks/HookRegistry
   :rel (fn [_ out] (= (zipmap hooks/hook-events (repeat [])) @out))
   :num-tests 10})

(deftrifecta-from-schema register-hook-appends-the-handler
  #'hooks/register-hook
  {:in [:cat hooks/HookRegistry hooks/HookEvent hooks/Handler]
   :out :nil
   :rel (fn [[reg event handler] out]
          (and (nil? out) (identical? handler (peek (get @reg event)))))})

(deftrifecta-from-schema trigger-hooks-answers-one-result-per-handler
  #'hooks/trigger-hooks
  {:in [:cat hooks/HookRegistry hooks/HookEvent [:map-of :keyword :int]]
   :out [:vector :any]
   :rel (fn [[reg event _] out] (= (count (get @reg event)) (count out)))})

(golden/deftest-golden trigger-order-and-safe-failure-golden
  "test/golden/hive-mcp/hooks/trigger.edn"
  (let [reg (hooks/create-registry)
        seen (atom [])]
    (hooks/register-hook reg :task-complete (fn [ctx] (swap! seen conj [:first ctx]) :one))
    (hooks/register-hook reg :task-complete (fn [_] (throw (ex-info "handler broke" {}))))
    (hooks/register-hook reg :task-complete (fn [ctx] (swap! seen conj [:third ctx]) (:task-id ctx)))
    (let [results (hooks/trigger-hooks reg :task-complete {:task-id "t-1"})]
      {:results (mapv #(if (and (map? %) (:error %)) {:error (ex-message (:error %))} %) results)
       :seen @seen
       :untouched (hooks/trigger-hooks reg :session-end {})})))

(deftest invalid-events-and-handlers-are-refused
  (let [reg (hooks/create-registry)]
    (testing "an unknown event is refused on register and on trigger"
      (is (thrown? clojure.lang.ExceptionInfo (hooks/register-hook reg :no-such-event identity)))
      (is (thrown? clojure.lang.ExceptionInfo (hooks/trigger-hooks reg :no-such-event {}))))
    (testing "a var is an invocable handler"
      (is (nil? (hooks/register-hook reg :file-changed #'identity)))
      (is (= [{:f 1}] (hooks/trigger-hooks reg :file-changed {:f 1}))))))
