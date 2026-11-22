(ns hive-mcp.embeddings.resilient-stages-test
  "Trifectas over the pure stages `run-chain` is composed of: the chain-step
   decision table and the two exhaustion errors. Circuit, retry and deadline
   behaviour end to end stay pinned by the resilient-* suites."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-mcp.embeddings.resilient :as res]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- step [entry failures skipped forced? expired?]
  (#'res/chain-step entry failures skipped forced? expired?))

(def ^:private e {:provider-key :p})

(deftrifecta chain-step
  hive-mcp.embeddings.resilient-stages-test/step
  {:golden-path "test/golden/hive-mcp/embeddings/resilient-chain-step.edn"
   :apply? true
   :cases {:attempt          [e [] [] false false]
           :all-skipped      [nil [] [e] false false]
           :skipped-forced   [nil [] [e] true false]
           :failed-and-done  [nil [{:error :x}] [e] false false]
           :expired-mid      [e [] [] false true]
           :expired-at-end   [nil [] [e] false true]
           :empty-chain      [nil [] [] false false]}
   :gen (gen/tuple (gen/elements [nil e])
                   (gen/elements [[] [{:error :x}]])
                   (gen/elements [[] [e]])
                   gen/boolean gen/boolean)
   :pred #{:retry-skipped :providers-exhausted :deadline :attempt}
   :num-tests 100
   :mutations [["never-retries" (fn [en f s fo ex] (if (= :retry-skipped (step en f s fo ex))
                                                     :providers-exhausted
                                                     (step en f s fo ex)))]
               ["ignores-deadline" (fn [en _ _ _ _] (if en :attempt :providers-exhausted))]]})

(defn- error-shape [^clojure.lang.ExceptionInfo ex]
  {:message (ex-message ex) :data (ex-data ex)})

(deftest exhaustion-errors-name-every-provider
  (let [fail {:provider :a :model "m" :elapsed-ms 5 :budget-ms 10 :error :weave/timeout}
        d (error-shape (#'res/deadline-exhausted 20 15 [{:provider-key :b}] [fail]))
        p (error-shape (#'res/providers-exhausted 7 [fail] []))]
    (is (= {:message (str "Embedding chain ran out of time after 20ms (total budget 15ms)"
                          " — :a (model m) :weave/timeout after 5ms of 10ms — never tried: :b")
            :data {:error :embedder/chain-exhausted :exhausted-by :deadline :elapsed-ms 20
                   :total-budget-ms 15 :untried [:b] :failures [fail]}}
           d))
    (is (= {:message "All embedding providers in the chain failed after 7ms — :a (model m) :weave/timeout after 5ms of 10ms"
            :data {:error :embedder/chain-exhausted :exhausted-by :providers :elapsed-ms 7
                   :failures [fail] :untried []}}
           p))))

(defn- failing-provider [calls]
  (reify proto/EmbeddingProvider
    (embed-text [_ _] (swap! calls inc) (throw (ex-info "down" {})))
    (embed-batch [this ts] (mapv #(proto/embed-text this %) ts))
    (embedding-dimension [_] 3)))

(deftest all-failing-chain-throws-providers-exhausted
  (let [calls (atom 0)
        emb (res/resilient-embedder
             [{:provider (failing-provider calls) :provider-key :a}
              {:provider (failing-provider calls) :provider-key :b}]
             {:health (atom {}) :warmth (atom #{}) :clock (constantly 0)
              :cooldown-ms 1000 :budget-ms 1000 :cold-budget-ms 1000 :total-budget-ms 10000})
        data (try (proto/embed-text emb "x") nil (catch clojure.lang.ExceptionInfo ex (ex-data ex)))]
    (is (= :providers (:exhausted-by data)))
    (is (= [:a :b] (mapv :provider (:failures data))))
    (is (= 2 @calls))))
