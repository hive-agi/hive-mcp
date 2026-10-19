(ns hive-mcp.batch.cli-adapter-contract-test
  "`hive-mcp.batch.cli-adapter/cli-batch-handler` against its contract, with
   the Batchable runner injected as a stub fn (the port), and a golden over
   the legacy envelopes it renders."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.batch.cli-adapter :as cli]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- stub-runner
  "A Batchable run-fn stand-in: records the ops it saw and answers a two-wave
   result with one success, one data-only success and one failure."
  [seen]
  (fn [ops _opts]
    (reset! seen ops)
    {:success false
     :waves   {2 {:results [{:success false :error "boom"}]}
               1 {:results [{:success true :result {:id 1}}
                            {:success true :data {:id 2}}]}}
     :summary {:total 3 :success 2 :failed 1 :waves 2}
     :errors  ["one op failed"]}))

(def ^:private Envelope
  (-> (m/function-schemas)
      (get-in ['hive-mcp.batch.cli-adapter 'cli-batch-handler :schema])
      m/children second m/children second))

(defn- run [params]
  (let [seen (atom nil)
        h    (cli/cli-batch-handler {:run-fn (stub-runner seen) :cmd-kw :add :tool-name "memory add"})
        out  (h params)]
    {:out out :seen @seen}))

(deftest handler-contract-test
  (testing "the built handler answers the legacy envelope"
    (let [{:keys [out seen]} (run {:operations [{:command "smuggled" :content "x"} {:content "y"}]})
          body (json/read-str (:text out) :key-fn keyword)]
      (is (m/validate Envelope out))
      (testing "per-op :command is forced and :tool is injected"
        (is (= ["add" "add"] (mapv :command seen)))
        (is (= ["memory add" "memory add"] (mapv :tool seen))))
      (testing "waves are flattened in wave order; :data stands in for a missing :result"
        (is (= [true true false] (mapv :success (:results body))))
        (is (= [{:id 1} {:id 2}] (mapv :result (take 2 (:results body)))))
        (is (= "boom" (:error (last (:results body))))))
      (is (= {:total 3 :success 2 :failed 1} (:summary body)) ":waves is not projected")
      (is (= ["one op failed"] (:errors body)))))
  (testing "no operations is an MCP error, and the runner is never called"
    (doseq [params [{} {:operations []} {:operations nil}]]
      (let [{:keys [out seen]} (run params)]
        (is (m/validate Envelope out))
        (is (true? (:isError out)))
        (is (nil? seen)))))
  (testing "tool-name defaults to the command name"
    (let [seen (atom nil)
          h    (cli/cli-batch-handler {:run-fn (stub-runner seen) :cmd-kw :edge})]
      (h {:operations [{}]})
      (is (= "edge" (:tool (first @seen)))))))

(deftest handler-spec-is-refused-test
  ;; the body asserts; under instrumentation the contract refuses first, so
  ;; either refusal is the expected outcome
  (is (thrown? Throwable (cli/cli-batch-handler {:run-fn nil :cmd-kw :add})))
  (is (thrown? Throwable (cli/cli-batch-handler {:run-fn identity :cmd-kw "add"}))))

(golden/deftest-golden-fn cli-envelope-golden
  "test/golden/batch/cli-envelope.edn"
  (fn []
    (into (sorted-map)
          (for [[label params] {:two-ops {:operations [{:a 1} {:b 2}]}
                                :empty   {:operations []}}]
            [label (:out (run params))]))))

(deftest cli-adapter-schemas-generate-test
  (doseq [s [cli/CliEnvelope cli/HandlerSpec]]
    (is (seq (mg/sample s {:size 4})) (pr-str s))))
