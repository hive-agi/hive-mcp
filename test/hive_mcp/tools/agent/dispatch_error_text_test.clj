(ns hive-mcp.tools.agent.dispatch-error-text-test
  "A refused dispatch answers the root cause's words. Ling dispatch! re-throws
   every strategy failure as \"Failed to dispatch to ling\" with the cause's
   message under ex-data :error and the cause chained; the MCP error text
   must carry that cause, not only the wrapper."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.tools.agent.dispatch :as dispatch]))

(def ^:private refusal
  (str "Ling lz-polish has finished its run run-lz-1; a message does not restart it."
       " To continue that run, its spawning parent calls `agent spawn` with"
       " resume={run_id: run-lz-1} under a new name."))

(defn- ling-wrapper
  "The exception Ling dispatch! throws for a strategy failure CAUSE."
  [cause]
  (ex-info "Failed to dispatch to ling"
           {:ling-id "lz-polish" :task-id "task-1" :error (ex-message cause)}
           cause))

(deftest the-root-cause-reaches-the-mcp-error-text
  (let [text (dispatch/dispatch-error-text
              (ling-wrapper (ex-info refusal {:error :ling/run-finished})))]
    (is (str/starts-with? text "Failed to dispatch: Failed to dispatch to ling: "))
    (is (str/includes? text "has finished its run run-lz-1"))
    (is (str/includes? text "resume={run_id: run-lz-1}"))))

(deftest the-cause-chain-is-read-when-ex-data-has-no-error
  (let [text (dispatch/dispatch-error-text
              (ex-info "Failed to dispatch to ling" {} (RuntimeException. "backend down")))]
    (is (= "Failed to dispatch: Failed to dispatch to ling: backend down" text))))

(deftest a-plain-exception-keeps-its-own-message
  (testing "no cause: the message alone"
    (is (= "Failed to dispatch: boom" (dispatch/dispatch-error-text (RuntimeException. "boom")))))
  (testing "a cause already in the message is not repeated"
    (is (= "Failed to dispatch: refused: x"
           (dispatch/dispatch-error-text (ex-info "refused: x" {:error "x"}))))))
