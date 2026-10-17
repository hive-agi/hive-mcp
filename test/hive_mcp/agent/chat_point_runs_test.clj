(ns hive-mcp.agent.chat-point-runs-test
  "Pure calculations over chat-point run rows, and the hive-agent adapter
   driven through an injected store-ops map."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-mcp.agent.chat-point-runs :as cpr]))

(def ^:private rows
  [{:run-id "r1" :agent-id "lz-a" :created 100}
   {:run-id "r2" :agent-id "lz-a" :created 300}
   {:run-id "r3" :agent-id "lz-b" :created 200}
   {:run-id "r4" :created 400}])

(deftest latest-run-by-agent-keeps-the-newest-run-of-each-agent
  (is (= {"lz-a" "r2" "lz-b" "r3"} (cpr/latest-run-by-agent rows)))
  (is (= {} (cpr/latest-run-by-agent []))))

(deftest ended-is-an-assistant-text-reply-with-no-pending-tool-calls
  (is (true? (cpr/ended? {:role "assistant" :content "done"})))
  (is (true? (cpr/ended? {:role :assistant :content "done" :tool_calls []})))
  (is (false? (cpr/ended? {:role "assistant" :content "" :tool_calls [{:id "1"}]})))
  (is (false? (cpr/ended? {:role "tool" :content "ok"})))
  (is (false? (cpr/ended? {:role "user" :content "go"})))
  (is (false? (cpr/ended? nil))))

(deftest run-row-normalizes-a-hive-agent-run
  (is (= {:run-id "r9" :agent-id "lz-c" :project-id "hive-mcp" :model "m"
          :created 5 :parent-point {:run-id "r1" :seq 3 :turn 2}}
         (cpr/run-row {:run/id "r9" :run/task "t"
                       :run/parent {:run-id "r1" :seq 3 :turn 2}
                       :run/meta {:agent-id "lz-c" :project-id "hive-mcp" :model "m" :created 5}})))
  (testing "a fresh run has no parent point and absent meta stays nil"
    (is (= {:run-id "r0" :agent-id nil :project-id nil :model nil :created nil :parent-point nil}
           (cpr/run-row {:run/id "r0"})))))

(defn- fake-ops
  "Store ops over a map of run-id -> {:run Run :lineage [Message]}."
  [runs]
  {:store   (constantly ::store)
   :list    (fn [store] (assert (= ::store store)) (mapv :run (vals runs)))
   :lineage (fn [_ id] (get-in runs [id :lineage]))})

(deftest hive-agent-index-reads-runs-and-tips-through-the-store-ops
  (let [idx (cpr/hive-agent-run-index
             (fake-ops {"r1" {:run {:run/id "r1" :run/meta {:agent-id "lz-a" :created 7}}
                              :lineage [{:seq 0 :turn 0 :message {:role "user" :content "go"}}
                                        {:seq 1 :turn 1 :message {:role "assistant" :content "ok"}}]}}))]
    (is (= [{:run-id "r1" :agent-id "lz-a" :project-id nil :model nil :created 7 :parent-point nil}]
           (:ok (cpr/list-runs idx))))
    (is (= {:seq 1 :turn 1 :message {:role "assistant" :content "ok"}}
           (:ok (cpr/run-tip idx "r1"))))
    (is (r/ok? (cpr/run-tip idx "nope")))
    (is (nil? (:ok (cpr/run-tip idx "nope"))))))

(deftest hive-agent-index-is-empty-when-hive-agent-is-absent
  (let [idx (cpr/hive-agent-run-index nil)]
    (is (= [] (:ok (cpr/list-runs idx))))
    (is (r/ok? (cpr/run-tip idx "r1")))))

(deftest a-throwing-store-is-an-err-result
  (let [idx (cpr/hive-agent-run-index {:store   (fn [] (throw (ex-info "boom" {})))
                                       :list    (constantly [])
                                       :lineage (constantly [])})]
    (is (not (r/ok? (cpr/list-runs idx))))))
