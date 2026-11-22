(ns hive-mcp.embeddings.availability.multi-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.embeddings.availability.methods :as methods]
            [hive-mcp.embeddings.availability.multi :as multi]
            [hive-mcp.embeddings.availability.schema :as schema]
            [hive-spi.log.ports :as log-ports]
            [hive-spi.schema.registry :as reg]))

(defn- capture-logger [sink]
  (reify log-ports/ILogger
    (log-event [_ level message] (swap! sink conj [level message]) nil)
    (logger-levels [_] #{:debug :info :warn :error})))

(use-fixtures :each (fn [t] (try (t) (finally (log-ports/clear-logger!)))))

(deftest secret-available?-contract
  (let [base {:impl :y}]
    (testing "declared variants are registered"
      (is (contains? (methods multi/secret-available?) :ollama))
      (is (contains? (methods multi/secret-available?) :openrouter))
      (is (contains? (methods multi/secret-available?) :openai))
      (is (contains? (methods multi/secret-available?) :venice)))
    (testing "LSP: implemented variants satisfy the out contract"
      (doseq [k [:ollama :openrouter :openai :venice]]
        (let [res (try (multi/secret-available? (assoc base :impl k))
                       (catch clojure.lang.ExceptionInfo _ ::stub))]
          (is (or (= ::stub res) (reg/validate :boolean res)) (str k)))))))

(deftest an-unknown-impl-is-unavailable
  (let [sink (atom [])]
    (log-ports/set-logger! (capture-logger sink))
    (testing "dispatch totality: an unknown impl answers false, never a truthy value"
      (let [res (multi/secret-available? {:impl ::unknown})]
        (is (false? res))
        (is (reg/validate :boolean res) "the :default branch keeps the out contract")))
    (testing "a spec with no :impl at all is unavailable too"
      (is (false? (multi/secret-available? {}))))
    (testing "callers filtering on the answer drop the unknown impl"
      (is (= [:ollama]
             (->> [{:impl :ollama} {:impl ::unknown}]
                  (filter multi/secret-available?)
                  (mapv :impl)))))
    (testing "the fall-through is logged at :warn through the log port"
      (let [[level msg] (first @sink)]
        (is (= :warn level))
        (is (str/includes? msg (pr-str ::unknown)))))))
