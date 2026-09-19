(ns hive-mcp.agent.ling.sandbox-ctx-test
  "A spawn's :sandbox reaches the headless backend's ctx unchanged, including
   an explicit false. The host does not interpret it: the backend resolves it
   against its own default, so the host only has to carry it faithfully."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.ling.lifecycle :as lifecycle]))

(deftest the-sandbox-flag-rides-from-ling-opts-to-strategy-ctx
  (testing "true and false both survive; false is an opt-out, not an absence"
    (doseq [v [true false]]
      (let [l (ling/->ling "sbx-ling" {:cwd "/w" :spawn-mode :hive-agent :sandbox v})]
        (is (= v (:sandbox l)))
        (is (= v (:sandbox (lifecycle/ling-ctx l)))))))
  (testing "a spawn that says nothing leaves the key out, so the backend default applies"
    (let [l (ling/->ling "plain-ling" {:cwd "/w" :spawn-mode :hive-agent})]
      (is (not (contains? (lifecycle/ling-ctx l) :sandbox))))))
