(ns hive-mcp.tools.consolidated.hot-reload-force-test
  "`hot reload force=true` asks the bridge to re-read the addon's source roots
   even when disk is unchanged."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.consolidated.hot :as hot]))

(def ^:private base {:mount-opts {:resolve-config ::cfg}})

(deftest force-asks-the-bridge-to-re-read-and-to-run-the-namespace-pass
  (testing "boolean and string true both force"
    (doseq [f [true "true"]]
      (let [opts (hot/reload-call-opts base f)]
        (is (true? (:force? opts)))
        (is (false? (:ns-reloaded? opts))
            "a namespace-seeded reload must not take the callback's already-loaded path")
        (is (= (:mount-opts base) (:mount-opts opts))))))
  (testing "absent or false leaves the opts untouched"
    (doseq [f [nil false "false"]]
      (is (= base (hot/reload-call-opts base f))))))

(deftest the-tool-advertises-force-for-reload
  (let [desc (get-in hot/tool-def [:inputSchema :properties "force" :description])]
    (is (str/includes? desc "[reload]"))))
