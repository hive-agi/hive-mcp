(ns hive-mcp.tools.consolidated.hot-report-keys-test
  "What `hot reload` keeps from a RemountReport when it trims it for a terminal."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.consolidated.hot :as hot]))

(deftest summarize-keeps-the-reload-failure-and-quarantine-keys-test
  ;; hive-addon reports an addon whose code failed to load under
  ;; :hot/reload-failures, and a later reload it refused before loading under
  ;; :hot/quarantined. A terminal reader must see both, not only :errors.
  (let [failures {'demo.addon {:phase :ns-load :attempts 1}}
        quarantined [{:addon 'demo.addon :cause :reload/failed}]
        out (#'hot/summarize {:ok? false :hot/refused? true
                              :hot/reload-failures failures
                              :hot/quarantined quarantined
                              :hot/internal-noise :dropped})]
    (is (= failures (:hot/reload-failures out)))
    (is (= quarantined (:hot/quarantined out)))
    (is (not (contains? out :hot/internal-noise)))))
