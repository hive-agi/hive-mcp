(ns hive-mcp.system.keepalive-mode-test
  "Which wait the keepalive chooses, from the merged :hive/keepalive config.

   system.edn sets {:mode :stdio}; a headless profile only ADDS
   {:signal :sigterm}. The deep merge therefore always carries :mode :stdio,
   and a resolver that read :mode alone kept every headless profile blocking
   on a stdio client that does not exist (kanban 20260902171631-4bd9a6e3)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.server.core :as core]
            [hive-mcp.system.keepalive :as keepalive]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta keepalive-resolve-mode
  keepalive/resolve-mode
  {:golden-path "test/golden/hive-mcp/system/keepalive-resolve-mode.edn"
   :cases {:desktop          {:mode :stdio}
           :k8s-merged       {:mode :stdio :signal :sigterm}
           :signal-only      {:signal :sigterm}
           :explicit-promise {:mode :promise}
           :empty            {}}
   :gen (gen/hash-map :mode (gen/elements [nil :stdio :promise])
                      :signal (gen/elements [nil :sigterm]))
   :pred #{:stdio :promise}
   :num-tests 50
   :mutations [["reads :mode only" (fn [{:keys [mode]}] (or mode :stdio))]
               ["always :stdio" (constantly :stdio)]]
   :assert (fn []
             (is (= :stdio (keepalive/resolve-mode {:mode :stdio})))
             (is (= :promise (keepalive/resolve-mode {:mode :stdio :signal :sigterm}))
                 ":signal :sigterm selects :promise even over the base :mode :stdio")
             (is (= :stdio (keepalive/resolve-mode {}))))})

(deftest headless-profiles-resolve-to-promise
  (testing "desktop keeps the stdio wait"
    (is (= :stdio (keepalive/resolve-mode
                   (get (core/load-system-config :desktop) :hive/keepalive)))))
  (doseq [profile [:k8s-headless :k8s-minimal :bare]]
    (testing (str profile " declares {:signal :sigterm}, so it waits on the promise")
      (is (= :promise (keepalive/resolve-mode
                       (get (core/load-system-config profile) :hive/keepalive)))))))
