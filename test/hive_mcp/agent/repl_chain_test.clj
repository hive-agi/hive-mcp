(ns hive-mcp.agent.repl-chain-test
  "The :repl chain: a headless nREPL registers in the JVM-side registry with
   no Emacs, and the chain falls to :emacs-cider only when it cannot start."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-dsl.result :as r]
            [hive-mcp.agent.repl-chain :as rc]
            [hive-mcp.transport.registry :as reg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f]
    (reg/clear!)
    (try (f) (finally (reg/clear!) (run! rc/stop! (keys (rc/sessions)))))))

(def ^:private fake-argv
  ["sh" "-c" "echo 'nREPL server started on port 40123 on host 127.0.0.1'; sleep 30"])

(deftest argv-and-banner
  (is (= ["clojure" "-M:dev-repl"] (rc/command-argv nil)))
  (is (= ["clojure" "-M:test:dev-repl"] (rc/command-argv ["test" "dev-repl"])))
  (is (= 44293 (rc/parse-port "nREPL server started on port 44293 on host 127.0.0.1")))
  (is (nil? (rc/parse-port "WARNING: something")))
  (is (nil? (rc/parse-port nil))))

(deftest start-registers-without-emacs
  (let [out (rc/start! {:name "t1" :dir "/tmp" :argv fake-argv :timeout-ms 10000})
        s   (:ok out)]
    (is (r/ok? out))
    (is (= 40123 (:port s)))
    (is (= s (rc/session "t1")))
    (is (= {:stopped? true} (rc/stop! "t1")))
    (is (nil? (rc/session "t1")))))

(deftest no-banner-is-unavailable-and-not-registered
  (let [out (rc/start! {:name "t2" :dir "/tmp" :argv ["sh" "-c" "echo hi"] :timeout-ms 5000})]
    (is (= :unavailable (:error/kind out)))
    (is (nil? (rc/session "t2")))))

(deftest chain-prefers-headless-and-names-fallback-when-it-fails
  (reg/rebind! {:repl {:chain [:headless-nrepl :emacs-cider]}})
  (testing "headless adapter failing falls to emacs-cider, which says why"
    (let [out (rc/spawn-repl! {:name "t3" :dir "/nonexistent-dir-xyz"})]
      (is (contains? out :error))
      (is (= [:headless-nrepl] (mapv :adapter (take 1 (:skipped out))))))))
