(ns hive-mcp.test-runner
  "Entry point of the default test tier (:test, :test-unit): cognitect's
   test-runner with the network tripwire (hive-mcp.test-support.net-guard)
   installed first. Every option is cognitect's own.

   After the run, any refused connect (a non-loopback host, or a loopback
   live-service port such as Chroma :8000 or the serving hive-mcp :7911)
   is printed with the frames that made it and the run exits non-zero, even
   when every assertion passed: the default tier does not reach live
   resources, so reaching one is a red run."
  (:require [cognitect.test-runner :as runner]
            [clojure.tools.cli :as cli]
            [hive-mcp.test-support.net-guard :as guard]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- report-violations! [vs]
  (println)
  (println (str "NETWORK TRIPWIRE: the default tier reached " (count vs)
                " live or non-loopback target(s). Move the test onto a stub, or"
                " tag it ^:e2e under test-e2e/ with an explicit sandbox."))
  (doseq [[uri group] (group-by :uri vs)]
    (println (str "  " uri " " (:reason (first group)) " x" (count group)))
    (doseq [f (:frames (first group))] (println (str "      at " f)))))

(defn -main
  "cognitect.test-runner/-main, plus the tripwire verdict."
  [& args]
  (guard/install!)
  (let [args   (cli/parse-opts args runner/cli-options)
        _      (when (:errors args)
                 (run! println (:errors args))
                 (System/exit 1))
        result (runner/test (:options args))
        vs     (guard/violations-seen)]
    (when (seq vs) (report-violations! vs))
    (shutdown-agents)
    (System/exit (if (and (zero? (+ (:fail result 0) (:error result 0))) (empty? vs)) 0 1))))
