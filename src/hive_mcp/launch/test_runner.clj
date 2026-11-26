(ns hive-mcp.launch.test-runner
  "Run the `:launch` test namespaces under babashka and exit non-zero on
   failure. Invoked by the `bb test:launch` task.

   Deliberately narrow: this is the FAST layer that LC0..LC7 run while
   iterating on launch config, so it must not pull a JVM, a backend driver or
   the server. The JVM suite (`clojure -M:test-unit`) still runs these same
   namespaces, because they are ordinary clojure.test namespaces in test/.

   Why a namespace instead of a shell one-liner: the exit status is the whole
   contract of this task, CI and a person both read it, and a runner can be
   READ and tested. It also lets babashka report a fixture failure (:error)
   distinctly from a red assertion."
  (:require [clojure.test :as t]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def test-namespaces
  "The launch namespaces `bb test:launch` runs, in dependency order."
  '[hive-mcp.launch.schema-test
    hive-mcp.launch.knobs-test])

(defn -main
  "Require each namespace, run it, print the summary, and exit 1 when any
   assertion failed or errored. A namespace that fails to LOAD also exits 1:
   an unloadable test is a red test, never a skipped one."
  [& _args]
  (let [loaded (reduce (fn [acc ns-sym]
                         (try
                           (require ns-sym)
                           (conj acc ns-sym)
                           (catch Throwable e
                             (binding [*out* *err*]
                               (println "FAILED to load" ns-sym "-"
                                        (.getSimpleName (class e)) (.getMessage e)))
                             acc)))
                       []
                       test-namespaces)
        summary (apply t/run-tests loaded)
        bad (+ (:fail summary 0) (:error summary 0))]
    (println (str "launch tests: " (:test summary 0) " tests, "
                  (:pass summary 0) " assertions passed, "
                  (:fail summary 0) " failed, "
                  (:error summary 0) " errored"))
    (when (or (pos? bad) (not= (count loaded) (count test-namespaces)))
      (System/exit 1))))

(m/=> -main [:=> [:cat [:* :string]] :any])