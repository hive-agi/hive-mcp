(ns hive-mcp.launch.test-runner
  "Run every hive-mcp.launch test namespace under babashka; exit non-zero on failure."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :as t]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def test-dir "test/hive_mcp/launch")

(defn- test-file?
  "True for a *_test.clj or *_test.cljc file."
  [f]
  (let [n (str (fs/file-name f))]
    (or (str/ends-with? n "_test.clj")
        (str/ends-with? n "_test.cljc"))))

(defn- strip-suffix
  "`n` without its trailing `suffix`, or nil when it does not end with it."
  [n suffix]
  (when (str/ends-with? n suffix)
    (subs n 0 (- (count n) (count suffix)))))

(defn- ns-sym-for
  "The namespace symbol a test file declares: drop the extension, turn each
   underscore into a dash."
  [f]
  (let [n (str (fs/file-name f))
        base (or (strip-suffix n ".cljc") (strip-suffix n ".clj"))
        dashed (apply str (map (fn [c] (if (= "_" (str c)) "-" (str c))) base))]
    (symbol (str "hive-mcp.launch." dashed))))

(defn test-namespaces
  "The launch test namespaces, sorted."
  []
  (->> (fs/glob test-dir "*_test.clj*")
       (filter test-file?)
       (map ns-sym-for)
       sort
       vec))

(defn -main
  [& _args]
  (let [nss (test-namespaces)
        _ (run! require nss)
        summary (apply t/run-tests nss)
        bad (+ (:fail summary 0) (:error summary 0))]
    (println (str "launch tests: " (:test summary 0) " tests, "
                  (:pass summary 0) " assertions passed, "
                  (:fail summary 0) " failed, "
                  (:error summary 0) " errored"))
    (when (pos? bad)
      (System/exit 1))))

(m/=> -main [:=> [:cat [:* :string]] :any])