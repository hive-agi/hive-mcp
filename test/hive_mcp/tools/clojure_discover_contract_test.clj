(ns hive-mcp.tools.clojure-discover-contract-test
  "Contract facets for the inline nREPL discovery. The wrapped handler is
   driven only for non-discover commands, whose answer depends on whether
   basic-tools-mcp is on the classpath and not on the filesystem. install!
   writes the extension registry and is not driven."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.clojure-discover :as cd]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private upstream?
  (some? (try (requiring-resolve 'basic-tools-mcp.tools/handle-clojure)
              (catch Throwable _ nil))))

(golden/deftest-golden missing-upstream-golden
  "test/golden/hive-mcp/tools/clojure-discover-missing.edn"
  (if upstream?
    :upstream-present
    ((cd/wrap-handle-clojure) {:command "eval"})))

(deftest scans-answer-their-contracted-shapes
  (is (m/validate [:vector cd/Dir] (cd/candidate-dirs)))
  (is (apply distinct? (map #(.getCanonicalPath ^java.io.File %) (cd/candidate-dirs))))
  (let [ports (cd/discover-ports)]
    (is (m/validate [:vector cd/DiscoveredPort] ports))
    (is (= (count ports) (count (distinct (map :port ports))))))
  (let [r ((cd/wrap-handle-clojure) {:command "discover"})]
    (is (m/validate cd/ClojureToolResponse r))
    (is (str/includes? (get-in r [:content 0 :text]) ":count"))))
