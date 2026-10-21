(ns hive-mcp.tools.swarm.jvm-contract-test
  "Contract facets for the JVM-cleanup shims: they delegate to hive-agent's
   handlers; without hive-agent on the classpath every call raises the
   delegate's directed error naming the missing addon, never a bare NPE."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.swarm.jvm :as jvm]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private agent-present?
  (some? (try (requiring-resolve 'hive-agent.swarm.tools.jvm/handle-jvm-cleanup)
              (catch Throwable _ nil))))

(defn shim-outcome
  "Subject: :answered when the shim returns, else the error's addon hint."
  [[which params]]
  (try ((if (= which :cleanup) jvm/handle-jvm-cleanup jvm/handle-resource-guard) params)
       :answered
       (catch Exception e
         (if (str/includes? (str (ex-message e)) "hive-agent") :needs-hive-agent :other))))

(deftrifecta-from-schema shims-name-the-missing-addon
  #'shim-outcome
  {:in [:tuple [:enum :cleanup :guard] [:map [:dry_run {:optional true} [:= true]]]]
   :out [:enum :answered :needs-hive-agent :other]
   :rel (fn [_ out] (= out (if agent-present? :answered :needs-hive-agent)))
   ;; mutation off: with no hive-agent every input reaches one arm, so the
   ;; schema-derived mutants of this enum cannot be told apart by input.
   :mutation false
   :num-tests 10})

(golden/deftest-golden jvm-shims-golden
  "test/golden/hive-mcp/tools/swarm/jvm-shims.edn"
  {:agent-present agent-present?
   :cleanup (if agent-present? :answered (shim-outcome [:cleanup {:dry_run true}]))
   :guard   (if agent-present? :answered (shim-outcome [:guard {}]))})

(deftest shims-keep-their-documented-arglists
  (is (= 1 (count (:arglists (meta #'jvm/handle-jvm-cleanup))))))
