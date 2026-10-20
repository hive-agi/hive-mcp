(ns hive-mcp.tools.migration-contract-test
  "Contract facets for the migration tool's dispatcher on the commands that
   touch no store: help, adapters, an unknown command, and the refusals of
   commands whose required param is missing."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.migration :as mig]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private StorelessCommand [:enum nil "help" "zzz" "adapters" "HELP"])

(deftrifecta-from-schema dispatch-falls-back-to-help
  #'mig/handle-migration
  {:in [:cat [:map [:command StorelessCommand]]]
   :out :map
   :rel (fn [[{:keys [command]}] out]
          (if (= "adapters" command)
            (contains? out :adapters)
            (= (mig/cmd-help {}) out)))
   :classify (fn [_ out] (if (contains? out :adapters) :adapters :help))
   :classify-domain #{:adapters :help}
   :num-tests 100})

(defn- refused
  "Run f; a throw (malli :invalid-input or the fn's own ex-info) is :refused."
  [f]
  (try (f) (catch Throwable _ :refused)))

(golden/deftest-golden migration-storeless-golden
  "test/golden/hive-mcp/tools/migration-storeless.edn"
  ;; The adapter registry is process-global and other suites register their
  ;; own adapters into it, so only the built-in ones are snapshotted.
  {:help-commands (mapv :command (:commands (mig/cmd-help {})))
   :adapters      (vec (sort (filter #{:edn :json} (map :id (:adapters (mig/cmd-adapters {}))))))
   :refusals      {:switch   (refused #(mig/cmd-switch {}))
                   :sync     (refused #(mig/cmd-sync {}))
                   :validate (refused #(mig/cmd-validate {}))
                   :export   (refused #(mig/cmd-export {}))
                   :import   (refused #(mig/cmd-import {}))}})

(deftest help-validates-its-own-schema
  (is (m/validate mig/HelpResult (mig/cmd-help nil)))
  (is (not (m/validate mig/HelpResult {:commands [{:command "x"}]}))))
