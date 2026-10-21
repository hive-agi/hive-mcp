(ns hive-mcp.tools.session-complete-contract-test
  "Contract facets for session_complete. Only the validation arm is driven:
   the success arm dispatches a :git-commit effect. The trifecta points at
   the contracted handler; the config and registration fns are pinned by a
   golden and checked against their schemas."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.result-bridge :as rb]
            [hive-mcp.tools.session-complete :as sc]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema session-complete-validates-commit-msg
  #'sc/handle-session-complete
  {:in [:cat [:map
              [:commit_msg {:optional true} [:enum "" " " "\t\n"]]
              [:task_ids {:optional true} [:vector [:enum "t1" "t2"]]]]]
   :out rb/McpResponse
   :rel (fn [[p] out]
          (and (true? (:isError out))
               (= (if (contains? p :commit_msg)
                    "commit_msg cannot be empty"
                    "Missing required field: commit_msg")
                  (:text out))))
   :classify (fn [[p] _] (if (contains? p :commit_msg) :blank :missing))
   :classify-domain #{:blank :missing}
   :golden-path "test/golden/hive-mcp/tools/session-complete-validation.edn"
   :num-tests 60})

(golden/deftest-golden session-config-golden
  "test/golden/hive-mcp/tools/session-complete-config.edn"
  {:config     (sc/get-config)
   :merge-noop (sc/set-config! {})
   :nil-params (sc/handle-session-complete nil)})

(deftest config-and-registration-hold-their-contracts
  (is (true? (sc/register-handler!)))
  (is (m/validate sc/SessionConfig (sc/set-config! {})))
  (is (contains? #{true false nil} (sc/reset-registration!)))
  (is (true? (sc/register-handler!))))
