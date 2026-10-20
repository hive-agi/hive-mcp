(ns hive-mcp.tools.swarm.claim-contract-test
  "Contract facets for the claim handlers' store-free arm: a blank file path
   is refused with a JSON error before either claim store is read. The store
   paths need hive-datascript / hive-agent and are covered by test-swarm."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.swarm.claim :as cl]
            [hive-mcp.tools.swarm.core :as core]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema claim-clear-needs-a-file-path
  #'cl/handle-claim-clear
  {:in [:cat [:map [:file_path {:optional true} [:enum nil ""]]
              [:force {:optional true} :boolean]]]
   :out core/SwarmResponse
   :rel (fn [_ out] (= {"error" "file_path is required"} (json/read-str (:text out))))
   :num-tests 30})

(golden/deftest-golden claim-clear-refusal-golden
  "test/golden/hive-mcp/tools/swarm/claim-clear-refusal.edn"
  {:no-path    (cl/handle-claim-clear {})
   :blank-path (cl/handle-claim-clear {:file_path ""})})

(deftest the-refusal-is-not-flagged-as-a-tool-error
  (is (nil? (:isError (cl/handle-claim-clear {})))))
