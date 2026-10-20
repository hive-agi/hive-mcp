(ns hive-mcp.system.layer3-contract-test
  "Contract facet for hive-mcp.system.layer3: discovery-mode is pure."
  (:require [hive-mcp.system.layer3 :as layer3]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema discovery-mode-defaults-to-classpath
  #'layer3/discovery-mode
  {:in [:cat [:maybe [:map [:discover {:optional true} [:enum :classpath :none :other]]]]]
   :out :keyword
   :rel (fn [[config] out] (= out (get config :discover :classpath)))
   :golden-path "test/golden/hive-mcp/system/discovery-mode.edn"
   :num-tests 100})
