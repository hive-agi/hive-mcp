(ns hive-mcp.tools.consolidated.session-config-trifecta-test
  "Schema-derived trifecta over session-config/resolve!: an override map whose
   :swarm-slave-id is a string or nil always resolves, and an override wins
   over the environment."
  (:require [hive-mcp.tools.consolidated.session-config :as sc]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Overrides
  "Override maps that pin the slave-id, so the result never reads the env.
   Blank strings are excluded: defconfig normalizes \"\" to nil."
  [:map [:swarm-slave-id [:maybe [:string {:min 1}]]]])

(deftrifecta-from-schema resolve!-contract
  hive-mcp.tools.consolidated.session-config/resolve!
  {:in          Overrides
   :out         sc/ResolvedSessionToolConfig
   :rel         (fn [in out] (= (:swarm-slave-id in) (:swarm-slave-id out)))
   :golden-path "test/golden/tools/consolidated/session-config-resolve.edn"
   :num-tests   100})
