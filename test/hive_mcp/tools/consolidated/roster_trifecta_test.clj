(ns hive-mcp.tools.consolidated.roster-trifecta-test
  "Trifecta over roster/tool-names: the sorted union of consolidated and
   composite tool names."
  (:require [clojure.test.check.generators :as gen]
            [hive-mcp.tools.consolidated.roster :as roster]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta tool-names-contract
  hive-mcp.tools.consolidated.roster/tool-names
  {:golden-path "test/golden/tools/consolidated/roster-tool-names.edn"
   :apply?      true
   :cases       {:roster []}
   :gen         (gen/return [])
   :pred        (fn [names]
                  (and (vector? names)
                       (= names (sort names))
                       (= (set names) (into (set (map first roster/consolidated-tools))
                                            roster/composite-tools))))
   :num-tests   5
   :mutations   [["unsorted" (fn [] (vec (concat (map first roster/consolidated-tools)
                                                 roster/composite-tools)))]
                 ["drops-composites" (fn [] (vec (sort (map first roster/consolidated-tools))))]]})
