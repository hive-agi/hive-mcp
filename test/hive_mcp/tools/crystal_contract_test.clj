(ns hive-mcp.tools.crystal-contract-test
  "Contract facets for the crystal wrap tools' pure surface: the wrap-notify
   payload builder and the explicit-agent arm of resolve-agent."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.crystal :as crystal]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:enum "m1" "m2" "s1" "s2" nil])

(def ^:private Harvested
  [:map [:memory-ids-created {:optional true} [:vector {:max 3} [:map [:id Id]]]]])

(def ^:private CrResult
  [:map
   [:session {:optional true} [:enum "sess"]]
   [:summary-id {:optional true} Id]
   [:sub-summaries {:optional true} [:vector {:max 3} [:map [:summary-id Id]]]]])

(deftrifecta-from-schema wrap-notify-counts-distinct-created-ids
  #'crystal/wrap-notify-data
  {:in [:cat [:enum "ling-1"] Harvested CrResult [:enum "proj"] [:map [:x {:optional true} :int]]]
   :out crystal/WrapNotify
   :rel (fn [[_ h cr _ stats] out]
          (let [ids (->> (concat (map :id (:memory-ids-created h))
                                 (map :summary-id (or (:sub-summaries cr) [cr])))
                         (remove nil?) distinct vec)]
            (and (= ids (:created-ids out))
                 (= (assoc stats :wrapped (count ids)) (:stats out))
                 (= (:session cr) (:session-id out)))))
   :golden-path "test/golden/hive-mcp/tools/crystal-wrap-notify.edn"
   :num-tests 200})

(deftest explicit-agent-id-wins
  (is (= "ling-7" (crystal/resolve-agent {:agent_id "ling-7"}))))
