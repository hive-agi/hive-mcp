(ns hive-mcp.tools.consolidated.hivemind-trifecta-test
  "Trifecta over hivemind/handle-nudge on its pure refusal arm: a missing or
   blank agent_id is refused with an mcp-error before the agent domain is
   resolved. The dispatch arm is pinned by hivemind-nudge-test."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.tools.consolidated.hivemind :as hm]
            [hive-mcp.tools.core :as tc]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta handle-nudge-refusal-contract
  hive-mcp.tools.consolidated.hivemind/handle-nudge
  {:golden-path "test/golden/tools/consolidated/hivemind-nudge-refusal.edn"
   :cases       {:no-id {} :nil nil :blank {:agent_id ""} :spaces {:agent_id "   "}
                 :message-only {:message "hello"}}
   :gen         (gen/fmap (fn [[ws msg]] {:agent_id ws :message msg})
                          (gen/tuple (gen/elements ["" " " "\t" "\n  "]) gen/string-ascii))
   :pred        #(and (true? (:isError %)) (m/validate tc/McpErrorResponse %))
   :num-tests   50
   :mutations   [["no-error-flag" (fn [p] (dissoc (hm/handle-nudge p) :isError))]
                 ["wrong-message" (fn [_] (tc/mcp-error "nope"))]]})

(deftest nudge-response-arms-discriminate
  (let [text-arm     {:type "text" :text "x" :isError true}
        envelope-arm {:content [{:type "text" :text "{}"}]}]
    (is (m/validate hm/NudgeResponse text-arm))
    (is (m/validate hm/NudgeResponse envelope-arm))
    (is (not (m/validate tc/McpResponse envelope-arm)))
    (is (not (m/validate hm/NudgeResponse {:content "not-a-seq"})))))
