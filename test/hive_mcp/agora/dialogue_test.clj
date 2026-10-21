(ns hive-mcp.agora.dialogue-test
  "Trifecta on the one part of `hive-mcp.agora.dialogue` that does not reach
   the agora store: `format-signal`, the `[SIGNAL: x]` prefix every turn
   shorthand (propose / counter / approve / no-change) writes. The rest of
   the namespace reads and writes dialogues through `hive-mcp.agora.schema`,
   a shim over hive-agent; its suites live in test-swarm/."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.agora.dialogue :as dialogue]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema format-signal-trifecta
  hive-mcp.agora.dialogue/format-signal
  {:in          [:cat [:or [:enum :propose :counter :approve :no-change :defer :ns/qualified]
                       [:enum "approve" "custom"] [:enum 'sym]]
                 [:or [:enum "LGTM" "" "multi\nline"] :nil [:enum 42]]]
   :out         :string
   :rel         (fn [[sig msg] out]
                  (and (= out (str "[SIGNAL: " (name sig) "] " msg))
                       (str/starts-with? out "[SIGNAL: ")
                       (not (str/includes? (subs out 0 (str/index-of out "]")) "/"))))
   :num-tests   300
   :n-cases     12
   :golden-path "test/golden/agora/dialogue-format-signal.edn"})

(deftest format-signal-example-test
  (is (= "[SIGNAL: approve] LGTM" (dialogue/format-signal :approve "LGTM"))))
