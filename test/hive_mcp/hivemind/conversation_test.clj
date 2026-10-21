(ns hive-mcp.hivemind.conversation-test
  "Facets for the conversation compat shim: every public fn delegates to the
   same-named var of hive-agent.swarm.hivemind.conversation. Offline (no
   swarm library, this tree) a call raises addon-missing naming the var it
   reached for; the golden, property and mutants pin that. The protocol
   itself is driven in test-swarm/ (conversation_round_trip_test).

   clojure.core.async is required BEFORE the shim on purpose: the shim's
   Chan schema names clojure.core.async/promise-chan in a :gen/fmap without
   requiring it (pre-existing, src unchanged), so a JVM that loads the shim
   first fails to compile it."
  (:require [clojure.core.async]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.hivemind.conversation]
            [hive-mcp.test.shim-facets :as sf]
            [hive-test.golden :as golden]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private shim 'hive-mcp.hivemind.conversation)
(def ^:private target "hive-agent.swarm.hivemind.conversation")
(def ^:private overrides {})
(def ^:private golden-path "test/golden/hive-mcp/hivemind/conversation-delegation.edn")

(golden/deftest-golden conversation-delegation-golden
  golden-path
  (sf/offline-golden 'hive-mcp.hivemind.conversation-test golden-path
                     #(sf/delegation-table shim (sf/contracted shim))))

(defspec every-conversation-call-reaches-its-namesake 100
  (prop/for-all [[sym args] (sf/call-gen shim (sf/contracted shim))]
    (sf/reaches-expected? shim target overrides sym args)))

;; Offline only: with the library present the call does real work.
(when (sf/offline?)
  (hmut/deftest-mutations tell-envelope-delegation-mutations
    hive-mcp.hivemind.conversation/tell-envelope
    (sf/delegation-mutants shim 'tell-envelope 'ask-envelope)
    (sf/delegation-check shim 'tell-envelope (sf/expected target overrides 'tell-envelope))))
