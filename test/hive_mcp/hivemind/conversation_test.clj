(ns hive-mcp.hivemind.conversation-test
  "Facets for the conversation compat shim: every public fn delegates to the
   same-named var of hive-agent.swarm.hivemind.conversation. Offline (no
   swarm library, this tree) a call raises addon-missing naming the var it
   reached for; the golden, property and mutants pin that. The protocol
   itself is driven in test-swarm/ (conversation_round_trip_test)."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
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

;; COV-FACETS-FINDINGS-W1: the Chan schema names clojure.core.async (its
;; :gen/fmap and the ReadPort protocol), so the shim must require it itself;
;; otherwise a JVM that loads the shim before core.async fails to compile it.
;; Read from source: core.async is already loaded in any test JVM.
(deftest shim-requires-the-core-async-it-names
  (let [ns-form (with-open [r (java.io.PushbackReader.
                               (io/reader (io/resource "hive_mcp/hivemind/conversation.clj")))]
                  (read r))
        required (->> ns-form
                      (filter #(and (seq? %) (= :require (first %))))
                      (mapcat rest)
                      (map #(if (vector? %) (first %) %))
                      set)]
    (is (contains? required 'clojure.core.async))
    (is (contains? required 'clojure.core.async.impl.protocols))))

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
