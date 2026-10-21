(ns hive-mcp.swarm.claim.negotiate-test
  "Facets for hive-mcp.swarm.claim.negotiate on the unit classpath.

   The negotiation reasons over claim spans and store rows, both compat shims
   over libraries absent here (hive-agent, hive-datascript). The golden pins
   what runs without them: an empty or nil file set is acquired with nothing
   touched and no message sent (the injected notify! port records nothing),
   foresee-deadlock answers nil (no wait graph loaded), held-key reaches
   file-span for a whole-file hold and form-span for a form hold, and the
   yield ledger resets to empty. The refusal / park / yield-request flow over
   real rows runs in test-swarm/hive_mcp/swarm/claim/negotiate_test.clj."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.swarm.claim.negotiate :as negotiate]
            [hive-mcp.test.swarm-shim :as shim]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- recording-notify
  "A notify! port that records every message instead of shouting it."
  [sent]
  (fn [sender to event-type data] (swap! sent conj [sender to event-type data]) true))

(defn- unit-answers []
  (let [sent    (atom [])
        opts    {:notify! (recording-notify sent) :task-id "t1" :now-ms 0}
        ;; sequenced in the let: a large map literal evaluates in hash order
        empty-a (negotiate/acquire-for-ling! "ling-a" [] opts)
        nil-a   (negotiate/acquire-for-ling! "ling-a" nil opts)
        one-a   (shim/target negotiate/acquire-for-ling! "ling-a" ["a.clj"] opts)]
    {:acquire-empty      empty-a
     :acquire-nil        nil-a
     :acquire-one        one-a
     :sent               @sent
     :deadlock-no-keys   (negotiate/foresee-deadlock "ling-a" [])
     :deadlock-no-graph  (negotiate/foresee-deadlock "ling-a" ["a.clj"])
     :held-key-file      (shim/target negotiate/held-key {:file "a.clj" :mode :file})
     :held-key-form      (shim/target negotiate/held-key {:file "a.clj" :qn "a/b" :mode :body})
     :held-key-file-mode (shim/target negotiate/held-key {:file "a.clj" :qn "a/b" :mode :file})
     :reset              (negotiate/reset-yield-ledger!)}))

(deftest-golden negotiate-unit-golden
  "test/golden/hive-mcp/swarm/claim/negotiate-unit.edn"
  (unit-answers))

(deftest yield-ttl-matches-the-staleness-window-test
  (is (= 600000 negotiate/yield-ttl-ms)))
