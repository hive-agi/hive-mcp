(ns hive-mcp.system.sweep-wiring-test
  "That :hive/sweep-coordinator actually starts the heartbeat, and that each
   sweep ends up with exactly one owner.

   Before this key existed the coordinator had no caller anywhere in the tree,
   so every registry-based sweep was inert while :hive/housekeeping ran its own
   hardcoded list. These tests are what keeps that from silently returning: the
   failure mode is not an exception, it is a component that initialises fine and
   sweeps nothing."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [integrant.core :as ig]
            [hive-mcp.protocols.lifecycle :as lifecycle]
            [hive-mcp.system.layer5]
            [hive-mcp.system.registry :as reg]
            [hive-mcp.system.sweep-coordinator :as sc]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- sweep-names []
  (set (map lifecycle/sweep-name (reg/registered-sweeps))))

(def ^:private sweeper-namespaces
  "The sweepers :hive/sweep-coordinator is responsible for putting on the
   load path. Kept here so the fixture can rebuild a real world before it
   snapshots one."
  '[hive-mcp.system.sweepers.orphan-channel
    hive-mcp.system.sweepers.async-result
    hive-mcp.system.sweepers.heap-pressure])

(defn- restore-world
  "Start from a stopped coordinator and a genuinely registered world, and put
   the sweep registry back afterwards.

   The key under test both starts a real timer and unregisters a sweep, so a
   test that does not restore leaks a running executor into the rest of the
   suite.

   The RELOAD is what makes this test honest on a second run in the same JVM.
   A sweeper registers from a top-level form in its own namespace, and
   `init-key` only `require`s, which is a no-op once the namespace is loaded.
   So the first run registered the sweeps, the `finally` below restored a
   snapshot taken BEFORE they existed, and every later run asserted against a
   registry the suite itself had emptied -- a test that passes exactly once
   per JVM. Reloading first is only possible because those registrations are
   `def` and not `defonce`; under `defonce` an unregistered sweep could not be
   brought back at all (kanban 20260916134011-1246379c)."
  [f]
  (doseq [n sweeper-namespaces] (require n :reload))
  (let [saved (vec (reg/registered-sweeps))]
    (sc/stop!)
    (try
      (f)
      (finally
        (sc/stop!)
        (doseq [s (reg/registered-sweeps)]
          (reg/unregister-sweep! (lifecycle/sweep-name s)))
        (doseq [s saved] (reg/register-sweep! s))))))

(use-fixtures :each restore-world)

(deftest the-component-starts-the-heartbeat-rather-than-only-reporting-running
  (is (false? (:running? (sc/status))) "precondition: stopped")
  (let [state (ig/init-key :hive/sweep-coordinator {})]
    (is (= {:status :running} state))
    (is (true? (:running? (sc/status)))
        "the coordinator is actually running, not merely reported as such")
    (is (pos? (:heartbeat-s (sc/status)))
        "and it has a real cadence to run on")))

(deftest the-component-puts-the-sweepers-on-the-load-path
  ;; A sweeper registers from the defonce in its own namespace. Requiring it is
  ;; the whole mechanism, so a dropped require is a silently missing sweep.
  (ig/init-key :hive/sweep-coordinator {})
  (let [names (sweep-names)]
    (is (contains? names "channels/async-result-gc")
        "the async result buffers have a reclaimer")
    (is (contains? names "channels/orphan")
        "and the orphan channel sweep, which nothing used to require")))

(deftest terminal-liveness-has-exactly-one-owner
  ;; This test USED TO PASS VACUOUSLY. It asserted the sweep was absent from the
  ;; registry without loading the namespace that registers it, so it was really
  ;; asserting "not loaded yet" -- and in the live server the namespace loaded
  ;; later, when housekeeping's resolve-and-call reached it, re-arming a
  ;; duplicate that ran three times before it was caught by hand.
  ;;
  ;; Loading the namespace FIRST is the whole point: it is the only version of
  ;; this test that can fail.
  (require 'hive-mcp.swarm.lifecycle.terminal-sweep :reload)
  (is (some? (find-ns 'hive-mcp.swarm.lifecycle.terminal-sweep))
      "precondition: the namespace that used to self-register is loaded")
  (testing "loading it must not put it in the registry"
    (is (not (contains? (sweep-names) "lings/terminal-liveness"))
        (str "housekeeping owns terminal liveness on its own 5 minute timer; "
             "a registration here means two timers over the same DataScript "
             "rows, and the 60s interval silently winning the cadence"))
    (is (false? @(resolve 'hive-mcp.swarm.lifecycle.terminal-sweep/registered?))
        "and the namespace says so in the open, rather than by omission"))
  (testing "starting the coordinator does not acquire it either"
    (ig/init-key :hive/sweep-coordinator {})
    (is (not (contains? (sweep-names) "lings/terminal-liveness")))))

(deftest halting-the-component-stops-the-heartbeat
  (let [state (ig/init-key :hive/sweep-coordinator {})]
    (is (true? (:running? (sc/status))))
    (ig/halt-key! :hive/sweep-coordinator state)
    (is (false? (:running? (sc/status)))
        "a halted system leaves no timer behind")))

(deftest starting-twice-is-idempotent
  (ig/init-key :hive/sweep-coordinator {})
  (ig/init-key :hive/sweep-coordinator {})
  (is (true? (:running? (sc/status)))
      "a repeated init must not leave two executors racing the same sweeps"))
