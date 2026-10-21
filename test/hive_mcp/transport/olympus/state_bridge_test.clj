(ns hive-mcp.transport.olympus.state-bridge-test
  "Facets for hive-mcp.transport.olympus.state-bridge (cov-facets f07).

   The two public fns are the bridge lifecycle. wire-ds-state-bridge! starts
   a daemon thread over an in-process queue (no socket) and then tries to
   listen on the swarm DataScript store; when hive-datascript is absent that
   listen! throws inside the rescue, so the thread is started either way.
   The golden pins the lifecycle state as the public atoms expose it, and
   that both fns answer nil and are idempotent. The classify/flush internals
   are private and only reached through a DataScript transaction (the store
   is a concretion: no injected port), so they are not pinned here."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.transport.olympus.state-bridge :as sb]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- state []
  {:thread? (some? @sb/ds-bridge-thread)
   :alive?  (boolean (some-> ^Thread @sb/ds-bridge-thread .isAlive))
   :queue   (.size sb/ds-bridge-queue)})

(deftest-golden state-bridge-lifecycle-golden
  "test/golden/hive-mcp/f07-olympus-state-bridge.edn"
  (let [before  (do (sb/stop-ds-state-bridge!) (state))
        wired   (sb/wire-ds-state-bridge!)
        running (state)
        rewired (sb/wire-ds-state-bridge!)
        same?   (some? @sb/ds-bridge-thread)
        stopped (sb/stop-ds-state-bridge!)
        after   (state)
        again   (sb/stop-ds-state-bridge!)]
    {:before before :wire wired :running running :rewire rewired
     :thread-kept same? :stop stopped :after after :stop-again again
     :throttle-ms sb/bridge-throttle-ms}))

(deftest stop-clears-the-queue
  (sb/stop-ds-state-bridge!)
  (is (zero? (.size sb/ds-bridge-queue)))
  (is (nil? @sb/ds-bridge-thread)))
