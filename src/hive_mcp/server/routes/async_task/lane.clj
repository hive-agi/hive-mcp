(ns hive-mcp.server.routes.async-task.lane
  "Which execution lane an async tool call runs on. Pure, declarative.

   Every async call used to share ONE pool (`mcp-async`, 8 slots, FIFO). The
   work on it ranges from a forge round-trip that needs seconds (`git ship
   land`) to an ingest that runs for hours, so eight long tasks left a land
   :task/queued for as long as the slowest of them ran (ASYNC-POOL-LANE,
   2026-10-12 atask-2225d26a: a queued land lost the race to a synchronous
   one and later failed with 405 already merged).

   A lane is a separate pool. `:batch` is the default and keeps the old pool's
   shape; `:short` is a small pool for bounded verbs a caller is waiting on, so
   long work cannot starve them. Which verbs are short is DATA
   (`declared-lanes`), not a branch in the middleware."
  (:require [malli.core :as m]
            [hive-mcp.server.routes.async-task.schema :as s]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-lane
  "The lane of everything not declared otherwise: the long-work pool."
  :batch)

(def lanes
  "Every lane with a pool behind it."
  #{:batch :short})

(defn known-lane
  "`lane` when it has a pool, else `default-lane`, so a typo or a nil never
   reaches a pool lookup."
  [lane]
  (if (contains? lanes lane) lane default-lane))

(def declared-lanes
  "tool name -> command string -> lane, for every call NOT on `default-lane`.

   Only bounded verbs belong here: the short lane is small, and a verb that
   can run for hours would starve it exactly as ingest starved the batch lane.
   The git verbs are the ones `hive-git.default-async/verbs` queues by
   default; each waits on a forge for a bounded time (land's no-CI wait is
   `hive-git.ship-wait/budget`, at most 30 s)."
  {"git" {"ship land"    :short
          "ship watch"   :short
          "ship release" :short
          "live sync"    :short}})

(defn- command-string
  [command]
  (cond
    (nil? command)     nil
    (keyword? command) (name command)
    :else              (str command)))

(defn classify
  "The lane for one call to `tool-name` with `command`, per `table`
   (default `declared-lanes`). Anything undeclared is `default-lane`."
  ([tool-name command] (classify declared-lanes tool-name command))
  ([table tool-name command]
   (or (get-in table [(some-> tool-name str) (command-string command)])
       default-lane)))

;; =============================================================================
;; Contracts
;; =============================================================================

(def ^:private LaneTable
  "tool name -> command string -> lane, the shape of `declared-lanes`."
  [:map-of :string [:map-of :string s/Lane]])

(m/=> known-lane [:=> [:cat :any] s/Lane])
(m/=> classify [:function
                [:=> [:cat :any :any] s/Lane]
                [:=> [:cat LaneTable :any :any] s/Lane]])
