(ns hive-mcp.test.stub.kanban
  "clojure.test fixtures for the hive-contracts kanban ports.

   with-core-kanban       core's real provider (hive-mcp.tools.kanban.port)
                          over whatever memory store the test installed
   with-recording-kanban  the hive-contracts recording stub, empty board;
                          a test reaches it with (current-stub)

   Both restore the ports to their prior providers afterwards."
  (:require [hive-contracts.kanban.stub :as stub]
            [hive-contracts.registry :as contracts]
            [hive-mcp.tools.kanban.port :as port]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:dynamic *stub*
  "The recording stub installed by `with-recording-kanban`."
  nil)

(defn current-stub
  "The recording stub the enclosing `with-recording-kanban` installed."
  []
  *stub*)

(defn- restore-ports!
  "Reinstall PRIOR providers (port -> impl) or revert to the Noops."
  [prior]
  (doseq [port [:IKanbanRead :IKanbanWrite]]
    (if-let [impl (get prior port)]
      (contracts/register! port impl)
      (contracts/unregister! port))))

(defn- prior-providers []
  (into {} (for [port [:IKanbanRead :IKanbanWrite]
                 :when (contracts/registered? port)]
             [port (contracts/provider port)])))

(defn with-core-kanban
  "Fixture: core's CoreKanban answers both ports for the duration of F."
  [f]
  (let [prior (prior-providers)]
    (try
      (port/register!)
      (f)
      (finally
        (restore-ports! prior)))))

(defn with-recording-kanban
  "Fixture: a fresh, empty recording stub answers both ports for F."
  [f]
  (let [prior (prior-providers)
        s     (stub/recording-kanban)]
    (try
      (stub/install! s)
      (binding [*stub* s] (f))
      (finally
        (restore-ports! prior)))))
