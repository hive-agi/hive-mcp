(ns hive-mcp.server.transport.olympus-ws-test
  "Facets for the Olympus WS starter. `soft/*resolve*` is bound to a recording
   resolver, so both arms (transport present, transport absent) run without
   the real Olympus transport and without binding a port."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.extensions.soft :as soft]
            [hive-mcp.server.transport.olympus-ws :as oly]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- recording-resolver
  "Resolves the symbols in PRESENT to fns that log their own name in SEEN."
  [seen present]
  (fn [sym]
    (when (contains? present sym)
      (fn [& _] (swap! seen conj (name sym)) ::ignored))))

(defn- drive [present]
  (let [seen (atom [])
        out  (binding [soft/*resolve* (recording-resolver seen present)]
               (oly/start-olympus-ws!))]
    {:out out :called @seen}))

(golden/deftest-golden olympus-start-arms-golden
  "test/golden/hive-mcp/server/transport/olympus-ws.edn"
  {:absent       (drive #{})
   :start-only   (drive #{'hive-mcp.transport.olympus/start!})
   :start-wire   (drive #{'hive-mcp.transport.olympus/start!
                          'hive-mcp.transport.olympus/wire-hivemind-events!})})

(deftest a-throwing-start-is-rescued
  (binding [soft/*resolve* (fn [_] (fn [] (throw (ex-info "boom" {}))))]
    (is (nil? (oly/start-olympus-ws!)))))
