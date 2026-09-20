(ns hive-mcp.crystal.wrap-handler
  "The native multi-scope wrap: harvest a session, fan it out per scope,
   persist the entries.

   Memory domain work. It ran inside `hive-mcp.tools.catchup`, which is
   kernel, and that is why the kernel required three `crystal.*` namespaces
   for one handler. The kernel keeps the ENTRY POINT
   (`tools.catchup/handle-native-wrap`, the name its tests and the tool
   registry call) and resolves this by symbol; with the memory domain absent
   the entry point answers an error naming what is missing, because a wrap
   with nothing to harvest into is not something to fake."
  (:require [clojure.data.json :as json]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.crystal.fanout :as fan]
            [hive-mcp.crystal.harvest.collect :as coll]
            [hive-mcp.crystal.persist :as persist]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.catchup.format :as fmt]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn handle-native-wrap
  "Native multi-scope wrap implementation — Step 8 of plan
   `20260504173159-46dc47f1`.

   Pipeline (no extension delegation):
     1. `coll/harvest-all-by-scope` — flat harvest → attribution → partition
        → `HarvestByScope`.
     2. `fan/synthesize-wraps` — fan-out one entry per touched scope plus
        an umbrella; each entry carries an explicit `scope:project:<pid>`
        (or `scope:multi-project`) tag from step-6 `with-scope-tag`.
     3. `persist/persist-wraps!` — direct `mem-proto/add-entry!` per entry
        with explicit `:project-id` from `:pid` (no pwd derivation).

   Returns MCP text payload with aggregate shape:
     {:session   <session-id>
      :directory <dir>
      :total     <count>
      :persisted <count>
      :failed    <count>
      :wraps     [{:pid :project-id :id :success? :error?} ...]}"
  [args]
  (let [directory (ctx/resolve-caller-directory args)
        agent-id (:agent_id args)]
    (log/info "native-wrap: per-scope chain" {:directory directory :agent-id agent-id})
    (if-not (mem-proto/store-set?)
      (fmt/store-not-configured-error)
      (try
        (let [hbs            (coll/harvest-all-by-scope {:directory directory
                                                         :agent-id  agent-id})
              wraps          (fan/synthesize-wraps hbs)
              persist-result (persist/persist-wraps! wraps)]
          (log/info "native-wrap: completed"
                    {:total     (:total persist-result)
                     :persisted (:persisted persist-result)
                     :failed    (:failed persist-result)})
          {:type "text"
           :text (json/write-str
                  {:session   (:session hbs)
                   :directory directory
                   :total     (:total persist-result)
                   :persisted (:persisted persist-result)
                   :failed    (:failed persist-result)
                   :wraps     (:results persist-result)})})
        (catch Exception e
          (log/error e "native-wrap failed")
          {:type "text"
           :text (json/write-str {:error (.getMessage e)})
           :isError true})))))
