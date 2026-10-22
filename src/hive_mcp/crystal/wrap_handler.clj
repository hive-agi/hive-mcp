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
            [hive-mcp.context.request :as ctx]
            [hive-mcp.crystal.fanout :as fan]
            [hive-mcp.crystal.harvest.collect :as coll]
            [hive-mcp.crystal.persist :as persist]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.catchup.format :as fmt]
            [taoensso.timbre :as log]
            [malli.core :as m]
            [hive-mcp.crystal.core :as core]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn handle-native-wrap
  "Native multi-scope wrap implementation — Step 8 of plan
   `20260504173159-46dc47f1`.

   Pipeline:
     1. `coll/harvest-all-by-scope` — flat harvest → attribution → partition
        → `HarvestByScope`.
     2. Without a registered LLM synthesiser: `fan/synthesize-wraps` (the
        mechanical fallback) + `persist/persist-wraps!`, one write per scope.
     3. With one: `fan/mechanical-wraps` is persisted FIRST (no network), so
        a synthesis that throws, times out or never returns still leaves the
        wrap; then `fan/synthesize-wraps` runs and `persist/upgrade-wraps!`
        rewrites each pid's mechanical entry in place (same id, no duplicate).
     Every entry carries an explicit `scope:project:<pid>` (or
     `scope:multi-project`) tag.

   Returns MCP text payload with aggregate shape:
     {:session   <session-id>
      :directory <dir>
      :total     <count>
      :persisted <count>
      :failed    <count>
      :wraps     [{:pid :project-id :id :success? :error?} ...]
      :synthesis <\"upgraded\" | \"failed: <msg>\">   ; only when delegated}"
  [args]
  (let [directory (ctx/resolve-caller-directory args)
        agent-id (:agent_id args)]
    (log/info "native-wrap: per-scope chain" {:directory directory :agent-id agent-id})
    (if-not (mem-proto/store-set?)
      (fmt/store-not-configured-error)
      (try
        (let [hbs (coll/harvest-all-by-scope {:directory directory
                                              :agent-id  agent-id})
              [persist-result synthesis]
              (if-not (core/synthesis-delegated?)
                [(persist/persist-wraps! (fan/synthesize-wraps hbs)) nil]
                (let [mechanical (persist/persist-wraps! (fan/mechanical-wraps hbs))]
                  (try
                    [(persist/upgrade-wraps! mechanical (fan/synthesize-wraps hbs)) "upgraded"]
                    (catch Throwable t
                      (log/warn t "native-wrap: synthesis failed, mechanical wrap kept")
                      [mechanical (str "failed: " (ex-message t))]))))]
          (log/info "native-wrap: completed"
                    {:total     (:total persist-result)
                     :persisted (:persisted persist-result)
                     :failed    (:failed persist-result)
                     :synthesis synthesis})
          {:type "text"
           :text (json/write-str
                  (cond-> {:session   (:session hbs)
                           :directory directory
                           :total     (:total persist-result)
                           :persisted (:persisted persist-result)
                           :failed    (:failed persist-result)
                           :wraps     (:results persist-result)}
                    synthesis (assoc :synthesis synthesis)))})
        (catch Exception e
          (log/error e "native-wrap failed")
          {:type "text"
           :text (json/write-str {:error (.getMessage e)})
           :isError true})))))

(m/=> handle-native-wrap [:=> [:cat [:maybe :map]] [:map [:type [:= "text"]] [:text :string] [:isError {:optional true} [:= true]]]])
