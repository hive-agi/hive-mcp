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
            [hive-mcp.crystal.chain :as chain]
            [hive-mcp.crystal.consolidate :as cons]
            [hive-mcp.crystal.core :as core]
            [hive-mcp.crystal.fanout :as fan]
            [hive-mcp.crystal.harvest.collect :as coll]
            [hive-mcp.crystal.persist :as persist]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.catchup.format :as fmt]
            [taoensso.timbre :as log]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- plan-folds
  "Same-day folds for WRAPS (both sets, so mechanical and synthesised wraps
   agree on one decision), or WRAPS unchanged when there is no chain, no day,
   or nothing already stored for the day. Never throws: a chain failure means a
   fresh add, never a lost wrap."
  [chain wraps]
  (try
    (let [wraps (vec wraps)
          today (core/session-id)]
      (if (and chain today (seq wraps))
        (let [{:keys [folds failed]} (cons/folds-for chain wraps today)]
          (when (pos? failed)
            (log/warn "native-wrap: consolidation read failed for" failed "project(s); writing those fresh"))
          (cons/apply-folds wraps folds))
        wraps))
    (catch Throwable t
      (log/warn "native-wrap: fold planning failed, writing fresh:" (ex-message t))
      (vec wraps))))

(defn handle-native-wrap
  "Native multi-scope wrap implementation — Step 8 of plan
   `20260504173159-46dc47f1`.

   Pipeline:
     1. `coll/harvest-all-by-scope` — flat harvest → attribution → partition
        → `HarvestByScope`.
     2. `consolidate/folds-for` decides, per project, whether a summary for
        TODAY's local day is already stored. A same-day re-wrap is then
        rewritten in place instead of added, so a day keeps one summary (its
        id, and so its `:supersedes` edge, survives).
     3. Without a registered LLM synthesiser: `fan/synthesize-wraps` (the
        mechanical fallback) + `persist/persist-wraps!`, one write per scope.
     4. With one: `fan/mechanical-wraps` is persisted FIRST (no network), so
        a synthesis that throws, times out or never returns still leaves the
        wrap; then `fan/synthesize-wraps` runs and `persist/upgrade-wraps!`
        rewrites each pid's mechanical entry in place (same id, no duplicate).
     5. `chain/chain-wraps!` links each persisted summary to the previous
        summary of its project with a `:supersedes` KG edge, when a store and
        the KG are present. A chain failure never fails the wrap.
     Every entry carries an explicit `scope:project:<pid>` (or
     `scope:multi-project`) tag.

   Returns MCP text payload with aggregate shape:
     {:session   <session-id>
      :directory <dir>
      :total     <count>
      :persisted <count>
      :failed    <count>
      :wraps     [{:pid :project-id :id :success? :error?} ...]
      :synthesis <\"upgraded\" | \"failed: <msg>\">   ; only when delegated
      :consolidated {:folded <count>}                ; only when a fold happened
      :chained   {:linked <count> :failed <count>}   ; only when chained}"
  [args]
  (let [directory (ctx/resolve-caller-directory args)
        agent-id (:agent_id args)]
    (log/info "native-wrap: per-scope chain" {:directory directory :agent-id agent-id})
    (if-not (mem-proto/store-set?)
      (fmt/store-not-configured-error)
      (try
        (let [hbs (coll/harvest-all-by-scope {:directory directory
                                              :agent-id  agent-id})
              ch (chain/store-chain)
              [persist-result synthesis folded]
              (if-not (core/synthesis-delegated?)
                (let [wraps (plan-folds ch (fan/synthesize-wraps hbs))]
                  [(persist/persist-wraps! wraps)
                   nil
                   (count (filter :consolidate-into wraps))])
                (let [mech (plan-folds ch (fan/mechanical-wraps hbs))
                      written (persist/persist-wraps! mech)]
                  (try
                    [(persist/upgrade-wraps! written (fan/synthesize-wraps hbs))
                     "upgraded"
                     (count (filter :consolidate-into mech))]
                    (catch Throwable t
                      (log/warn t "native-wrap: synthesis failed, mechanical wrap kept")
                      [written (str "failed: " (ex-message t))
                       (count (filter :consolidate-into mech))]))))
              chained (when ch
                        (chain/chain-wraps! ch persist-result))]
          (log/info "native-wrap: completed"
                    {:total     (:total persist-result)
                     :persisted (:persisted persist-result)
                     :failed    (:failed persist-result)
                     :folded    folded
                     :synthesis synthesis
                     :chained   (:linked chained)})
          {:type "text"
           :text (json/write-str
                  (cond-> {:session   (:session hbs)
                           :directory directory
                           :total     (:total persist-result)
                           :persisted (:persisted persist-result)
                           :failed    (:failed persist-result)
                           :wraps     (:results persist-result)}
                    (pos? (or folded 0)) (assoc :consolidated {:folded folded})
                    synthesis (assoc :synthesis synthesis)
                    chained   (assoc :chained (select-keys chained [:linked :failed]))))})
        (catch Exception e
          (log/error e "native-wrap failed")
          {:type "text"
           :text (json/write-str {:error (.getMessage e)})
           :isError true})))))

(m/=> handle-native-wrap [:=> [:cat [:maybe :map]] [:map [:type [:= "text"]] [:text :string] [:isError {:optional true} [:= true]]]])
