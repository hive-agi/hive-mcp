(ns hive-mcp.tools.swarm.jvm.summary
  "Pure summary builders for JVM management operations.

   Compat shim: moved to hive-agent.swarm.tools.jvm.summary in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]
            [hive-mcp.tools.swarm.jvm.memory :as memory]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.tools.jvm.summary" sym))

(defn- impl!
  "impl, after making sure the host adapters fill the swarm port slots.
   The adapters are resolved late: a static require would close a load
   cycle through the events adapter. A failure here (for instance a call
   made while that cycle is still loading) leaves the ports on their
   noops until a later call installs them."
  [sym]
  (try ((requiring-resolve 'hive-mcp.swarm.adapters.boot/ensure!))
       (catch Throwable _ nil))
  (impl sym))

;; Value schemas for the shim contracts. They transcribe the bodies of
;; hive-agent.swarm.tools.jvm.summary, where the behaviour lives.

(def Pid
  "A process id as ps prints it."
  [:or :string :int])

(def SummaryProcess
  "A classified process as the summaries read it. Only :type is required:
   the rest are flags and ids read for grouping or counting."
  [:map
   [:type [:or :keyword :string]]
   [:pid {:optional true} [:maybe Pid]]
   [:swarm-spawned {:optional true} [:maybe :boolean]]
   [:swarm-slave-id {:optional true} [:maybe :string]]])

(def CleanupOpts
  "The cleanup flags echoed back into the summary."
  [:map
   [:dry-run {:optional true} [:maybe :boolean]]
   [:swarm-only {:optional true} [:maybe :boolean]]
   [:min-age {:optional true} [:maybe number?]]
   [:true-orphans-only {:optional true} [:maybe :boolean]]])

(def CleanupSummary
  "What `build-cleanup-summary` answers."
  [:map
   [:total-jvm-processes [:int {:min 0}]]
   [:by-type [:map-of :string [:int {:min 1}]]]
   [:swarm [:map
            [:total-swarm-spawned [:int {:min 0}]]
            [:by-slave [:map-of :string [:map [:count [:int {:min 1}]]
                                         [:pids [:sequential [:maybe Pid]]]]]]]]
   [:orphan-detection [:map
                       [:mode [:enum "true-orphans" "age-based"]]
                       [:truly-orphaned-count [:int {:min 0}]]
                       [:managed-by-claude [:int {:min 0}]]]]
   [:orphans-found [:int {:min 0}]]
   [:orphan-pids [:sequential [:maybe Pid]]]
   [:dry-run [:maybe :boolean]]
   [:swarm-only-mode [:maybe :boolean]]
   [:killed [:sequential [:maybe Pid]]]
   [:min-age-threshold [:maybe number?]]
   [:details [:sequential :map]]])

(def GuardStatus
  "Memory state across the cleanup."
  [:enum :healthy :recovered-after-cleanup :capacity-reached])

(def CleanupData
  "The decoded jvm_cleanup answer the guard reads, or nil when no cleanup ran."
  [:maybe [:map
           [:orphans-found {:optional true} [:maybe [:int {:min 0}]]]
           [:killed {:optional true} [:maybe [:sequential :any]]]]])

(def GuardSummary
  "What `build-resource-guard-summary` answers."
  [:map
   [:can-spawn [:maybe :boolean]]
   [:memory [:map
             [:initial [:maybe :map]]
             [:final [:maybe :map]]
             [:threshold-percent [:maybe number?]]
             [:min-available-mb [:maybe number?]]]]
   [:status GuardStatus]
   [:cleanup [:maybe [:map
                      [:ran [:= true]]
                      [:dry-run [:maybe :boolean]]
                      [:orphans-found [:maybe [:int {:min 0}]]]
                      [:killed [:int {:min 0}]]]]]
   [:recommendation memory/Recommendation]])

(defn build-cleanup-summary
  {:arglists '([all-procs classified all-classified orphans killed-pids {:keys [dry-run swarm-only min-age true-orphans-only]}])}
  [& args]
  (apply (impl! 'build-cleanup-summary) args))

(m/=> build-cleanup-summary
      [:=> [:cat [:sequential :any] [:sequential SummaryProcess] [:sequential :map]
            [:sequential [:map [:pid {:optional true} [:maybe Pid]]]]
            [:maybe [:sequential [:maybe Pid]]] CleanupOpts]
       CleanupSummary])

(defn build-resource-guard-summary
  {:arglists '([can-spawn initial-mem final-mem threshold min-available initial-high? final-high? {:keys [auto-clean cleanup-dry]} cleanup-data])}
  [& args]
  (apply (impl! 'build-resource-guard-summary) args))

(m/=> build-resource-guard-summary
      [:=> [:cat [:maybe :boolean] [:maybe :map] [:maybe :map] [:maybe number?] [:maybe number?]
            [:maybe :boolean] [:maybe :boolean]
            [:map [:auto-clean {:optional true} [:maybe :boolean]]
             [:cleanup-dry {:optional true} [:maybe :boolean]]]
            CleanupData]
       GuardSummary])
