(ns hive-mcp.tools.catchup.spawn
  "Spawn context injection for ling priming."
  (:require [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.project.scope :as project-scope]
            [hive-mcp.tools.catchup.scope :as catchup-scope]
            [hive-mcp.tools.catchup.git :as catchup-git]
            [hive-mcp.tools.catchup.format :as fmt]
            [hive-mcp.spi.disc :as disc-port]
            [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.context.reconstruction :as reconstruction]
            [hive-mcp.dns.result :refer [rescue]]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.extensions.soft :as soft]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- lookup-context-refs
  "Query context-store for catchup-cached entries by project scope.
   context-query returns entries newest first; the first entry seen per
   category wins, so each category resolves to its newest cached ref."
  [project-id]
  (rescue nil
          (let [entries (context-store/context-query :tags #{"catchup"} :limit 20)
                project-tag (or project-id "global")
                matching (filter (fn [entry]
                                   (contains? (:tags entry) project-tag))
                                 entries)
                category-tags #{"axioms" "priority-conventions" "sessions"
                                "decisions" "conventions" "snippets"}
                refs (reduce (fn [acc entry]
                               (let [cat-tag (first (filter category-tags (:tags entry)))
                                     k (some-> cat-tag keyword)]
                                 (if (and k (not (contains? acc k)))
                                   (assoc acc k (:id entry))
                                   acc)))
                             {}
                             matching)]
            (when (seq refs)
              refs))))

(defn- serialize-ref-context
  "Serialize context-store refs to a compact markdown block for spawn injection."
  [refs {:keys [project-name git-info]}]
  (let [sections (atom [])]
    (swap! sections conj "## Project Context (Ref Mode — Auto-Injected)")
    (swap! sections conj "")
    (when project-name
      (swap! sections conj (str "**Project**: " project-name)))
    (swap! sections conj "")
    (swap! sections conj "**Mode**: ref (context-store references, not full text)")
    (swap! sections conj "**Action**: Fetch these refs via `session context-get` to hydrate your context.")
    (swap! sections conj "")

    (swap! sections conj "### Context References")
    (swap! sections conj "")
    (swap! sections conj "| Category | Context ID |")
    (swap! sections conj "|----------|------------|")
    (doseq [[category ctx-id] (sort-by key refs)]
      (swap! sections conj (str "| " (name category) " | `" ctx-id "` |")))
    (swap! sections conj "")

    (swap! sections conj "### How to Hydrate")
    (swap! sections conj "")
    (swap! sections conj "Fetch each ref via the session tool:")
    (swap! sections conj "```")
    (doseq [[_category ctx-id] (sort-by key refs)]
      (swap! sections conj (str "session {\"command\": \"context-get\", \"ctx_id\": \"" ctx-id "\"}")))
    (swap! sections conj "```")
    (swap! sections conj "")
    (swap! sections conj (str "**TTL Warning**: These refs expire ~1 hour after catchup. "
                              "If expired, run `/catchup` to refresh."))
    (swap! sections conj "")

    (when git-info
      (swap! sections conj "### Git Status")
      (swap! sections conj (str "- **Branch**: " (or (:branch git-info) "unknown")))
      (when (:uncommitted git-info)
        (swap! sections conj "- **Uncommitted changes**: yes"))
      (swap! sections conj (str "- **Last commit**: " (or (:last-commit git-info) "unknown")))
      (swap! sections conj ""))

    (str/join "\n" @sections)))

;; -----------------------------------------------------------------------------
;; spawn-context stages (CPPB): pure promote/pipeline fns, one collector per mode
;; -----------------------------------------------------------------------------

(def ^:private GitInfo
  [:maybe [:map
           [:branch {:optional true} [:maybe :string]]
           [:uncommitted {:optional true} :any]
           [:last-commit {:optional true} [:maybe :string]]]])

(def ^:private SpawnScope
  "What every mode collector needs: where, which project, and its label."
  [:map
   [:directory [:maybe :string]]
   [:project-id [:maybe :string]]
   [:label :string]])

(defn- git-status-md
  "Pipeline (pure): the Git Status block appended to a reconstructed context,
   or nil without git info."
  [git-info]
  (when git-info
    (str "\n\n### Git Status\n"
         "- **Branch**: " (or (:branch git-info) "unknown") "\n"
         (when (:uncommitted git-info) "- **Uncommitted changes**: yes\n")
         "- **Last commit**: " (or (:last-commit git-info) "unknown") "\n")))

(m/=> git-status-md [:=> [:cat GitInfo] [:maybe :string]])

(defn- enrich-hints
  "Promote (pure): fold task-scoped hints (L1 read ids, L2 queries, L3 KG
   seeds) into the project hint data's :memory-hints. No task hints, no change."
  [hint-data task-hints]
  (if-not task-hints
    hint-data
    (update hint-data :memory-hints
            (fn [mh]
              (cond-> mh
                (seq (:l1-ids task-hints))
                (update :read-ids (fnil into []) (:l1-ids task-hints))
                (seq (:l2-queries task-hints))
                (update :queries (fnil into []) (:l2-queries task-hints))
                (seq (:l3-seeds task-hints))
                (assoc :kg-seeds (mapv :id (:l3-seeds task-hints))
                       :kg-depth (get (first (:l3-seeds task-hints)) :depth 2)))))))

(m/=> enrich-hints [:=> [:cat :map [:maybe :map]] :map])

(defn- full-context-payload
  "Pipeline (pure): serialize the :full payload, dropping decisions and stale
   files when it would exceed `fmt/max-spawn-context-chars`."
  [payload]
  (let [context-str (fmt/serialize-spawn-context payload)]
    (if (> (count context-str) fmt/max-spawn-context-chars)
      (do (log/warn "spawn-context exceeds token budget:"
                    (count context-str) "chars, truncating decisions + stale-files")
          (fmt/serialize-spawn-context (assoc payload :decisions [] :stale-files [])))
      context-str)))

(m/=> full-context-payload [:=> [:cat :map] :string])

(defn- full-mode-context
  "Collect + render :full: axioms, priority conventions, decisions, stale
   files and git status for the project."
  [{:keys [directory project-id label]}]
  (let [axioms (catchup-scope/query-axioms project-id)
        priority-conventions (catchup-scope/query-scoped-entries
                              "convention" ["catchup-priority"] project-id 50)
        decisions (catchup-scope/query-scoped-entries "decision" nil project-id 50)]
    (full-context-payload
     {:axioms (mapv fmt/entry->axiom-meta axioms)
      :priority-conventions (mapv fmt/entry->priority-meta priority-conventions)
      :decisions (mapv #(fmt/entry->catchup-meta % 80) decisions)
      :stale-files (rescue [] (disc-port/top-stale-files {:n 5, :project-id project-id}))
      :git-info (catchup-git/gather-git-info directory)
      :project-name label})))

(m/=> full-mode-context [:=> [:cat SpawnScope] :string])

(defn- decision-kg-seeds
  "Collect: up to five entry ids from the cached decisions ref, the KG seeds
   for reconstruction. [] when the ref is missing or unreadable."
  [refs]
  (or (when-let [dec-ref (:decisions refs)]
        (rescue []
                (when-let [entry (context-store/context-get dec-ref)]
                  (->> (:data entry) (take 5) (keep :id) vec))))
      []))

(m/=> decision-kg-seeds [:=> [:cat [:maybe :map]] [:vector :any]])

(defn- ref-mode-context
  "Collect + render :ref from cached context-store refs: the reconstruction
   extension's output with git status, else the self-describing ref table.
   nil when the project has no cached refs (the caller falls back to :full)."
  [{:keys [directory project-id label]}]
  (when-let [refs (lookup-context-refs project-id)]
    (let [git-info (catchup-git/gather-git-info directory)
          reconstructed (rescue nil (reconstruction/reconstruct-context
                                     refs (decision-kg-seeds refs) project-id))]
      (log/info "spawn-context :ref mode, found" (count refs) "refs"
                {:categories (keys refs) :reconstructed? (boolean (seq reconstructed))})
      (if (seq reconstructed)
        (str reconstructed (git-status-md git-info))
        (serialize-ref-context refs {:project-name label :git-info git-info})))))

(m/=> ref-mode-context [:=> [:cat SpawnScope] [:maybe :string]])

(defn- hints-mode-context
  "Collect + render :hints through the agent domain's hint generator. nil when
   that domain is absent (the caller falls back to :full)."
  [{:keys [directory project-id label]} task task-id]
  (when-let [generate (soft/resolve-soft 'hive-mcp.agent.hints/generate-hints)]
    (let [task-hints (when task-id
                       (rescue nil
                               (when-let [gen-task (soft/resolve-soft 'hive-mcp.agent.hints/generate-task-hints)]
                                 (gen-task {:task-id task-id :depth 2}))))
          serialize (soft/resolve-soft 'hive-mcp.agent.hints/serialize-hints)]
      (serialize (enrich-hints (generate project-id {:task task}) task-hints)
                 :project-name label
                 :git-info (catchup-git/gather-git-info directory)))))

(m/=> hints-mode-context [:=> [:cat SpawnScope [:maybe :string] [:maybe :string]] [:maybe :string]])

(defn spawn-context
  "Generate a compact context payload for ling spawn injection.

   :ref and :hints answer through their own collector; either one falling
   through (no cached refs, no agent domain) answers the :full context
   instead, as does any other mode."
  ([directory] (spawn-context directory {}))
  ([directory {:keys [mode task task-id] :or {mode :full}}]
   (when (mem-proto/store-set?)
     (rescue nil
             (let [project-id (project-scope/get-current-project-id directory)
                   scope {:directory directory
                          :project-id project-id
                          :label (or (catchup-scope/get-current-project-name directory)
                                     project-id "global")}
                   ctx (case mode
                         :ref (ref-mode-context scope)
                         :hints (hints-mode-context scope task task-id)
                         nil)]
               (or ctx
                   (do (when (#{:ref :hints} mode)
                         (log/info "spawn-context" mode "mode unavailable, falling back to :full"))
                       (full-mode-context scope))))))))

;; =============================================================================
;; Contracts
;; =============================================================================

(def SpawnOpts
  "spawn-context options. :mode picks :ref, :hints or (any other value) :full."
  [:maybe [:map
           [:mode {:optional true} [:maybe [:or :keyword :string]]]
           [:task {:optional true} [:maybe :string]]
           [:task-id {:optional true} [:maybe :string]]]])

(def ^:private Directory
  [:maybe [:string {:gen/schema [:enum "/tmp" "/nonexistent-m26/x"]}]])

(m/=> spawn-context
      [:function
       [:=> [:cat Directory] [:maybe :string]]
       [:=> [:cat Directory SpawnOpts] [:maybe :string]]])
