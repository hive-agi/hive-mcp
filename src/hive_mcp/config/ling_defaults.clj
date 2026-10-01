(ns hive-mcp.config.ling-defaults
  "Typed default for the ling spawn mode, resolved via hive-di.

   A ling spawned without an explicit `spawn_mode` runs in this mode. The
   shipped default is `:headless` — the abstract headless interface, whose
   concrete backend `hive-mcp.agent.ling.headless-registry` resolves at
   spawn time. An operator who wants terminal lings back sets `:claude`
   (or any registered mode) as inert data.

   Three-tier resolution (highest precedence first):
     1. config.edn  [:ling :default-spawn-mode]   (operator decision)
     2. env var      HIVE_LING_DEFAULT_SPAWN_MODE  (deployment override)
     3. :headless                                  (shipped literal)

   Not validated here; the spawn path refuses an unregistered mode."
  (:require [hive-di.core :refer [defconfig env]]
            [hive-dsl.result :as r]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const shipped-spawn-mode
  "Spawn mode used when neither config.edn nor the env var names one."
  :headless)

(def config-path
  "Path of the operator override inside ~/.config/hive-mcp/config.edn."
  [:ling :default-spawn-mode])

(defconfig LingDefaultsConfig
  :default-spawn-mode (env "HIVE_LING_DEFAULT_SPAWN_MODE"
                           :default :headless
                           :type :keyword
                           :doc "Spawn mode for a ling spawned without spawn_mode. :headless = abstract headless (backend resolved by the headless registry); :claude = Claude Code TUI in a terminal addon."))

;; ── Collect ─────────────────────────────────────────────────────────────────

(defn- config-edn-value
  "Read `path` from config.edn via hive-mcp.config.core/get-in-config.
   Soft-resolved so this namespace loads without the config subsystem.
   Returns nil when config is not loaded or the path is absent. Never throws:
   an unreadable config falls to the env tier, never fails a spawn."
  [path]
  (r/rescue nil
    (when-let [getter (requiring-resolve 'hive-mcp.config.core/get-in-config)]
      (getter path))))

(defn- env-value
  "Resolve the env tier (with the shipped literal as its fallback).
   Returns nil when hive-di resolution fails."
  []
  (let [result (resolve-LingDefaultsConfig {})]
    (when (r/ok? result)
      (:default-spawn-mode (:ok result)))))

;; ── Promote (pure) ──────────────────────────────────────────────────────────

(defn ->spawn-mode
  "Coerce a raw configured value to a spawn-mode keyword.
   Accepts a keyword or a non-blank string (with or without a leading ':').
   Returns nil for anything else, so the caller falls to the next tier."
  [v]
  (cond
    (keyword? v) v
    (and (string? v) (not (re-matches #"\s*:?\s*" v)))
    (keyword (subs v (if (.startsWith ^String v ":") 1 0)))
    :else nil))

(defn pick
  "Pure precedence: first coercible value of config.edn, env, shipped literal."
  [edn-val env-val]
  (or (->spawn-mode edn-val)
      (->spawn-mode env-val)
      shipped-spawn-mode))

;; ── Facade ──────────────────────────────────────────────────────────────────

(defn default-spawn-mode
  "Resolved default ling spawn mode:
     config.edn [:ling :default-spawn-mode] > HIVE_LING_DEFAULT_SPAWN_MODE > :headless.
   Read per call, so a config reload takes effect at the next spawn."
  []
  (pick (config-edn-value config-path) (env-value)))
