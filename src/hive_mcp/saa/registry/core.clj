(ns hive-mcp.saa.registry.core
  "Behaviour shared by the singleton SAA child registries (planners,
   phase-providers, scorers, plan-stores, dispatch-modes). Each child owns its
   own state atom, shaped by `types/RegistryState`; the helpers here read it."
  (:require [malli.core :as m]
            [hive-mcp.saa.types :as types]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def StateRef
  "A deref-able holder of a child registry's `types/RegistryState`."
  [:fn {:gen/elements [(atom {:by-id {} :by-owner {}})]}
   #(instance? clojure.lang.IDeref %)])

(defn lookup
  "The record registered under `id` in the child registry held by
   `state-ref`, or nil."
  [state-ref id]
  (get-in @state-ref [:by-id id]))

(m/=> lookup [:=> [:cat StateRef :any] [:maybe types/RegistryRecord]])
