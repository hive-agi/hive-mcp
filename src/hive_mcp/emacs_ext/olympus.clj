(ns hive-mcp.emacs-ext.olympus
  "Extension-registry façade for `:emacs/olympus-*` keys.

   Replaces the legacy `hive-mcp.emacs.olympus` delegation shim. Pure
   layout calculations — when hive-emacs is not loaded, fns return nil
   and the consumer (tools/olympus.clj) falls back to no-ops.

   Decisions: 20260429195812-0c5dfe8d, 20260429230453-7e7627cc (Phase 2)."
  (:require [hive-mcp.extensions.registry :as ext]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def GridLayout
  "A single-screen grid: rows x cols, minus any `:empty-cells`."
  [:map
   [:rows nat-int?]
   [:cols nat-int?]
   [:empty-cells {:optional true} [:set [:tuple nat-int? nat-int?]]]])

(def TabbedLayout
  "Five or more lings: `:tabs` pages of `:per-tab` cells each."
  [:map
   [:tabs pos-int?]
   [:per-tab pos-int?]])

(def Layout
  "The layout spec `calculate-layout` answers and `assign-positions` reads."
  [:or GridLayout TabbedLayout])

(def Position
  "Where one ling sits: row and column in the grid, and its tab (nil for an
   untabbed grid)."
  [:map
   [:row nat-int?]
   [:col nat-int?]
   [:tab [:maybe nat-int?]]])

(def Positions
  "ling-id -> Position."
  [:map-of [:maybe :string] Position])

(defn calculate-layout [n]
  (when-let [f (ext/get-extension :emacs/olympus-calculate-layout)] (f n)))

(m/=> calculate-layout [:=> [:cat [:maybe :int]] [:maybe Layout]])

(defn assign-positions [lings layout]
  (when-let [f (ext/get-extension :emacs/olympus-assign-positions)]
    (f lings layout)))

(m/=> assign-positions
      [:=> [:cat [:maybe [:sequential :map]] [:maybe :map]] [:maybe Positions]])

(defn position-for-cell [positions row col tab]
  (when-let [f (ext/get-extension :emacs/olympus-position-for-cell)]
    (f positions row col tab)))

;; positions arrive from persisted Olympus state (parsed EDN), so the input
;; is any map of cells; the answer is the slave id found there.
(m/=> position-for-cell
      [:=> [:cat [:maybe [:map-of :any :map]] :int :int [:maybe :int]] [:maybe :string]])
