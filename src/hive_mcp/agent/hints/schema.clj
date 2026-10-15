(ns hive-mcp.agent.hints.schema
  "Contract schemas for the pure hint-domain calculations. Leaf namespace.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Items
  "Any collection of items the sections iterate (vector, list, set, lazy seq)."
  [:maybe [:or [:sequential :any] [:set :any]]])

(def Strings
  "A collection of strings (tags): vector, list or set."
  [:maybe [:or [:sequential :string] [:set :string]]])

(def TagSets
  "A collection of tag collections."
  [:maybe [:sequential [:sequential :string]]])

(def Section
  "A rendered section: lines, or nil when there is nothing to render."
  [:maybe [:vector :string]])

(def GitInfo
  [:maybe [:map-of :any :any]])
