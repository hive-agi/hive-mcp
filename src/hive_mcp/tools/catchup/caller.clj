(ns hive-mcp.tools.catchup.caller
  "Caller-id resolution for per-caller catchup extensions.

   A per-caller extension (`:catchup/persona-lens`, `:catchup/bundle-profile`)
   is keyed by the id the caller was registered under, which is the bare agent
   id. The `:_caller_id` stamped on a tool call may carry a session suffix
   (`<agent-id>:<session-id>`). These functions derive the lookup keys; they do
   not change the raw caller id used for piggyback or hivemind cursors."
  (:require [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private default-caller-id "coordinator")

(defn- digits?
  [s]
  (and (seq s) (every? #(Character/isDigit ^char %) s)))

(defn caller-id-candidates
  "Lookup keys for `raw-caller-id`, most specific first.

   nil resolves to \"coordinator\". The raw id always comes first. When the id
   has a non-empty prefix before its LAST ':' and the suffix after it is all
   digits, that prefix follows as a second candidate. Returns a vector of one
   or two distinct strings."
  [raw-caller-id]
  (let [raw    (or raw-caller-id default-caller-id)
        idx    (str/last-index-of raw ":")
        prefix (when (and idx (pos? idx)) (subs raw 0 idx))
        suffix (when idx (subs raw (inc idx)))]
    (if (and prefix (digits? suffix))
      [raw prefix]
      [raw])))

(defn resolve-for-caller
  "Invoke the per-caller extension `f` as `(f candidate project-id)` for each
   of `(caller-id-candidates raw-caller-id)` in order, returning the first
   truthy result, or nil. Exceptions from `f` propagate."
  [f raw-caller-id project-id]
  (some #(f % project-id) (caller-id-candidates raw-caller-id)))
