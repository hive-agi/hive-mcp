(ns hive-mcp.tools.agent.liveness
  "Backend liveness age for `agent status` rows.

   A registered headless backend (hive-agent's agentic loop) reports how long
   ago its loop last made progress as `:liveness-age-ms` in its
   `headless-status`. The registry row alone says `:working` for as long as a
   dispatch has not finished, so a ling whose loop stalled kept reading
   \"working\". `annotate` copies the backend's age onto the row so
   `format-agent` can show it, and a stalled ling shows a growing age.

   Stratified: `annotate` is a pure calculation over rows and a status
   function; `backend-status-fn` is the boundary that resolves the status
   function from the headless registry."
  (:require [hive-mcp.agent.ling.headless-registry :as headless-registry]
            [hive-spi.addon.headless :as headless]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn annotate
  "Rows with `:slave/liveness-age-ms` set from `status-fn`, a function of one
   row returning the backend status map (or nil). Only a non-negative number
   is copied; a row the backend does not know, a status without an age and a
   status function that throws leave the row untouched. Pure given
   `status-fn`."
  [status-fn rows]
  (mapv (fn [row]
          (let [age (try (:liveness-age-ms (status-fn row))
                         (catch Exception e
                           (log/debug "liveness status failed:" (ex-message e))
                           nil))]
            (if (and (number? age) (not (neg? age)))
              (assoc row :slave/liveness-age-ms age)
              row)))
        rows))

(defn backend-status-fn
  "Boundary: a row -> the headless status of its backend, or nil when the
   row's `:ling/spawn-mode` names no registered backend. `backend-for` maps a
   spawn-mode keyword to an IHeadlessBackend (default: the headless registry)."
  ([] (backend-status-fn headless-registry/get-headless-backend))
  ([backend-for]
   (fn [row]
     (when-let [backend (some-> (:ling/spawn-mode row) backend-for)]
       (headless/headless-status backend {:id (:slave/id row)} nil)))))
