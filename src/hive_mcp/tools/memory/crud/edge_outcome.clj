(ns hive-mcp.tools.memory.crud.edge-outcome
  "Outcome of the KG edge step of a memory add: the edge ids written, or one
   error row per requested edge when the edge write failed."
  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn edge-requests
  "The edges `kg-params` asks for, as [{:relation <name> :to <target-id>}] in
   creation order (implements, supersedes, depends-on, refines)."
  [{:keys [kg_implements kg_supersedes kg_depends_on kg_refines]}]
  (into []
        (mapcat (fn [[relation targets]]
                  (map (fn [target] {:relation relation :to target}) targets)))
        [["implements" kg_implements]
         ["supersedes" kg_supersedes]
         ["depends-on" kg_depends_on]
         ["refines"    kg_refines]]))

(defn- error-message
  [^Throwable t]
  (or (.getMessage t) (.getName (class t))))

(defn attempt-edges
  "Run `create!` (no args, returns the written edge ids) for `requests`.

   Returns {:edge-ids [ids] :edge-errors []} when create! returns, or
   {:edge-ids [] :edge-errors [{:relation :to :error}]} with one row per
   request when it throws. Never throws an Exception."
  [create! requests]
  (try
    {:edge-ids (vec (create!)) :edge-errors []}
    (catch Exception e
      (log/error e "KG edge creation failed; entry kept without edges")
      {:edge-ids    []
       :edge-errors (mapv #(assoc % :error (error-message e)) requests)})))
