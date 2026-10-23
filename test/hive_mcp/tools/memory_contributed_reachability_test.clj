(ns hive-mcp.tools.memory-contributed-reachability-test
  "Every command an addon contributes to the `memory` root is reachable through
   the schema the root serves: each of its params is declared there, so a
   client that validates against the schema and drops undeclared keys still
   delivers them, and a call carrying them routes to the contributed handler.

   Kanban 20260820011051-47647d6f: `ingest batch` was advertised by
   `memory help` while `paths`, `strategy`, `dir`, `glob` had no slot in the
   memory schema, so the client stripped them before dispatch."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-addon.registry.commands :as acmds]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.tools.consolidated.memory :as memory]
            [hive-mcp.tools.registry :as reg]
            [hive-mcp.tools.schema-keys :as schema-keys]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private addon-id :memory-reachability-test)

(def ^:private stub-params
  "The ingest params a client lost at the memory schema boundary, plus `path`,
   which memory itself declares for migration (a shared name must survive)."
  {"path"       {:type "string" :description "File path to ingest"}
   "paths"      {:type "array" :items {:type "string"} :description "[batch] Files"}
   "dir"        {:type "string" :description "[batch] Directory to expand"}
   "glob"       {:type "string" :description "[batch] Glob applied to dir"}
   "strategy"   {:type "string" :enum ["outline" "heading"] :description "Chunking strategy"}
   "chunk-size" {:type "integer" :description "Chunk size in chars"}
   "urls"       {:type "array" :items {:type "string"} :description "[web] URLs"}})

(defn- contribute-stub!
  "Contribute an `ingest`-shaped subdomain to `memory` whose verbs echo the
   params they receive into CAPTURED."
  [captured]
  (ext/contribute-commands!
   "memory" addon-id
   {"zingest" {:handler {:batch (fn [p] (reset! captured p) {:type "text" :text "ok"})
                         :file  (fn [p] (reset! captured p) {:type "text" :text "ok"})}
               :params  stub-params
               :description "stub ingest subdomain"}}))

(defn- served-props
  "Property NAMES of the memory root on SURFACE (:advertised or :server)."
  [surface]
  (let [t (case surface
            :advertised (first (filter #(= "memory" (:name %)) (reg/get-advertised-tools)))
            :server     (routes/make-tool (first (memory/tool-defs))))]
    (set (map name (keys (get-in t [:inputSchema :properties]))))))

(defn- legal-names
  "PARAMS' names as the served surface spells them (provider-legal aliases)."
  [params]
  (set (map name (keys (schema-keys/legal-properties params)))))

(deftest every-contributed-memory-param-is-declared
  (contribute-stub! (atom nil))
  (try
    (doseq [surface [:advertised :server]]
      (testing (str surface " surface")
        (let [props (served-props surface)]
          (testing "the stub's params, including the ones a client used to drop"
            (is (every? props (legal-names stub-params))
                (str "missing: " (remove props (legal-names stub-params)))))
          (testing "every live contribution to memory, ingest included when mounted"
            (doseq [[cmd spec] (acmds/get-commands "memory")
                    :let [missing (remove props (legal-names (:params spec)))]]
              (is (empty? missing) (str cmd " params not served: " (vec missing))))))))
    (finally (ext/retract-commands! "memory" addon-id))))

(deftest contributed-params-reach-the-contributed-handler
  (let [captured (atom nil)]
    (contribute-stub! captured)
    (try
      (let [handler (:handler (routes/make-tool (first (memory/tool-defs))))
            args    {"command" "zingest batch" "paths" ["/tmp/a.md"] "strategy" "outline"
                     "chunk-size" 900 "async" false "_caller_id" "memory-reachability-test"}]
        (handler args)
        (let [p @captured
              v (fn [k] (or (get p k) (get p (keyword k))))]
          (is (some? p) "the contributed verb was dispatched")
          (is (= ["/tmp/a.md"] (vec (v "paths"))))
          (is (= "outline" (v "strategy")))
          (is (= 900 (v "chunk-size")))))
      (finally (ext/retract-commands! "memory" addon-id)))))
