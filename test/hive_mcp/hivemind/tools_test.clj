(ns hive-mcp.hivemind.tools-test
  "Facets for the hivemind MCP tool table and register-tools!.

   register-tools! hands every tool to an injected register fn (its port),
   so it runs here with a recording stub. The golden pins the advertised
   surface (names, required params, property keys) and the handler branches
   that answer before touching the swarm: hivemind_respond without an answer,
   hivemind_messages with neither agent_id nor context_id. The other branches
   reach the swarm library and are driven in test-swarm/."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.hivemind.tools :as tools]
            [hive-mcp.test.shim-facets :as sf]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- registered
  "The tools register-tools! hands to a recording register fn, in order."
  []
  (let [seen (atom [])]
    (tools/register-tools! #(swap! seen conj %))
    @seen))

(defn- surface
  "A tool as the MCP client sees it, without the handler fn. The event_type
   :enum is present only with the swarm library, so it is dropped here."
  [{:keys [name inputSchema]}]
  {:name name
   :required (vec (:required inputSchema))
   :properties (vec (sort (keys (:properties inputSchema))))})

(defn- handler [tool-name]
  (:handler (first (filter #(= tool-name (:name %)) tools/tools))))

(defn- answer [tool-name args]
  (json/read-str (:text ((handler tool-name) args)) :key-fn keyword))

(deftest-golden hivemind-tool-surface-golden
  "test/golden/hive-mcp/hivemind/tools-surface.edn"
  {:registered (mapv surface (registered))
   :respond-blank [(answer "hivemind_respond" {:ask_id "a1"})
                   (answer "hivemind_respond" {:ask_id "a1" :answer "  "})]
   :messages-without-ids [(answer "hivemind_messages" {})
                          (answer "hivemind_messages" {:agent_id ""})]})

(defspec register-tools-hands-each-tool-once-in-order 50
  (prop/for-all [n (gen/choose 1 3)]
    ;; however many times it is called, each call hands the table over once
    (let [seen (atom [])]
      (dotimes [_ n] (tools/register-tools! #(swap! seen conj (:name %))))
      (= (apply concat (repeat n (mapv :name tools/tools))) @seen))))

(deftest respond-without-an-answer-never-reaches-the-swarm
  (doseq [args [{:ask_id "x"} {:ask_id "x" :answer ""} {:ask_id "x" :decision " "}]]
    (is (= {:error "answer is required"} (answer "hivemind_respond" args)) (pr-str args))))

(hmut/deftest-mutations register-tools-mutations
  hive-mcp.hivemind.tools/register-tools!
  [["registers nothing" (fn [_] nil)]
   ["registers only the first tool" (fn [register] (register (first tools/tools)) nil)]
   ["registers in reverse" (fn [register] (doseq [t (reverse tools/tools)] (register t)))]]
  (fn []
    (is (= ["hivemind_shout" "hivemind_ask" "hivemind_status" "hivemind_respond" "hivemind_messages"]
           (mapv :name (registered))))
    (is (every? fn? (map :handler (registered))))))

(deftest status-and-messages-handlers-need-the-swarm-offline
  (if (sf/offline?)
    (is (= {:threw "java.lang.NullPointerException"}
           (sf/outcome (handler "hivemind_status") [{:project_id "p"}]))
        "SEAM GAP: hivemind.status reads registry/default-registry, a nil shim value offline")
    (is (string? (:text ((handler "hivemind_status") {:project_id "p"}))))))
