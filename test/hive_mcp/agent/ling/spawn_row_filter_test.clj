(ns hive-mcp.agent.ling.spawn-row-filter-test
  "The registry row a spawn writes passes the swarm row filter, and fails
   CLOSED (routing keys only) when the filter throws or answers a non-map."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.agent.ling.spawn :as spawn]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private row
  {:status :working :depth 1 :parent "coordinator"
   :presets ["p"] :cwd "/secret/cwd" :project-id "secret-project"
   :kanban-task-id "k-1"})

(defn- opaque-filter
  "Stub row filter: replaces the identifying attrs, as darkmatter does."
  [_ling-id attrs]
  (assoc attrs :cwd "opaque" :project-id "opaque" :kanban-task-id nil))

(defn- throwing-filter [_ _] (throw (ex-info "boom" {})))

(defn- non-map-filter [_ _] :not-a-map)

(def ^:private filters
  {:none nil :opaque opaque-filter :throws throwing-filter :non-map non-map-filter})

(deftest stub-filter-rewrites-the-row
  (testing "a registered filter's rewrite is what reaches the registry"
    (let [out (spawn/row-attrs opaque-filter "ling-1" row)]
      (is (= "opaque" (:cwd out)))
      (is (= "opaque" (:project-id out)))
      (is (nil? (:kanban-task-id out))))))

(deftest broken-filter-fails-closed
  (doseq [f [throwing-filter non-map-filter]]
    (is (= {:status :working :depth 1 :parent "coordinator"}
           (spawn/row-attrs f "ling-1" row)))))

(deftest absent-filter-is-identity
  (is (= row (spawn/row-attrs nil "ling-1" row))))

(def ^:private gen-args
  (gen/fmap (fn [[k cwd]] [(filters k) "ling-g" (assoc row :cwd cwd)])
            (gen/tuple (gen/elements (keys filters)) gen/string-alphanumeric)))

(defn- no-leak?
  "Whatever the filter does, a cwd outside an identity pass never leaks
   unless the filter is absent."
  [out]
  (and (map? out) (contains? out :status) (contains? out :parent)))

(deftrifecta spawn-row-attrs-fail-closed
  #'hive-mcp.agent.ling.spawn/row-attrs
  {:golden-path "test/golden/spawn_row_attrs.edn"
   :apply?      true
   :cases       {:none    [nil "ling-1" row]
                 :opaque  [opaque-filter "ling-1" row]
                 :throws  [throwing-filter "ling-1" row]
                 :non-map [non-map-filter "ling-1" row]}
   :gen         gen-args
   :pred        no-leak?
   :num-tests   100
   :mutations   [["fail-open-on-throw"
                  (fn [f id attrs] (try (if f (f id attrs) attrs) (catch Throwable _ attrs)))]
                 ["ignore-filter" (fn [_ _ attrs] attrs)]
                 ["drop-everything" (fn [_ _ _] {})]]})
