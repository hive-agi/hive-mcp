(ns hive-mcp.swarm.adapters.memory-scope-test
  "Facets for hive-mcp.swarm.adapters.memory-scope.

   IProjectScope delegates to hive-mcp.project.scope, a static require, so
   the property is plain delegation over paths with no .hive-project.edn
   anywhere above them (under /nonexistent-f00): the adapter's answer is the
   host's answer. The golden pins the staleness surface: the warning text
   for empty and populated warning lists, and the empty-path answers."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.project.scope :as project-scope]
            [hive-mcp.swarm.adapters.memory-scope :as memory-scope]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-spi.swarm.ports.memory-scope :as ms]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (ms/clear-memory-scope!) (try (f) (finally (ms/clear-memory-scope!)))))

(def ^:private gen-path
  (gen/fmap #(str "/nonexistent-f00/" %) (gen/not-empty gen/string-alphanumeric)))

(defspec project-scope-delegates-to-the-host 100
  (prop/for-all [path gen-path]
    (let [a (memory-scope/make-adapter)]
      (and (= (project-scope/get-current-project-id path) (ms/project-id-for-path a path))
           (= (project-scope/infer-scope-from-path path) (ms/infer-scope-from-path a path))))))

(deftest-golden staleness-surface-golden
  "test/golden/hive-mcp/swarm/adapters/memory-scope-staleness.edn"
  (let [a (memory-scope/make-adapter)]
    {:format-empty   (ms/format-staleness-warnings a [])
     :format-two     (ms/format-staleness-warnings
                      a [{:path "a.clj" :staleness 0.5 :id "x" :message "a.clj changed"}
                         {:path "b.clj" :staleness 0.9 :id "y" :message "b.clj changed"}])
     :warnings-empty (ms/staleness-warnings a [])
     :context-empty  (ms/kg-first-context a [])
     :project-id     (ms/project-id-for-path a "/nonexistent-f00/proj")
     :scope          (ms/infer-scope-from-path a "/nonexistent-f00/proj")}))

(deftest install-fills-the-slot-test
  (is (identical? (memory-scope/install!) (ms/get-memory-scope))))

(def ^:private Path
  "A path under a directory that holds no .hive-project.edn."
  [:string {:gen/schema [:string {:min 1}] :gen/fmap #(str "/nonexistent-f00/" %)}])

(deftest-port-contract project-scope-port-laws
  {:port/methods
   {ms/project-id-for-path   {:in [:cat Path] :out :string}
    ms/infer-scope-from-path {:in  [:cat Path] :out :string
                              :rel (fn [_ out] (= "global" out))}}}
  (memory-scope/make-adapter))
