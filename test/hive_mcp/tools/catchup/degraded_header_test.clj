(ns hive-mcp.tools.catchup.degraded-header-test
  "The catchup header names degraded subsystems and does not let zero counts
   read as an empty knowledge base."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [hive-mcp.tools.catchup.format :as fmt]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- header [in]
  (json/read-str (:text (first (fmt/build-catchup-response in))) :key-fn keyword))

(def ^:private carto-query-error
  {:readiness :query-error :lsp-up? true :carto-store? true
   :warnings ["carto query-entries failed - the :carto vector store did not answer"]})

(deftest header-names-degraded-carto
  (let [h (header {:project-id "p" :memory-status {:status :ok :warnings []}
                   :carto-status carto-query-error})]
    (is (= ["carto"] (mapv :subsystem (:degraded h))))
    (is (= "query-error" (:status (first (:degraded h)))))
    (is (string? (:degraded-note h)))))

(deftest header-names-degraded-memory
  (let [h (header {:project-id "p"
                   :memory-status {:status :partial :warnings ["vector store timeout"]}})]
    (is (some #{"memory"} (map :subsystem (:degraded h))))))

(deftest healthy-header-carries-no-degraded-key
  (let [h (header {:project-id "p" :memory-status {:status :ok :warnings []}
                   :carto-status {:readiness :ready :warnings []}
                   :axioms-meta [{:id "a"}]})]
    (is (not (contains? h :degraded)))
    (is (not (contains? h :counts-note)))))

(deftest zero-counts-are-not-an-empty-knowledge-base
  (testing "all-zero counts carry a note that they are not absence"
    (let [h (header {:project-id "p" :memory-status {:status :ok :warnings []}})]
      (is (every? zero? (vals (:counts h))))
      (is (re-find #"not evidence of an empty knowledge base" (str (:counts-note h)))))))

(deftest degraded-subsystems-is-pure
  (is (= [] (fmt/degraded-subsystems {:status :ok :warnings []} {:readiness :ready})))
  (is (= [] (fmt/degraded-subsystems nil nil)))
  (is (= ["carto"] (mapv :subsystem (fmt/degraded-subsystems nil {:readiness :store-unavailable})))))
