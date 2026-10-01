(ns hive-mcp.addons.boot-health-test
  "A boot that discovers one addon manifest must say so: in the log, in every
   help / unknown-command answer, in memory-store errors, and in a readiness
   probe. Incident: hive memory 20260901002309-05854dd7."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.addons.boot-health :as bh]
            [hive-mcp.addons.core :as addon-core]
            [hive-mcp.tools.cli :as cli]
            [hive-mcp.tools.consolidated.addon :as addon-tool]
            [hive-mcp.extensions.loader :as loader]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (bh/reset-state!) (try (f) (finally (bh/reset-state!)))))

(defn- tmp-path []
  (let [f (java.io.File/createTempFile "roster-baseline" ".edn")]
    (.delete f)
    (.deleteOnExit f)
    (str f)))

(def incident-facts
  {:discovered ["hive.ttracking"] :mounted ["hive.ttracking"] :failed []})

(deftest memory-deferral-to-absent-addon-is-an-error
  (let [issues (bh/assess (assoc incident-facts
                                 :memory {:backend "milvus" :deferred-to "hive.milvus"
                                          :store-set? false}))]
    (is (bh/degraded? issues))
    (is (= [:memory-store-missing] (map :issue issues)))
    (is (str/includes? (:message (first issues)) "NOT on the classpath"))))

(deftest kept-promise-is-healthy
  (is (empty? (bh/assess {:discovered ["hive.milvus"] :mounted ["hive.milvus"]
                          :memory {:backend "milvus" :deferred-to "hive.milvus"
                                   :store-set? true}}))))

(deftest floor-and-expected-ids
  (let [issues (bh/assess (assoc incident-facts :expected-min 5 :expected #{"hive.carto"}))]
    (is (= #{:below-floor :expected-missing} (set (map :issue issues))))
    (is (bh/degraded? issues))))

(deftest mount-failure-alone-warns-but-does-not-degrade
  (let [issues (bh/assess {:discovered ["a" "b"] :mounted ["a"] :failed ["b"]})]
    (is (= [:warn] (map :severity issues)))
    (is (not (bh/degraded? issues)))))

(deftest baseline-catches-the-drop-and-is-not-lowered-by-it
  (let [path    (tmp-path)
        healthy {:discovered (mapv #(str "hive.a" %) (range 30))
                 :mounted    (mapv #(str "hive.a" %) (range 30))
                 :failed     []}]
    (testing "healthy boot writes the baseline"
      (is (not (:degraded? (bh/record-roster! healthy path))))
      (is (= 30 (count (:discovered (bh/read-baseline path))))))
    (testing "a one-manifest boot is degraded against it"
      (let [snap (bh/record-roster! incident-facts path)]
        (is (:degraded? snap))
        (is (= 30 (:expected-count snap)))
        (is (str/includes? (bh/notice) "expected 30 addons, mounted 1"))))
    (testing "the degraded boot did not overwrite the baseline"
      (is (= 30 (count (:discovered (bh/read-baseline path))))))
    (io/delete-file path true)))

(deftest record-memory-after-roster
  (bh/record-roster! incident-facts nil)
  (is (nil? (bh/notice)) "one manifest with no expectations is not provably degraded")
  (bh/record-memory! {:backend "milvus" :deferred-to "hive.milvus" :store-set? false})
  (is (:degraded? (bh/snapshot)))
  (is (str/includes? (bh/notice) "hive.milvus")))

(deftest cli-help-and-unknown-command-carry-the-notice
  (let [h (cli/make-cli-handler {:analysis (fn [_] {:type "text" :text "ok"})})]
    (testing "healthy: untouched"
      (is (not (str/includes? (:text (h {:command "help"})) "DEGRADED"))))
    (bh/record-roster! (assoc incident-facts :expected #{"hive.carto"}) nil)
    (is (str/includes? (:text (h {:command "help"})) "DEGRADED ADDON ROSTER"))
    (let [err (h {:command "carto callers"})]
      (is (:isError err))
      (is (str/includes? (:text err) "Unknown command"))
      (is (str/includes? (:text err) "hive.carto")))
    (is (= "ok" (:text (h {:command "analysis"}))) "successful dispatch is not decorated")))

(deftest readiness-probe
  (testing "before boot"
    (let [r (json/read-str (:text (addon-tool/handle-doctor {:command "doctor"})) :key-fn keyword)]
      (is (false? (:ready? r)))))
  (bh/record-roster! (assoc incident-facts :expected-min 10) nil)
  (let [resp (addon-tool/handle-addon {:command "readiness"})
        r    (json/read-str (:text resp) :key-fn keyword)]
    (is (:isError resp))
    (is (false? (:ready? r)))
    (is (= "expected 10 addons, mounted 1 (discovered 1)" (:summary r)))
    (is (= ["hive.ttracking"] (:mounted r)))))

(deftest loader-roster-facts
  (let [ordered [{:addon/id "hive.carto" :addon/init-ns "hive-carto.addon"}
                 {:addon/id "hive.keg" :addon/init-ns "hive-keg.addon"}
                 {:addon/id "hive.rss" :addon/init-ns "hive-rss.addon"}
                 {:addon/id "hive.bad" :addon/init-ns "hive-bad.addon"}]
        facts   (with-redefs [addon-core/list-addons (constantly [])]
                  (loader/roster-facts
                   ordered
                   {:report {:mounted [{:addon/id "hive.carto" :success? true}
                                       {:addon/id "hive.bad" :success? false}]}
                    :lifecycle {:dormant ["hive.keg"]}}
                   #{'hive-rss.addon}))]
    (is (= ["hive.carto" "hive.keg" "hive.rss"] (:mounted facts)))
    (is (= ["hive.bad"] (:failed facts)))))
