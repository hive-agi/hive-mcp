(ns hive-mcp.tools.catchup.carto-status-block-test
  "Regression for friction 20261109101930-6115ab49: the catchup carto-status
   block must carry the provider's cached last known count and its age, so a
   session whose live count is slow still sees the index size."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.catchup.format :as fmt]))

(deftest cached-count-rides-along
  (let [b (fmt/carto-status-block {:readiness :ready :indexed-forms nil
                                   :indexed-forms-cached 93909
                                   :cached-at 1794324000000
                                   :cached-age-ms 6000
                                   :indexed-forms-pending? true
                                   :indexed-forms-populated? true})]
    (is (= "carto-status" (:_block b)))
    (is (nil? (:indexed-forms b)))
    (is (= 93909 (:indexed-forms-cached b)))
    (is (= 1794324000000 (:cached-at b)))
    (is (= 6000 (:cached-age-ms b)))
    (is (true? (:indexed-forms-pending? b)))
    (is (true? (:indexed-forms-populated? b)))))

(deftest absent-cache-keys-stay-absent
  (testing "a provider that reports no cache fabricates no cache keys"
    (let [b (fmt/carto-status-block {:readiness :ready :indexed-forms 12})]
      (is (= 12 (:indexed-forms b)))
      (is (= [] (:warnings b)))
      (is (not-any? #(contains? b %) [:indexed-forms-cached :cached-at :cached-age-ms
                                       :indexed-forms-pending?])))))

(deftest build-response-uses-the-block
  (let [blocks (fmt/build-catchup-response
                {:project-id "p"
                 :carto-status {:readiness :ready :indexed-forms nil
                                :indexed-forms-cached 5 :cached-at 1 :cached-age-ms 2}})
        text   (some #(when (re-find #"carto-status" (str (:text %))) (:text %)) blocks)]
    (is (some? text))
    (is (re-find #"indexed-forms-cached" text))))
