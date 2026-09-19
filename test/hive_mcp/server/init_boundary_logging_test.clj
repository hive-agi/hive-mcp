(ns hive-mcp.server.init-boundary-logging-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private src (io/file "src/hive_mcp/server/init.clj"))
(def ^:private lines (delay (str/split-lines (slurp src))))

(deftest source-is-readable
  (testing "non-vacuity: an empty read would make every assertion below pass"
    (is (.exists src))
    (is (< 500 (count @lines)))))

(deftest every-top-level-init-boundary-logs
  (testing "no bare rescue at column 3 is left"
    (is (empty? (filter #(re-find #"^  \(result/rescue (nil|false)" %) @lines))))
  (testing "and they became rescue-log"
    (is (<= 20 (count (filter #(re-find #"^  \(result/rescue-log " %) @lines))))))

(deftest nested-rescues-are-untouched
  (testing "the deeper rescue sites are not init boundaries and must survive"
    (is (= 8 (count (filter #(re-find #"^\s{3,}\(result/rescue (nil|false)" %) @lines))))))
