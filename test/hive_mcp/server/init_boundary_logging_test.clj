(ns hive-mcp.server.init-boundary-logging-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private src (io/file "src/hive_mcp/server/init.clj"))
(def ^:private lines (delay (str/split-lines (slurp src))))

(def ^:private moved-src
  "Where the embedding boot step went when it left the kernel: its nested
   rescues moved with it and are part of the same invariant."
  (io/file "src/hive_mcp/embeddings/boot.clj"))

(def ^:private moved-lines (delay (str/split-lines (slurp moved-src))))

(def ^:private nested-rescue-re
  "A rescue nested INSIDE a boundary: indented three or more columns."
  #"^\s{3,}\(result/rescue (nil|false)")

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
    ;; The invariant is that nothing NESTED was converted to rescue-log, not
    ;; that a particular file holds them. Three of the original eight moved
    ;; with init-embedding-provider! when the embedding wiring left the kernel
    ;; for hive-mcp.embeddings.boot. One was DELETED, not converted: the
    ;; auto-heal listener registration is now a keyed, idempotent call whose
    ;; failure lands in init-hot-reload-watcher!'s own rescue, so seven remained.
    ;; Two more were DELETED with the code they guarded: the requires of
    ;; hive-mcp.hot.state and hive-mcp.hot.silence, namespaces that no longer
    ;; exist, so every boot swallowed the same failure. Five remain.
    (is (= 2 (count (filter #(re-find nested-rescue-re %) @lines))))
    (is (= 3 (count (filter #(re-find nested-rescue-re %) @moved-lines))))
    (is (= 5 (+ (count (filter #(re-find nested-rescue-re %) @lines))
                (count (filter #(re-find nested-rescue-re %) @moved-lines)))))))
