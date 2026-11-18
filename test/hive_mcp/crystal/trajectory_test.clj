(ns hive-mcp.crystal.trajectory-test
  "The `trajectory` catchup block (SYNTHESIS-HISTORY slice (d)).

     1. Pure: trajectory-lines, budget-lines (golden cases and a property: the
        cap is never exceeded and the kept rows are a prefix).
     2. gather-trajectory over a map-backed ISummaryHistory: newest first,
        default 3, fail-soft on a nil port, a throwing port and no summary.
     3. Rendering: the section reaches a catchup answer as `_block trajectory`
        through the host's contributed-sections, and a nil value emits none.
   No with-redefs: every port is a reify."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.crystal.history :as h]
            [hive-mcp.crystal.trajectory :as t]
            [hive-mcp.tools.catchup.format :as fmt]
            [hive-spi.catchup.registry :as blocks]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- sid [day n] (format "%s090000-%08d" day n))

(def ^:private d1 (sid "20261115" 1))
(def ^:private d2 (sid "20261116" 2))
(def ^:private d3 (sid "20261117" 3))
(def ^:private d4 (sid "20261118" 4))

(defn- summary [id] {:id id :project-id "a" :tags ["session-summary"]})

(defn- map-port [summaries edges entries]
  (reify h/ISummaryHistory
    (-summaries [_ pid] (get summaries pid []))
    (-superseded-by [_ id] (get edges id []))
    (-entry [_ id] (get entries id))))

(def ^:private chain-port
  (map-port {"a" (mapv summary [d1 d2 d3 d4])}
            {d4 [d3] d3 [d2] d2 [d1]}
            {d4 {:id d4 :tags ["session:2026-11-18"] :content "## Session Synthesis\n- fourth"}
             d3 {:id d3 :tags ["session:2026-11-17"] :content "third"}
             d2 {:id d2 :tags ["session:2026-11-16"] :content "second"}
             d1 {:id d1 :tags ["session:2026-11-15"] :content "first"}}))

;; =============================================================================
;; Pure
;; =============================================================================

(deftest trajectory-lines-skip-what-has-no-text
  (is (= [{:id d3 :line "2026-11-17  third"}]
         (t/trajectory-lines {:entries [{:id d3 :date "2026-11-17" :digest "third"}
                                        {:id d2 :date "2026-11-16" :digest nil :missing true}
                                        {:id d1 :date "2026-11-15" :digest "  "}]})))
  (is (= [{:id d1 :line "????-??-??  x"}]
         (t/trajectory-lines {:entries [{:id d1 :date nil :digest "x"}]})))
  (is (= [] (t/trajectory-lines {:entries []})))
  (is (= [] (t/trajectory-lines nil))))

(deftest budget-lines-keeps-a-prefix-under-the-cap
  (let [rows [{:id "a" :line "12345"} {:id "b" :line "12345"} {:id "c" :line "12345"}]]
    (is (= {:rows (subvec rows 0 2) :truncated true} (t/budget-lines rows 12)))
    (is (= {:rows rows :truncated false} (t/budget-lines rows 15)))
    (testing "an oversized first line is cut, never dropped"
      (is (= {:rows [{:id "a" :line "123…"}] :truncated true} (t/budget-lines rows 4))))
    (is (= {:rows [] :truncated false} (t/budget-lines [] 10)))))

(deftest budget-lines-property
  (let [row (gen/fmap (fn [[id s]] {:id id :line s})
                      (gen/tuple gen/string-alphanumeric gen/string-ascii))
        r (tc/quick-check
           200
           (prop/for-all [rows (gen/vector row 0 8)
                          cap  (gen/choose 1 60)]
             (let [{kept :rows truncated :truncated} (t/budget-lines rows cap)
                   total (reduce + (map (comp count :line) kept))]
               (and (<= total cap)
                    (if (seq rows) (seq kept) (empty? kept))
                    ;; ids are a prefix of the input ids
                    (= (map :id kept) (take (count kept) (map :id rows)))
                    (= truncated (not= kept (vec rows)))))))]
    (is (:pass? r) (pr-str (:shrunk r)))))

;; =============================================================================
;; gather-trajectory
;; =============================================================================

(deftest gather-is-the-last-three-newest-first
  (let [s (t/gather-trajectory chain-port "a" nil)]
    (is (= ["2026-11-18  fourth" "2026-11-17  third" "2026-11-16  second"] (:trajectory s)))
    (is (= [d4 d3 d2] (:ids s)))
    (is (string? (:hint s)))
    (is (not (contains? s :truncated))))
  (testing "n is honoured"
    (is (= [d4] (:ids (t/gather-trajectory chain-port "a" 1))))))

(deftest gather-respects-the-char-cap
  (let [long-port (map-port {"a" [(summary d2)]}
                            {d2 [d1]}
                            {d2 {:id d2 :content (apply str (repeat 110 "y"))}
                             d1 {:id d1 :content (apply str (repeat 110 "z"))}})
        s (t/gather-trajectory long-port "a" 100)]
    (is (<= (reduce + (map count (:trajectory s))) t/char-cap))))

(deftest gather-fails-soft
  (testing "no port: no block"
    (is (nil? (t/gather-trajectory nil "a" 3))))
  (testing "no project: no block"
    (is (nil? (t/gather-trajectory chain-port nil 3)))
    (is (nil? (t/gather-trajectory chain-port "" 3))))
  (testing "no summary: no block"
    (is (nil? (t/gather-trajectory chain-port "nobody" 3))))
  (testing "a throwing port: no block, no throw"
    (let [broken (reify h/ISummaryHistory
                   (-summaries [_ _] (throw (ex-info "store down" {})))
                   (-superseded-by [_ _] (throw (ex-info "kg down" {})))
                   (-entry [_ _] (throw (ex-info "gone" {}))))]
      (is (nil? (t/gather-trajectory broken "a" 3)))))
  (testing "a broken edge read keeps what was walked"
    (let [half (reify h/ISummaryHistory
                 (-summaries [_ _] [(summary d4)])
                 (-superseded-by [_ _] (throw (ex-info "kg down" {})))
                 (-entry [_ id] {:id id :content "head only"}))]
      (is (= [d4] (:ids (t/gather-trajectory half "a" 3)))))))

;; =============================================================================
;; Block + rendering
;; =============================================================================

(deftest block-shape-and-registration
  (is (= :trajectory (:block/id t/block)))
  (is (blocks/block? t/block))
  (is (= #{"session-summary"} (get-in t/block [:block/cache :drop-tags])))
  (testing "block-value never throws, whatever is (not) up"
    (is (or (nil? (t/block-value {:project-id "no-such-project-trajectory-test"}))
            (map? (t/block-value {:project-id "no-such-project-trajectory-test"}))))
    (is (nil? (t/block-value {})))))

(deftest section-renders-as-a-trajectory-block
  (let [[sec] (fmt/contributed-sections [[:trajectory (t/gather-trajectory chain-port "a" nil)]])
        parsed (json/read-str (:text sec) :key-fn keyword)]
    (is (= "trajectory" (:_block parsed)))
    (is (= #{:_block :trajectory :ids :hint} (set (keys parsed))))
    (is (every? #(str/starts-with? % "2026-11-") (:trajectory parsed))))
  (testing "nothing to show: no section at all"
    (is (= [] (fmt/contributed-sections [[:trajectory (t/gather-trajectory nil "a" 3)]])))))
