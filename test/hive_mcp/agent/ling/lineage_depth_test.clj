(ns hive-mcp.agent.ling.lineage-depth-test
  "Trifecta for `hive-mcp.agent.ling.spawn/lineage-depth`: the depth a child of
   PARENT sits at, the parent's registry depth plus one; a coordinator, a blank
   or an unknown parent is depth 0, so the child is 1.

   Driven through the injected `get-slave` port (the 2-arity), over generated
   registries. No redefinition.

     golden    : depth per parent shape over a fixed registry
     property  : a recorded depth d answers d+1; a chain without recorded
                 depths answers hops-to-the-recorded-ancestor + depth + 1
     mutation  : no increment, a constant, or a hardcoded depth is caught"
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.agent.ling.spawn :as spawn]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private registry
  {"ling-1" {:slave/id "ling-1" :slave/depth 1 :slave/parent "coordinator:t"}
   "ling-2" {:slave/id "ling-2" :slave/depth 2 :slave/parent "ling-1"}
   "ling-3" {:slave/id "ling-3" :slave/depth 3 :slave/parent "ling-2"}
   ;; a rehydrated row without :slave/depth: derived from the parent chain
   "ling-r" {:slave/id "ling-r" :slave/parent "ling-2"}})

(defn- depth [parent] (spawn/lineage-depth registry parent))

(deftest-golden lineage-depth-golden
  "test/golden/hive-mcp/agent/ling/lineage-depth.edn"
  (into (sorted-map)
        (for [[label parent] {:coordinator "coordinator:t" :nil nil :blank "  "
                              :unknown "ghost" :depth-1 "ling-1" :depth-2 "ling-2"
                              :depth-3 "ling-3" :derived "ling-r"}]
          [label (depth parent)])))

(deftest child-of-a-recorded-depth-sits-one-below
  (is (= 2 (depth "ling-1")))
  (is (= 4 (depth "ling-3"))))

(deftest a-coordinator-or-unknown-parent-makes-a-depth-1-child
  (doseq [p ["coordinator:t" nil "" "ghost"]]
    (is (= 1 (depth p)) (pr-str p))))

(deftest the-default-port-is-the-live-registry
  (is (pos? (spawn/lineage-depth "no-such-ling-in-the-registry"))))

(defspec a-recorded-depth-d-answers-d-plus-1 200
  (prop/for-all [d (gen/choose 1 20)]
    (= (inc d) (spawn/lineage-depth {"p" {:slave/id "p" :slave/depth d}} "p"))))

(defspec a-depthless-chain-adds-one-per-hop 100
  (prop/for-all [d (gen/choose 1 10) hops (gen/choose 1 6)]
    (let [ids  (mapv #(str "n" %) (range hops))
          rows (into {"root" {:slave/id "root" :slave/depth d}}
                     (map-indexed (fn [i id]
                                    [id {:slave/id id
                                         :slave/parent (if (zero? i) "root" (nth ids (dec i)))}])
                                  ids))]
      (= (+ d hops 1) (spawn/lineage-depth rows (last ids))))))

(defspec the-answer-is-always-a-positive-integer 200
  (prop/for-all [parent (gen/one-of [gen/string-alphanumeric (gen/return nil)
                                     (gen/elements ["ling-1" "ling-3" "coordinator:x"])])]
    (let [n (depth parent)] (and (integer? n) (pos? n)))))

(hmut/deftest-mutations lineage-depth-mutations
  hive-mcp.agent.ling.spawn/lineage-depth
  [["no increment"      (fn [& _] 0)]
   ["always 1"          (fn [& _] 1)]
   ["parent depth only" (fn [get-slave p] (long (or (:slave/depth (get-slave p)) 0)))]
   ["plus two"          (fn [get-slave p] (+ 2 (long (or (:slave/depth (get-slave p)) 0))))]]
  (fn []
    (is (= 2 (spawn/lineage-depth registry "ling-1")))
    (is (= 4 (spawn/lineage-depth registry "ling-3")))
    (is (= 1 (spawn/lineage-depth registry "coordinator:t")))
    (is (= 4 (spawn/lineage-depth registry "ling-r")))))
