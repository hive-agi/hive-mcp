(ns hive-mcp.tools.kg.derived-excess-test
  "prune-derived-excess! is dry-run by default, refuses without :i-mean-it,
   refuses when the snapshot fails, and only then retracts the weakest
   :co-accessed edges past the per-node cap [NATIVE-RSS-0.4]. Every test runs
   on an ephemeral in-memory store; none touches a live KG."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.knowledge-graph.edges.m14-support :as sup]
            [hive-mcp.knowledge-graph.edges.queries :as queries]
            [hive-mcp.knowledge-graph.edges.write :as write]
            [hive-mcp.tools.kg.derived-excess :as dx]
            [hive-schemas.test :as hst]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private node [:enum "a" "b" "c" "d" "e"])

(defn- degrees [edges]
  (frequencies (mapcat (juxt :kg-edge/from :kg-edge/to) edges)))

(hst/deftrifecta-from-schema excess-co-access-ids-contract
  hive-mcp.tools.kg.derived-excess/excess-co-access-ids
  {:in [:cat [:vector {:max 12}
              [:map [:kg-edge/id [:string {:min 1 :max 4}]]
               [:kg-edge/from node] [:kg-edge/to node]
               [:kg-edge/confidence [:double {:min 0.0 :max 1.0}]]]]
        [:int {:min 0 :max 4}]]
   :out [:vector :string]
   :rel (fn [[edges cap] out]
          (let [dropped (frequencies out)
                kept (reduce (fn [[ks d] e]
                               (if (pos? (get d (:kg-edge/id e) 0))
                                 [ks (update d (:kg-edge/id e) dec)]
                                 [(conj ks e) d]))
                             [[] dropped] edges)]
            ;; Store edge ids are unique; with a generated duplicate id the
            ;; oracle cannot tell which copy was dropped, so only the count
            ;; is checked then.
            (and (= (count edges) (+ (count out) (count (first kept))))
                 (or (not (apply distinct? nil (map :kg-edge/id edges)))
                     (every? #(<= % cap) (vals (degrees (first kept))))))))
   :classify (fn [_ out] (boolean (seq out)))
   :classify-domain #{true false}
   :num-tests 200})

(defn- co-edge! [from to conf]
  (write/add-edge! {:from from :to to :relation :co-accessed
                    :confidence conf :source-type :co-access}))

(defn- seed-hub! []
  (doseq [[i c] (map vector (range 5) [0.9 0.8 0.7 0.2 0.1])]
    (co-edge! "hub" (str "n" i) c)))

(defn- co-count [] (count (queries/get-edges-by-relation :co-accessed nil)))

(def ^:private never-snapshot
  (fn [] (throw (ex-info "snapshot must not be taken on this path" {}))))

(deftest dry-run-by-default-reports-and-deletes-nothing
  (sup/in-fresh-store
   (fn []
     (seed-hub!)
     (let [r (dx/prune-derived-excess! {:max-degree 3 :snapshot-fn never-snapshot})]
       (is (:dry-run? r))
       (is (= 2 (get-in r [:plan :co-accessed-excess])))
       (is (nil? (:deleted r)))
       (is (= 5 (co-count)))))))

(deftest apply-without-i-mean-it-is-refused
  (sup/in-fresh-store
   (fn []
     (seed-hub!)
     (let [r (dx/prune-derived-excess! {:max-degree 3 :dry-run? false
                                        :snapshot-fn never-snapshot})]
       (is (:refused? r))
       (is (= 5 (co-count)))))))

(deftest failed-snapshot-refuses-the-delete
  (sup/in-fresh-store
   (fn []
     (seed-hub!)
     (let [r (dx/prune-derived-excess! {:max-degree 3 :dry-run? false :i-mean-it true
                                        :snapshot-fn (fn [] {:success false})})]
       (is (:refused? r))
       (is (= 5 (co-count)))))))

(deftest guarded-apply-snapshots-first-then-drops-the-weakest
  (sup/in-fresh-store
   (fn []
     (seed-hub!)
     (let [calls (atom [])
           r (dx/prune-derived-excess!
              {:max-degree 3 :dry-run? false :i-mean-it true
               :snapshot-fn (fn [] (swap! calls conj [:snapshot (co-count)])
                              {:success true :path "/tmp/fake"})})]
       (testing "the snapshot ran while every edge still existed"
         (is (= [[:snapshot 5]] @calls)))
       (is (= 2 (get-in r [:deleted :co-accessed-removed])))
       (is (= 3 (co-count)))
       (is (some? (queries/find-edge "hub" "n0" :co-accessed)))
       (is (nil? (queries/find-edge "hub" "n4" :co-accessed)))))))
