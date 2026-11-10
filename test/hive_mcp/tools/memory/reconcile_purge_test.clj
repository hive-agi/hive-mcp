(ns hive-mcp.tools.memory.reconcile-purge-test
  "`memory reconcile purge=true`: the opt-in purge of orphan VECTOR records
   (card 20260723023019-14943893, follow-up (a)). Port stubs only; no live
   store is reached. Each stub records every delete it is asked for and
   keeps its entry-store ids in an atom nothing in the verb can write, so
   every test can assert the source of truth was not touched."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.memory.reconcile :as core]
            [hive-mcp.memory.reconcile-sources :as rsrc]
            [hive-mcp.tools.consolidated.memory :as cmem]
            [hive-mcp.tools.memory.reconcile :as rec]
            [hive-mcp.tools.memory.schema :as ms]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.memory.ports :as ports]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [t] (rec/clear-sources!) (try (t) (finally (rec/clear-sources!)))))

(defn- purgeable
  "Two-sided sources with a vector-side purge. STORE and VECTOR are atoms of
   id vectors; DELETED collects every id batch purge-vector-ids! received.
   ON-STORE-READ, when given, runs before each store-ids answer (to model a
   write landing between the report and the purge)."
  ([store vector deleted] (purgeable store vector deleted nil))
  ([store vector deleted on-store-read]
   (reify
     core/IReconcileSources
     (store-ids [_] (when on-store-read (on-store-read)) @store)
     (vector-ids [_] @vector)
     core/IOrphanVectorPurge
     (purge-vector-ids! [_ ids]
       (swap! deleted conj (vec ids))
       (swap! vector (fn [v] (vec (remove (set ids) v))))
       (count ids)))))

(defn- answer [resp] (json/read-str (:text resp) :key-fn keyword))

(def ^:private store-ids ["a" "b" "c"])
(def ^:private vector-ids ["a" "b" "ghost-1" "ghost-2" nil " "])

(defn- fixture []
  (let [store   (atom store-ids)
        vector  (atom vector-ids)
        deleted (atom [])]
    {:store store :vector vector :deleted deleted
     :sources (purgeable store vector deleted)}))

(deftest without-purge-the-verb-stays-read-only
  (let [{:keys [sources deleted store]} (fixture)]
    (rec/set-sources! sources)
    (let [out (answer (rec/handle-reconcile {}))]
      (is (= "ok" (:status out)))
      (is (= ["ghost-1" "ghost-2"] (:orphan-vector-ids out)))
      (is (not (contains? out :purge)) "no purge block unless asked")
      (is (= [] @deleted))
      (is (= store-ids @store)))))

(deftest purge-alone-is-a-dry-run
  (doseq [confirm [nil false "true" "yes" 1]]
    (let [{:keys [sources deleted store vector]} (fixture)]
      (rec/set-sources! sources)
      (let [p (:purge (answer (rec/handle-reconcile (cond-> {:purge true}
                                                      (some? confirm) (assoc :i_mean_it confirm)))))]
        (testing (str "i_mean_it " (pr-str confirm) " is not literal true")
          (is (= "dry-run" (:mode p)))
          (is (= 2 (:candidate-count p)))
          (is (= 2 (:skipped-nil-ids p)) "nil and blank ids are reported, never purged")
          (is (zero? (:purged p)))
          (is (= [] @deleted))
          (is (= vector-ids @vector))
          (is (= store-ids @store)))))))

(deftest purge-with-i-mean-it-deletes-only-orphan-vectors
  (let [{:keys [sources deleted store vector]} (fixture)]
    (rec/set-sources! sources)
    (let [out (answer (rec/handle-reconcile {:purge "true" :i_mean_it true}))
          p   (:purge out)]
      (is (= "applied" (:mode p)))
      (is (= 2 (:purged p) (:planned-count p) (:candidate-count p)))
      (is (zero? (:kept-count p)))
      (is (= [["ghost-1" "ghost-2"]] @deleted) "one delete, of the orphans only")
      (is (= store-ids @store) "the entry store is never written")
      (is (= ["a" "b" nil " "] @vector) "store-backed vectors and id-less rows stay")
      (is (m/validate ms/ReconcilePurgeResponse out)))))

(deftest an-entry-written-before-the-delete-is-kept
  (let [store   (atom store-ids)
        vector  (atom vector-ids)
        deleted (atom [])
        reads   (atom 0)
        ;; The second store read (the fresh one before deleting) sees ghost-2
        ;; land in the entry store.
        sources (purgeable store vector deleted
                           #(when (= 2 (swap! reads inc)) (swap! store conj "ghost-2")))
        p       (:purge (rec/purge-response sources nil true))]
    (is (= 2 @reads) "the store is re-read before the delete")
    (is (= [["ghost-1"]] @deleted))
    (is (= 1 (:purged p)))
    (is (= 1 (:kept-count p)))
    (is (some #{"ghost-2"} @vector))))

(deftest nothing-to-purge-makes-no-delete-call
  (let [store   (atom ["a"])
        vector  (atom ["a"])
        deleted (atom [])
        p (:purge (rec/purge-response (purgeable store vector deleted) 10 true))]
    (is (= "applied" (:mode p)))
    (is (zero? (:purged p)))
    (is (= [] @deleted))))

(deftest sources-without-a-vector-purge-are-unsupported
  (rec/set-sources! (reify core/IReconcileSources
                      (store-ids [_] ["a"])
                      (vector-ids [_] ["a" "ghost"])))
  (let [p (:purge (answer (rec/handle-reconcile {:purge true :i_mean_it true})))]
    (is (= "unsupported" (:mode p)))
    (is (= 1 (:candidate-count p)))
    (is (zero? (:purged p)))))

(deftest the-default-store-scan-cannot-purge
  (let [calls (atom 0)
        store (reify ports/IMemoryStoreScan
                (scan-ids [_ _] (swap! calls inc) ["x" nil]))]
    (with-redefs-fn {#'rec/active-store (constantly store)}
      (fn []
        (let [out (answer (rec/handle-reconcile {:purge true :i_mean_it true}))]
          (is (= "unsupported" (get-in out [:purge :mode])))
          (is (= 1 (get-in out [:purge :skipped-nil-ids])))
          (is (= 1 @calls) "read once, nothing else asked of the store"))))))

(deftest a-partial-paged-listing-is-refused
  (testing "complete-listing? reads exhausted? of paged sources"
    (let [cut  (rsrc/paged-sources (fn [_ n] (vec (repeat n "s"))) (fn [_ _] []) {:page-size 2 :max-pages 1})
          full (rsrc/paged-sources (fn [_ _] ["s"]) (fn [_ _] []) {:page-size 2})]
      (core/report cut)
      (core/report full)
      (is (false? (#'rec/complete-listing? cut)))
      (is (true? (#'rec/complete-listing? full)))
      (is (true? (#'rec/complete-listing? (reify core/IReconcileSources
                                            (store-ids [_] [])
                                            (vector-ids [_] [])))))))
  (testing "a purge over a cut listing deletes nothing"
    (let [{:keys [sources deleted]} (fixture)]
      (with-redefs-fn {#'rec/complete-listing? (constantly false)}
        (fn []
          (let [p (:purge (rec/purge-response sources nil true))]
            (is (= "refused" (:mode p)))
            (is (zero? (:purged p)))
            (is (= [] @deleted))))))))

(deftest a-failing-delete-is-a-failed-reconcile
  (rec/set-sources! (reify
                      core/IReconcileSources
                      (store-ids [_] [])
                      (vector-ids [_] ["ghost"])
                      core/IOrphanVectorPurge
                      (purge-vector-ids! [_ _] (throw (ex-info "index down" {})))))
  (is (:isError (rec/handle-reconcile {:purge true :i_mean_it true}))))

(deftrifecta-from-schema purge-plan-never-names-a-store-id
  hive-mcp.memory.reconcile/purge-plan
  {:in  [:cat [:vector {:max 4} [:maybe [:enum "a" "b" "c" ""]]]
         [:vector {:max 4} [:maybe [:enum "a" "b" "x" "y" " "]]]]
   :out [:vector [:string {:min 1}]]
   :rel (fn [[store candidates] out]
          (let [s (set (keep #(when (and % (not (str/blank? %))) %) store))]
            (and (not-any? s out)
                 (every? (set candidates) out)
                 (= out (vec (sort (distinct out)))))))
   :num-tests 200})

(deftest the-help-advertises-reconcile-and-its-purge-params
  (let [props (get-in cmem/tool-def [:inputSchema :properties])]
    (is (str/includes? (:description cmem/tool-def) "reconcile"))
    (is (str/includes? (:description cmem/tool-def) "i_mean_it"))
    (is (contains? props "purge"))
    (is (contains? props "i_mean_it")))
  (is (some #{"- reconcile"}
            (map str/trim (str/split-lines (:text (cmem/handle-memory {:command "help"})))))))
