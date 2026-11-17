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
            [hive-mcp.multi.param-coerce :as param-coerce]
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
  "Two-sided sources with a vector-side purge that report their listings
   complete. STORE and VECTOR are atoms of id vectors; DELETED collects
   every id batch purge-vector-ids! received. Opts:
     :on-store-read  runs before each store-ids answer (to model a write, a
                     fault or a cut landing between the report and the purge)
     :complete?      an atom read by listings-complete? (default: true)
     :purge-answer   (fn [ids]) what the port answers (default: the count)"
  ([store vector deleted] (purgeable store vector deleted {}))
  ([store vector deleted {:keys [on-store-read complete? purge-answer]
                          :or   {complete? (atom true) purge-answer count}}]
   (reify
     core/IReconcileSources
     (store-ids [_] (when on-store-read (on-store-read)) @store)
     (vector-ids [_] @vector)
     core/IListingCompleteness
     (listings-complete? [_] @complete?)
     core/IOrphanVectorPurge
     (purge-vector-ids! [_ ids]
       (swap! deleted conj (vec ids))
       (swap! vector (fn [v] (vec (remove (set ids) v))))
       (purge-answer ids)))))

(defn- mute-purgeable
  "Sources with a vector-side purge and NO IListingCompleteness port: they
   cannot say whether their listings reached the end."
  [store vector deleted]
  (reify
    core/IReconcileSources
    (store-ids [_] @store)
    (vector-ids [_] @vector)
    core/IOrphanVectorPurge
    (purge-vector-ids! [_ ids] (swap! deleted conj (vec ids)) (count ids))))

(defn- paged-purgeable
  "A purge port over real PAGED sources: listing and completeness are the
   paged ones, the purge records what it was asked to delete."
  [paged deleted]
  (reify
    core/IReconcileSources
    (store-ids [_] (core/store-ids paged))
    (vector-ids [_] (core/vector-ids paged))
    core/IListingCompleteness
    (listings-complete? [_] (core/listings-complete? paged))
    core/IOrphanVectorPurge
    (purge-vector-ids! [_ ids] (swap! deleted conj (vec ids)) (count ids))))

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
        (testing (str "i_mean_it " (pr-str confirm) " reaching the handler is not boolean true")
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
      (is (= ["ghost-1" "ghost-2"] (:purged-ids p)) "what was handed to the port")
      (is (= "before-purge" (:orphan-vector-ids-as-of p)))
      (is (= ["ghost-1" "ghost-2"] (:orphan-vector-ids out))
          "the listed orphans are the pre-purge candidates, labelled as such")
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
                           {:on-store-read
                            #(when (= 2 (swap! reads inc)) (swap! store conj "ghost-2"))})
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
    ;; The sources the verb derives from a scanning store, installed through
    ;; the public seam instead of redefining the active-store lookup.
    (rec/set-sources! (rec/store-sources store))
    (let [out (answer (rec/handle-reconcile {:purge true :i_mean_it true}))]
      (is (= "unsupported" (get-in out [:purge :mode])))
      (is (= 1 (get-in out [:purge :skipped-nil-ids])))
      (is (= 1 @calls) "read once, nothing else asked of the store"))))

(deftest paged-sources-report-their-own-completeness
  (let [cut  (rsrc/paged-sources (fn [_ n] (vec (repeat n "s"))) (fn [_ _] []) {:page-size 2 :max-pages 1})
        full (rsrc/paged-sources (fn [_ _] ["s"]) (fn [_ _] []) {:page-size 2})]
    (is (satisfies? core/IListingCompleteness full))
    (is (false? (core/listings-complete? full)) "nothing read yet: not known to be complete")
    (core/report cut)
    (core/report full)
    (is (false? (core/listings-complete? cut)))
    (is (true? (core/listings-complete? full)))
    (is (= (rsrc/exhausted? full) (core/listings-complete? full)))))

(deftest a-cut-paged-listing-is-refused
  (testing "a purge-capable adapter whose store listing was cut at :max-pages"
    (let [deleted (atom [])
          ;; The store holds a, b, c, d; the cut listing shows only a and b,
          ;; so c and d look like orphans.
          store   ["a" "b" "c" "d"]
          paged   (rsrc/paged-sources (fn [off n] (subvec store off (min (count store) (+ off n))))
                                      (fn [off _] (if (zero? off) ["a" "b" "c" "d"] []))
                                      {:page-size 2 :max-pages 1})
          p       (:purge (rec/purge-response (paged-purgeable paged deleted) nil true))]
      (is (= "refused" (:mode p)))
      (is (str/includes? (:reason p) "stopped before its end"))
      (is (zero? (:purged p)))
      (is (= [] @deleted) "c and d are store ids beyond the cut and are not deleted")))
  (testing "the same adapter read to the end purges only the real orphan"
    (let [deleted (atom [])
          store   ["a" "b" "c"]
          paged   (rsrc/paged-sources (fn [off n] (subvec store (min off (count store))
                                                         (min (count store) (+ off n))))
                                      (fn [off _] (if (zero? off) ["a" "b" "c" "ghost"] []))
                                      {:page-size 5})
          p       (:purge (rec/purge-response (paged-purgeable paged deleted) nil true))]
      (is (= "applied" (:mode p)))
      (is (= [["ghost"]] @deleted)))))

(deftest sources-that-cannot-vouch-for-their-listings-are-refused
  (let [store   (atom store-ids)
        vector  (atom vector-ids)
        deleted (atom [])]
    (doseq [confirm? [true false]]
      (let [p (:purge (rec/purge-response (mute-purgeable store vector deleted) nil confirm?))]
        (is (= "refused" (:mode p)) "no IListingCompleteness port: fail closed")
        (is (str/includes? (:reason p) "IListingCompleteness"))
        (is (zero? (:purged p)))))
    (is (= [] @deleted))
    (is (= vector-ids @vector))))

(deftest sources-reporting-an-incomplete-listing-are-refused
  (testing "incomplete on the first listing"
    (let [store   (atom store-ids)
          vector  (atom vector-ids)
          deleted (atom [])
          p (:purge (rec/purge-response
                     (purgeable store vector deleted {:complete? (atom false)}) nil true))]
      (is (= "refused" (:mode p)))
      (is (= [] @deleted))))
  (testing "anything but a literal true is not complete"
    (let [deleted (atom [])
          p (:purge (rec/purge-response
                     (purgeable (atom store-ids) (atom vector-ids) deleted {:complete? (atom nil)})
                     nil true))]
      (is (= "refused" (:mode p)))
      (is (= [] @deleted))))
  (testing "the fresh re-read before the delete is cut"
    (let [store     (atom store-ids)
          vector    (atom vector-ids)
          deleted   (atom [])
          complete? (atom true)
          reads     (atom 0)
          sources   (purgeable store vector deleted
                               {:complete?     complete?
                                :on-store-read #(when (= 2 (swap! reads inc))
                                                  (reset! complete? false))})
          p         (:purge (rec/purge-response sources nil true))]
      (is (= 2 @reads))
      (is (= "refused" (:mode p)))
      (is (= [] @deleted)))))

(deftest an-empty-store-listing-is-refused
  (testing "the store side answers nothing while vectors exist"
    (doseq [empty-listing [[] nil [nil " "]]
            confirm?      [true false]]
      (let [vector  (atom ["v1" "v2" "v3"])
            deleted (atom [])
            p (:purge (rec/purge-response
                       (purgeable (atom empty-listing) vector deleted) nil confirm?))]
        (is (= "refused" (:mode p)) (pr-str empty-listing))
        (is (= 3 (:candidate-count p)))
        (is (str/includes? (:reason p) "empty"))
        (is (zero? (:purged p)))
        (is (= [] @deleted))
        (is (= ["v1" "v2" "v3"] @vector)))))
  (testing "the fresh re-read comes back empty (store dropped between the reads)"
    (let [store   (atom store-ids)
          vector  (atom vector-ids)
          deleted (atom [])
          reads   (atom 0)
          sources (purgeable store vector deleted
                             {:on-store-read #(when (= 2 (swap! reads inc)) (reset! store []))})
          p       (:purge (rec/purge-response sources nil true))]
      (is (= 2 @reads))
      (is (= "refused" (:mode p)))
      (is (str/includes? (:reason p) "empty"))
      (is (= [] @deleted))
      (is (= vector-ids @vector))))
  (testing "an empty store with no vector candidates is not a refusal"
    (let [deleted (atom [])
          p (:purge (rec/purge-response (purgeable (atom []) (atom []) deleted) nil true))]
      (is (= "applied" (:mode p)))
      (is (= [] @deleted)))))

(deftest a-plan-above-the-safety-cap-is-refused
  (is (= 1/2 core/max-purge-fraction))
  (testing "2 of 3 vector ids is more than half"
    (doseq [confirm? [true false]]
      (let [vector  (atom ["a" "ghost-1" "ghost-2"])
            deleted (atom [])
            p (:purge (rec/purge-response (purgeable (atom ["a"]) vector deleted) nil confirm?))]
        (is (= "refused" (:mode p)))
        (is (str/includes? (:reason p) "safety cap"))
        (is (= 2 (:candidate-count p)))
        (is (= [] @deleted))
        (is (= ["a" "ghost-1" "ghost-2"] @vector)))))
  (testing "exactly half is allowed (the fixture: 2 orphans of 4 usable vector ids)"
    (let [{:keys [sources deleted]} (fixture)]
      (is (= "applied" (:mode (:purge (rec/purge-response sources nil true)))))
      (is (= [["ghost-1" "ghost-2"]] @deleted)))))

(deftest a-listing-that-throws-is-an-error-never-an-empty-list
  (testing "the first store listing throws"
    (let [deleted (atom [])]
      (rec/set-sources! (purgeable (atom store-ids) (atom vector-ids) deleted
                                   {:on-store-read #(throw (ex-info "store down" {}))}))
      (is (:isError (rec/handle-reconcile {:purge true :i_mean_it true})))
      (is (= [] @deleted))))
  (testing "the fresh re-read throws"
    (let [deleted (atom [])
          reads   (atom 0)]
      (rec/set-sources! (purgeable (atom store-ids) (atom vector-ids) deleted
                                   {:on-store-read #(when (= 2 (swap! reads inc))
                                                      (throw (ex-info "store down" {})))}))
      (is (:isError (rec/handle-reconcile {:purge true :i_mean_it true})))
      (is (= 2 @reads))
      (is (= [] @deleted))))
  (testing "the vector listing throws"
    (let [deleted (atom [])]
      (rec/set-sources! (reify
                          core/IReconcileSources
                          (store-ids [_] store-ids)
                          (vector-ids [_] (throw (ex-info "index down" {})))
                          core/IListingCompleteness
                          (listings-complete? [_] true)
                          core/IOrphanVectorPurge
                          (purge-vector-ids! [_ ids] (swap! deleted conj (vec ids)) (count ids))))
      (is (:isError (rec/handle-reconcile {:purge true :i_mean_it true})))
      (is (= [] @deleted)))))

(deftest a-port-that-reports-no-count-is-reported-as-unknown
  (doseq [bad [nil "2" -1 :done]]
    (let [store   (atom store-ids)
          vector  (atom vector-ids)
          deleted (atom [])
          out (rec/purge-response
               (purgeable store vector deleted {:purge-answer (constantly bad)}) nil true)
          p   (:purge out)]
      (is (= "applied" (:mode p)))
      (is (nil? (:purged p)) "unknown, not assumed to be the planned count")
      (is (= 2 (:planned-count p)))
      (is (= ["ghost-1" "ghost-2"] (:purged-ids p)))
      (is (str/includes? (:reason p) "unknown"))
      (is (m/validate ms/ReconcilePurgeResponse out)))))

(deftest i-mean-it-through-the-multi-tool-is-decoded-to-a-boolean
  (testing "the memory schema declares both flags boolean, so multi decodes them"
    (let [props (get-in cmem/tool-def [:inputSchema :properties])]
      (is (= "boolean" (name (get-in props ["i_mean_it" :type]))))
      (is (= "boolean" (name (get-in props ["purge" :type]))))
      (is (str/includes? (get-in props ["i_mean_it" :description]) "multi"))))
  (testing "the string \"true\" sent through multi reaches the handler as true and confirms"
    (let [{:keys [sources deleted]} (fixture)
          coerced (param-coerce/coerce-params
                   (:inputSchema cmem/tool-def)
                   {:command "reconcile" :purge "true" :i_mean_it "true"})]
      (is (true? (get-in coerced [:ok :i_mean_it])))
      (rec/set-sources! sources)
      (is (= "applied" (get-in (answer (rec/handle-reconcile (:ok coerced))) [:purge :mode])))
      (is (= [["ghost-1" "ghost-2"]] @deleted))))
  (testing "multi's boolean decoder, not this verb, decides which strings confirm"
    (let [decoded (fn [v] (param-coerce/coerce-params
                           (:inputSchema cmem/tool-def)
                           {:command "reconcile" :purge "true" :i_mean_it v}))]
      (doseq [v ["yes" "1" "TRUE"]]
        (is (true? (get-in (decoded v) [:ok :i_mean_it])) (str v " confirms through multi")))
      (doseq [v ["false" "no" "0"]]
        (is (false? (get-in (decoded v) [:ok :i_mean_it])) (str v " is a dry run")))
      (is (not (contains? (decoded "maybe") :ok)) "undecodable: refused before the handler")))
  (testing "the same string straight to the handler is not a confirmation"
    (let [{:keys [sources deleted]} (fixture)]
      (rec/set-sources! sources)
      (is (= "dry-run" (get-in (answer (rec/handle-reconcile {:purge "true" :i_mean_it "true"}))
                               [:purge :mode])))
      (is (= [] @deleted)))))

(deftest a-failing-delete-is-a-failed-reconcile
  (rec/set-sources! (reify
                      core/IReconcileSources
                      (store-ids [_] ["a" "b"])
                      (vector-ids [_] ["a" "b" "ghost"])
                      core/IListingCompleteness
                      (listings-complete? [_] true)
                      core/IOrphanVectorPurge
                      (purge-vector-ids! [_ _] (throw (ex-info "index down" {})))))
  (is (:isError (rec/handle-reconcile {:purge true :i_mean_it true}))))

(deftrifecta-from-schema purge-refusal-fails-closed
  hive-mcp.memory.reconcile/purge-refusal
  {:in  [:cat [:vector {:max 4} [:maybe [:enum "a" "b" ""]]]
         [:vector {:max 6} [:maybe [:enum "a" "b" "x" "y" "z" " "]]]
         [:vector {:max 4} [:enum "x" "y" "z"]]]
   :out [:maybe :string]
   :rel (fn [[store vectors plan] out]
          (let [ids (fn [c] (set (remove #(or (nil? %) (str/blank? %)) c)))
                p   (count (ids plan))
                s   (count (ids store))
                v   (count (ids vectors))]
            (= (some? out)
               (and (pos? p) (or (zero? s) (> (* 2 p) v))))))
   :num-tests 200})

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
