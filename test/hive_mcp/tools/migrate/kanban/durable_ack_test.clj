(ns hive-mcp.tools.migrate.kanban.durable-ack-test
  "The target's durability is checked again after a batch writes.

   hive-qdrant's reconnect can drop its client mid-batch; add-entry! then
   lands in an in-memory fallback and still answers the id. A batch whose
   target is not durable once its writes are done must not move the cursor
   past them, nor leave their ids in the run's held-id cache (a held id is
   never written again): its :written outcomes become :unconfirmed, the
   cursor stays at the batch start, and the step answers :target/not-durable.
   A re-run then writes the batch again (add-entry! upserts by id).

   Stub ports only, no with-redefs."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-mcp.tools.migrate.kanban.events :as mig-events]
            [hive-mcp.tools.migrate.kanban.ports :as ports]
            [hive-mcp.tools.migrate.kanban.pure :as pure]
            [hive-mcp.tools.migrate.kanban.state :as state]
            [hive-mcp.tools.migrate.kanban.usecases :as uc]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Stub ports
;; =============================================================================

(defrecord MemState [atm]
  ports/IState
  (load-state [_]    (r/ok @atm))
  (save-state! [_ s] (reset! atm s) (r/ok s))
  (reset-state! [_]  (reset! atm state/initial-state) (r/ok state/initial-state)))

(defrecord MapReader [m]
  ports/IEntryReader
  (read-by-ids [_ ids] (r/ok (select-keys @m ids))))

(defrecord FixedLister [ids]
  ports/IIdLister
  (list-ids [_] (r/ok {:ids ids :per-collection {"stub" (count ids)}})))

(defrecord SetProbe [ids]
  ports/IIdProbe
  (held-ids [_] (r/ok (set ids))))

(defrecord UpsertWriter [target writes]
  ;; Upserts each entry into TARGET (an atom id => entry) by id, counting
  ;; every add-entry!; answers ok for each, as a fallback-backed store does.
  ports/IEntryWriter
  (write-entries [_ es]
    (r/ok (mapv (fn [e]
                  (swap! writes inc)
                  (swap! target assoc (:id e) e)
                  {:id (:id e) :ok? true})
                es))))

(defrecord FlippingDurability [writes n]
  ;; Durable until more than N writes have been made, then not.
  ports/ITargetDurability
  (check-durable [_]
    (if (<= @writes n)
      (r/ok {:connected? true})
      (r/err :target/not-durable {:message "client dropped"}))))

(defrecord ThrowingAfter [writes n]
  ;; Durable until more than N writes, then the check itself errs differently.
  ports/ITargetDurability
  (check-durable [_]
    (if (<= @writes n)
      (r/ok {:connected? true})
      (r/err :store/check-failed {:message "boom"}))))

(defn- card [id]
  {:id id :type :note :tags ["kanban" "todo"]
   :content {:task-type "kanban" :title (str "t-" id) :status "todo"}})

(defn- fixture
  "Deps over ALL ids, a source card per id, an empty target. DURABILITY is
   built from the writes counter."
  [all mk-durability]
  (mig-events/init!)
  (let [writes (atom 0)
        target (atom {})
        st     (atom state/initial-state)]
    {:writes writes
     :target target
     :st     st
     :deps   {:source-lister     (->FixedLister all)
              :source-reader     (->MapReader (atom (into {} (map (juxt identity card)) all)))
              :target-reader     (->MapReader target)
              :target-probe      (->SetProbe #{})
              :target-durability (mk-durability writes)
              :writer            (->UpsertWriter target writes)
              :state             (->MemState st)}}))

;; =============================================================================
;; Pure fold
;; =============================================================================

(deftest unconfirm-written-is-identity-when-durable
  (let [os [{:id "a" :outcome :written} {:id "b" :outcome :already-full}]]
    (is (= os (pure/unconfirm-written os true)))))

(deftest unconfirm-written-turns-every-written-unconfirmed
  (let [os  [{:id "a" :outcome :written} {:id "b" :outcome :already-full}
             {:id "c" :outcome :written} {:id "d" :outcome :failed :error "x"}]
        out (pure/unconfirm-written os false)]
    (is (= [:unconfirmed :already-full :unconfirmed :failed] (mapv :outcome out)))
    (is (= ["a" "c"] (pure/unconfirmed-ids out)))
    (is (contains? pure/outcome-types :unconfirmed))))

(deftest hold-cursor-keeps-an-unconfirmed-batch-at-its-start
  (is (= [4 false] (pure/hold-cursor 4 [{:outcome :already-full} {:outcome :unconfirmed}] 6 true)))
  (testing "an unconfirmed batch wins over a later queued one"
    (is (= [4 false] (pure/hold-cursor 4 [{:outcome :queued} {:outcome :unconfirmed}] 6 false))))
  (testing "nothing held: the proposal stands"
    (is (= [6 true] (pure/hold-cursor 4 [{:outcome :written}] 6 true)))))

(deftest needs-durable-ack-only-after-a-write
  (is (pure/needs-durable-ack? [{:outcome :written}]))
  (is (not (pure/needs-durable-ack? [{:outcome :already-full} {:outcome :would-write}]))))

;; =============================================================================
;; Use case: a target that drops mid-run
;; =============================================================================

(deftest a-target-lost-mid-batch-leaves-the-cursor-at-the-batch-start
  ;; batch 1 (a b) writes 2: durable. batch 2 (c d) writes 2 more: 4 > 3, not
  ;; durable once it is over.
  (let [{:keys [deps st writes]} (fixture ["a" "b" "c" "d" "e"]
                                          #(->FlippingDurability % 3))
        held (atom nil)
        deps (assoc deps :held-ids held)
        _    (uc/init-ids! deps)]
    (testing "batch 1 is confirmed and advances"
      (let [res (uc/step! deps {:batch-size 2})]
        (is (r/ok? res) (pr-str res))
        (is (= 2 (:cursor @st)))
        (is (= 2 (-> @st :stats :written)))))
    (testing "batch 2 is unconfirmed: cursor at its start, :target/not-durable"
      ;; prime the held cache as run! would have it after a listing
      (swap! held #(or % #{"a" "b"}))
      (let [res (uc/step! deps {:batch-size 2})]
        (is (r/err? res))
        (is (= :target/not-durable (:error res)))
        (is (= ["c" "d"] (:unconfirmed res)))
        (is (= 2 (:cursor @st)) "the cursor did not move past c and d")
        (is (not= :done (:phase @st)))
        (is (= 2 (-> @st :stats :written)) "c and d are not counted written")
        (is (= 2 (-> @st :stats :scanned)) "c and d are not counted scanned")
        (is (= 2 (-> @st :last-step :tally :unconfirmed))
            "the step reports its unconfirmed writes")
        (is (not (contains? (:stats @st) :unconfirmed))
            ":unconfirmed is not a cumulative stat")
        (is (= 4 @writes))))
    (testing "the held-id cache does not keep the unconfirmed ids"
      (is (= #{"a" "b"} @held)))))

(deftest run-stops-on-a-lost-target-and-a-rerun-writes-the-batch-again
  (let [n-before (atom 3)
        {:keys [deps st writes target]}
        (fixture ["a" "b" "c" "d" "e"]
                 (fn [w] (reify ports/ITargetDurability
                           (check-durable [_]
                             (if (<= @w @n-before)
                               (r/ok {:connected? true})
                               (r/err :target/not-durable {:message "client dropped"}))))))
        _    (uc/init-ids! deps)
        res  (uc/run! deps {:batch-size 2 :max-steps 10})]
    (is (= :target/not-durable (:error res)))
    (is (= 2 (:cursor @st)))
    ;; the backend comes back; c and d are still in the target map here (the
    ;; fallback), so simulate the restart losing them, as a RAM fallback does
    (swap! target dissoc "c" "d")
    (reset! n-before Long/MAX_VALUE)
    (let [res2 (uc/run! deps {:batch-size 2 :max-steps 10})]
      (is (r/ok? res2) (pr-str res2))
      (is (= :done (-> res2 :ok :stopped)))
      (is (= 5 (:cursor @st)))
      (is (= #{"a" "b" "c" "d" "e"} (set (keys @target))) "c and d rewritten")
      (is (= 7 @writes) "a b c d, then c d e again"))))

(deftest a-rerun-against-rows-that-did-land-rewrites-nothing
  ;; If the unconfirmed writes in fact reached the backend, the re-run reads
  ;; them back as :already-full: idempotent, nothing written twice. Each id
  ;; is counted once: a and b written by the confirmed batch, c and d
  ;; already full on the re-run; the unconfirmed attempt counts nothing.
  (let [n (atom 3)
        {:keys [deps st writes]}
        (fixture ["a" "b" "c" "d"]
                 (fn [w] (reify ports/ITargetDurability
                           (check-durable [_]
                             (if (<= @w @n)
                               (r/ok {})
                               (r/err :target/not-durable {}))))))
        _ (uc/init-ids! deps)
        _ (uc/run! deps {:batch-size 2})]
    (is (= 2 (-> @st :stats :scanned)) "the held batch is not counted")
    (reset! n Long/MAX_VALUE)
    (is (= :done (-> (uc/run! deps {:batch-size 2}) :ok :stopped)))
    (is (= 4 @writes))
    (is (= 4 (-> @st :stats :scanned)))
    (is (= 2 (-> @st :stats :written)))
    (is (= 2 (-> @st :stats :already-full)))
    (is (not (contains? (:stats @st) :unconfirmed)))))

(deftest a-durability-check-that-errs-after-the-batch-also-holds
  (let [{:keys [deps st]} (fixture ["a" "b"] #(->ThrowingAfter % 0))
        _   (uc/init-ids! deps)
        res (uc/step! deps {:batch-size 2})]
    (is (= :target/not-durable (:error res)))
    (is (= :store/check-failed (:cause res)))
    (is (zero? (:cursor @st)))))

(deftest a-target-that-stays-durable-behaves-as-before
  (let [{:keys [deps st writes]} (fixture ["a" "b" "c" "d" "e"]
                                          #(->FlippingDurability % Long/MAX_VALUE))
        _   (uc/init-ids! deps)
        res (uc/run! deps {:batch-size 2 :max-steps 10})]
    (is (r/ok? res) (pr-str res))
    (is (= :done (-> res :ok :stopped)))
    (is (= 3 (-> res :ok :steps-run)))
    (is (= 5 (:cursor @st)))
    (is (= 5 (-> @st :stats :written)))
    (is (= 5 (-> @st :stats :scanned)))
    (is (zero? (-> @st :last-step :tally :unconfirmed)))
    (is (= 5 @writes))))
