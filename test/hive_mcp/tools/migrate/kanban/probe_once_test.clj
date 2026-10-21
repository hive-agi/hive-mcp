(ns hive-mcp.tools.migrate.kanban.probe-once-test
  "Two run-level guarantees of the kanban migrator.

   ONE LISTING PER RUN: the target's id listing (`scan-ids`, one unbounded
   scroll on hive-qdrant) happens at most once per run!, however many
   batches it takes; ids the run writes join the kept set. A run whose
   batches have no probe candidate lists nothing at all.

   DURABLE TARGET ONLY: run! and step! refuse, writing and reading nothing,
   unless the target's ITargetDurability port answers ok. The store adapter
   answers through the store port's `connected?`: hive-qdrant without a
   client answers add-entry! with the id out of an in-memory fallback, so a
   disconnected store must never be migrated into.

   Stub ports only: hive-spi's atom-backed store, wrapped by a counting
   record. No with-redefs."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-mcp.tools.migrate.kanban.api :as api]
            [hive-mcp.tools.migrate.kanban.events :as mig-events]
            [hive-mcp.tools.migrate.kanban.ports :as ports]
            [hive-mcp.tools.migrate.kanban.state :as state]
            [hive-mcp.tools.migrate.kanban.store :as mig-store]
            [hive-mcp.tools.migrate.kanban.usecases :as uc]
            [hive-spi.memory.ports :as sp]
            [hive-spi.memory.stub :as stub]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord CountingScanStore [inner scans]
  ;; A backend store that counts every scan-ids call.
  sp/IMemoryStore
  (connected? [_] (sp/connected? inner))
  (add-entry! [_ e] (sp/add-entry! inner e))
  (get-entry [_ id] (sp/get-entry inner id))
  sp/IMemoryStoreScan
  (scan-ids [_ opts] (swap! scans inc) (sp/scan-ids inner opts)))

(defrecord CountingProbe [ids calls]
  ports/IIdProbe
  (held-ids [_] (swap! calls inc) (r/ok (set ids))))

(defrecord MemState [atm]
  ports/IState
  (load-state [_]    (r/ok @atm))
  (save-state! [_ s] (reset! atm s) (r/ok s))
  (reset-state! [_]  (reset! atm state/initial-state) (r/ok state/initial-state)))

(defrecord MapReader [m]
  ports/IEntryReader
  (read-by-ids [_ ids] (r/ok (select-keys m ids))))

(defrecord FixedLister [ids]
  ports/IIdLister
  (list-ids [_] (r/ok {:ids ids :per-collection {"stub" (count ids)}})))

(defrecord FixedDurability [answer calls]
  ports/ITargetDurability
  (check-durable [_] (swap! calls inc) answer))

(defn- card [id]
  {:id id :type :note :tags ["kanban" "todo"]
   :content {:task-type "kanban" :title (str "t-" id) :status "todo"}})

(defn- ids [n] (mapv #(format "id-%02d" %) (range n)))

(defn- default-deps
  "api/make-deps over the target STORE, source holding a card per id."
  [store ids st]
  (mig-events/init!)
  (api/make-deps :target-store  store
                 :source-lister (->FixedLister ids)
                 :source-reader (->MapReader (into {} (map (juxt identity card)) ids))
                 :state         (->MemState st)))

(deftest a-multi-batch-run-lists-the-target-once
  (let [inner (stub/create-store)
        scans (atom 0)
        store (->CountingScanStore inner scans)
        ;; one already-migrated card, so the held set is not just what we wrote
        _     (sp/add-entry! inner (card "id-03"))
        all   (ids 9)
        st    (atom state/initial-state)
        deps  (default-deps store all st)
        _     (uc/init-ids! deps)
        res   (api/run! deps {:batch-size 2 :max-steps 20})]
    (is (r/ok? res) (pr-str res))
    (is (= :done (-> res :ok :stopped)))
    (is (= 5 (-> res :ok :steps-run)) "five batches")
    (is (= 1 @scans) "the target's ids were listed ONCE for the whole run")
    (is (= 8 (-> @st :stats :written)))
    (is (= 1 (-> @st :stats :already-full)))
    (is (= (set all) (set (sp/scan-ids inner {}))))))

(deftest written-ids-join-the-kept-set
  ;; A later batch carrying an id an earlier batch wrote (the lister may
  ;; repeat one) must see it held, though the read never shows a row and the
  ;; listing ran before the write.
  (mig-events/init!)
  (let [calls (atom 0)
        st    (atom state/initial-state)
        deps  {:source-lister     (->FixedLister ["a" "b" "a"])
               :source-reader     (->MapReader {"a" (card "a") "b" (card "b")})
               :target-reader     (->MapReader {})
               :target-probe      (->CountingProbe #{} calls)
               :target-durability (->FixedDurability (r/ok {}) (atom 0))
               :writer            (mig-store/make-entry-writer (stub/create-store))
               :state             (->MemState st)}
        _     (uc/init-ids! deps)
        res   (uc/run! deps {:batch-size 1 :max-steps 10})]
    (is (r/ok? res) (pr-str res))
    (is (= 1 @calls))
    (is (= 2 (-> @st :stats :written)))
    (is (= [{:id "a" :reason :hidden}] (:unreadable @st))
        "the repeated id is not written a second time")))

(deftest a-run-without-probe-candidates-lists-nothing
  (mig-events/init!)
  (let [calls (atom 0)
        all   ["a" "b" "c" "d"]
        deps  {:source-lister     (->FixedLister all)
               :source-reader     (->MapReader (into {} (map (juxt identity card)) all))
               ;; every target row is already a full card: nothing to write
               :target-reader     (->MapReader (into {} (map (juxt identity card)) all))
               :target-probe      (->CountingProbe all calls)
               :target-durability (->FixedDurability (r/ok {}) (atom 0))
               :writer            (mig-store/make-entry-writer (stub/create-store))
               :state             (->MemState (atom state/initial-state))}
        _     (uc/init-ids! deps)
        res   (uc/run! deps {:batch-size 2 :max-steps 10})]
    (is (r/ok? res))
    (is (= :done (-> res :ok :stopped)))
    (is (zero? @calls) "no candidate, no listing")))

(deftest a-probe-error-still-fails-the-run-closed
  (mig-events/init!)
  (let [writes (atom 0)
        deps   {:source-lister     (->FixedLister ["a" "b"])
                :source-reader     (->MapReader {"a" (card "a") "b" (card "b")})
                :target-reader     (->MapReader {})
                :target-probe      (reify ports/IIdProbe
                                     (held-ids [_] (r/err :store/id-probe-failed {})))
                :target-durability (->FixedDurability (r/ok {}) (atom 0))
                :writer            (reify ports/IEntryWriter
                                     (write-entries [_ es] (swap! writes + (count es)) (r/ok [])))
                :state             (->MemState (atom state/initial-state))}
        _      (uc/init-ids! deps)
        res    (uc/run! deps {:batch-size 1})]
    (is (r/err? res))
    (is (= :store/id-probe-failed (:error res)))
    (is (zero? @writes))))

;; =============================================================================
;; Durability gate
;; =============================================================================

(defn- gated-deps [durability writes st]
  (mig-events/init!)
  {:source-lister     (->FixedLister ["a"])
   :source-reader     (->MapReader {"a" (card "a")})
   :target-reader     (->MapReader {})
   :target-probe      (->CountingProbe #{} (atom 0))
   :target-durability durability
   :writer            (reify ports/IEntryWriter
                        (write-entries [_ es] (swap! writes + (count es))
                          (r/ok (mapv #(hash-map :id (:id %) :ok? true) es))))
   :state             (->MemState st)})

(deftest a-target-that-cannot-prove-durability-is-refused
  (let [writes (atom 0)
        st     (atom state/initial-state)
        deps   (gated-deps (->FixedDurability (r/err :target/not-durable {:message "nope"}) (atom 0))
                           writes st)
        _      (uc/init-ids! deps)]
    (testing "run! refuses before any step"
      (is (= :target/not-durable (:error (uc/run! deps {:batch-size 1}))))
      (is (zero? @writes))
      (is (zero? (:cursor @st))))
    (testing "step! refuses too"
      (is (= :target/not-durable (:error (uc/step! deps {:batch-size 1}))))
      (is (zero? @writes)))
    (testing "a dry run writes nothing, so it is not gated"
      (is (r/ok? (uc/step! deps {:batch-size 1 :dry-run? true}))))
    (testing "no durability port at all is a refusal"
      (let [deps' (dissoc deps :target-durability)]
        (is (= :target-durability/missing (:error (uc/run! deps' {:batch-size 1}))))
        (is (zero? @writes))))))

(deftest a-durable-target-runs
  (let [writes (atom 0)
        st     (atom state/initial-state)
        deps   (gated-deps (->FixedDurability (r/ok {}) (atom 0)) writes st)
        _      (uc/init-ids! deps)]
    (is (= :done (-> (uc/run! deps {:batch-size 1}) :ok :stopped)))
    (is (= 1 @writes))))

(deftest the-store-gate-reads-connected-through-the-port
  (let [inner (stub/create-store)
        gate  (mig-store/make-durability (->CountingScanStore inner (atom 0)))]
    (is (r/ok? (ports/check-durable gate)) "connected: ok")
    (sp/disconnect! inner)
    (let [res (ports/check-durable gate)]
      (is (= :target/not-durable (:error res)))
      (is (re-find #"not connected" (str (:message res)))))
    (is (= :store/missing (:error (ports/check-durable (mig-store/make-durability nil)))))
    (testing "a disconnected default-wired target migrates nothing"
      (let [st   (atom state/initial-state)
            deps (default-deps (->CountingScanStore inner (atom 0)) ["x"] st)
            _    (uc/init-ids! deps)]
        (is (= :target/not-durable (:error (api/run! deps {:batch-size 1}))))
        (is (nil? (sp/get-entry inner "x")))))))
