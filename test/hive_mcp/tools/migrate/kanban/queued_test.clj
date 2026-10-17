(ns hive-mcp.tools.migrate.kanban.queued-test
  "A queued add-entry! acknowledgement ({:success? true :queued? true}, what
   hive-qdrant answers while its circuit breaker is open) is NOT a durable
   write: its queue lives in RAM. The migrator must not mark it :written nor
   move the cursor past it; the next step writes it again.

   Stub ports only: hive-spi's atom-backed store behind reify decorators."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-mcp.tools.migrate.kanban.events :as mig-events]
            [hive-mcp.tools.migrate.kanban.ports :as ports]
            [hive-mcp.tools.migrate.kanban.pure :as pure]
            [hive-mcp.tools.migrate.kanban.state :as state]
            [hive-mcp.tools.migrate.kanban.store :as mig-store]
            [hive-mcp.tools.migrate.kanban.usecases :as uc]
            [hive-spi.memory.ports :as sp]
            [hive-spi.memory.stub :as stub]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

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

(defn- kanban-entry [id]
  {:id id :type :note :tags ["kanban" "todo"]
   :content {:task-type "kanban" :title (str "t-" id) :status "todo"}})

(defn- queue-then-land-store
  "Backed by INNER, but answers the open-circuit queue ack for the first
   QUEUE-N add-entry! calls; later calls land."
  [inner queue-n calls]
  (reify
    sp/IMemoryStore
    (add-entry! [_ e]
      (if (<= (:n (swap! calls update :n inc)) queue-n)
        {:success? true :queued? true :depth 1}
        (sp/add-entry! inner e)))
    (get-entry [_ id] (sp/get-entry inner id))))

(deftest write-result-queued-is-not-ok
  (let [row (pure/write-result "a" {:success? true :queued? true :depth 1})]
    (is (false? (:ok? row)))
    (is (true? (:queued? row))))
  (let [row (pure/write-result "a" "a")]
    (is (true? (:ok? row)))
    (is (not (:queued? row))))
  (testing "a queued ack that also failed is a plain failure"
    (is (not (:queued? (pure/write-result "a" {:queued? true :success? false}))))))

(deftest hold-cursor-stops-at-the-first-queued-id
  (is (= [5 false] (pure/hold-cursor 4 [{:outcome :written} {:outcome :queued}
                                        {:outcome :written}] 7 true)))
  (is (= [7 true]  (pure/hold-cursor 4 [{:outcome :written} {:outcome :failed}
                                        {:outcome :already-full}] 7 true))))

(deftest a-queued-write-keeps-the-cursor-and-is-retried
  (mig-events/init!)
  (let [inner (stub/create-store)
        calls (atom {:n 0})
        st    (atom state/initial-state)
        store (queue-then-land-store inner 1 calls)
        deps  {:source-lister (->FixedLister ["a" "b" "c"])
               :source-reader (->MapReader (into {} (map (juxt identity kanban-entry)) ["a" "b" "c"]))
               :target-reader (mig-store/make-entry-reader store)
               :target-probe  (mig-store/make-id-probe inner)
               :target-durability (mig-store/make-durability inner)
               :writer        (mig-store/make-entry-writer store)
               :state         (->MemState st)}
        _     (uc/init-ids! deps)
        r1    (uc/step! deps {:batch-size 3})]
    (testing "first step: a only queued, b and c written, cursor held at a"
      (is (r/ok? r1))
      (is (= [:queued :written :written]
             (mapv :outcome (-> r1 :ok :batch-result :outcomes))))
      (is (= 0 (:cursor @st)))
      (is (= :running (:phase @st)))
      (is (= 2 (-> @st :stats :written)))
      (is (= 1 (-> @st :stats :queued)))
      (is (empty? (:failed @st)) "a queued write is not a failure")
      (is (nil? (sp/get-entry inner "a")) "nothing durable for a yet"))
    (testing "next step writes a again; b and c are already full"
      (let [r2 (uc/step! deps {:batch-size 3})]
        (is (= [:written :already-full :already-full]
               (mapv :outcome (-> r2 :ok :batch-result :outcomes))))
        (is (= 3 (:cursor @st)))
        (is (= :done (:phase @st)))
        (is (some? (sp/get-entry inner "a")))))))

(deftest a-durable-write-still-advances-the-cursor
  (mig-events/init!)
  (let [inner (stub/create-store)
        st    (atom state/initial-state)
        deps  {:source-lister (->FixedLister ["a" "b"])
               :source-reader (->MapReader {"a" (kanban-entry "a") "b" (kanban-entry "b")})
               :target-reader (mig-store/make-entry-reader inner)
               :target-probe  (mig-store/make-id-probe inner)
               :target-durability (mig-store/make-durability inner)
               :writer        (mig-store/make-entry-writer inner)
               :state         (->MemState st)}
        _     (uc/init-ids! deps)
        r1    (uc/step! deps {:batch-size 2})]
    (is (= [:written :written] (mapv :outcome (-> r1 :ok :batch-result :outcomes))))
    (is (= 2 (:cursor @st)))
    (is (= :done (:phase @st)))))

(deftest run-stops-when-the-target-only-queues
  (mig-events/init!)
  (let [calls (atom {:n 0})
        inner (stub/create-store)
        deps  {:source-lister (->FixedLister ["a" "b"])
               :source-reader (->MapReader {"a" (kanban-entry "a") "b" (kanban-entry "b")})
               :target-reader (->MapReader {})
               :target-probe  (mig-store/make-id-probe inner)
               :target-durability (mig-store/make-durability inner)
               :writer        (mig-store/make-entry-writer
                               (queue-then-land-store inner 1000 calls))
               :state         (->MemState (atom state/initial-state))}
        _     (uc/init-ids! deps)
        res   (uc/run! deps {:batch-size 1 :max-steps 10})]
    (is (= :queued (-> res :ok :stopped)))
    (is (= 1 (-> res :ok :steps-run)))
    (is (= 0 (-> res :ok :state :cursor)))
    (is (= 1 (:n @calls)) "no busy loop re-queuing the same id")))
