(ns hive-mcp.tools.migrate.kanban.unreadable-test
  "The migrator writes only where the target truly has no row for the id:
   add-entry! upserts by id, so writing over a row it cannot read would
   replace an already-migrated, since-edited card with the stale source.

   - HIDDEN: a visibility-like layer hides a stored entry, so the read
     answers nothing; the existence probe (base store id listing only)
     finds it, and it is reported :unreadable / :hidden, not written.
   - UNOPENABLE: a layer that cannot open the envelope answers the row with
     its string content; it is :unreadable / :unopenable, not written.
   - ABSENT: an id the target does not hold is still written.

   Stub ports only: hive-spi's atom-backed store under record layers that
   declare themselves hive-spi decorators. No with-redefs."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-mcp.tools.migrate.kanban.api :as api]
            [hive-mcp.tools.migrate.kanban.events :as mig-events]
            [hive-mcp.tools.migrate.kanban.ports :as ports]
            [hive-mcp.tools.migrate.kanban.state :as state]
            [hive-mcp.tools.migrate.kanban.store :as mig-store]
            [hive-mcp.tools.migrate.kanban.usecases :as uc]
            [hive-spi.memory.decorate :as decorate]
            [hive-spi.memory.ports :as sp]
            [hive-spi.memory.stub :as stub]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord HidingLayer [inner hidden]
  ;; Like the seal's VisibilityLayer: every read exit drops an id in HIDDEN;
  ;; add-entry! checks only the incoming entry and writes through.
  decorate/IStoreDecorator
  (inner-store [_] inner)
  sp/IMemoryStore
  (connected? [_] (sp/connected? inner))
  (add-entry! [_ e] (sp/add-entry! inner e))
  (get-entry [_ id] (when-not (contains? hidden id) (sp/get-entry inner id))))

(defrecord UnopenableLayer [inner]
  ;; Like the cipher layer when the key cannot be resolved: the envelope
  ;; comes back unchanged, :content stays a string.
  decorate/IStoreDecorator
  (inner-store [_] inner)
  sp/IMemoryStore
  (connected? [_] (sp/connected? inner))
  (add-entry! [_ e] (sp/add-entry! inner (assoc e :content (str "#env:" (pr-str (:content e))))))
  (get-entry [_ id] (sp/get-entry inner id)))

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

(defn- card [id title]
  {:id id :type :note :tags ["kanban" "todo"]
   :content {:task-type "kanban" :title title :status "todo"}})

(defn- step-over
  "One default-wired step (api/make-deps) over the target LAYER for IDS,
   the source holding a STALE copy of each."
  [layer ids]
  (mig-events/init!)
  (let [st   (atom state/initial-state)
        deps (api/make-deps :target-store  layer
                            :source-lister (->FixedLister ids)
                            :source-reader (->MapReader (into {} (map (juxt identity #(card % "stale"))) ids))
                            :state         (->MemState st))
        _    (uc/init-ids! deps)]
    [(api/step! deps {:batch-size 10}) st]))

(deftest a-hidden-row-is-not-overwritten
  (let [inner (stub/create-store)
        _     (sp/add-entry! inner (card "h1" "edited since migration"))
        layer (->HidingLayer inner #{"h1"})
        [res st] (step-over layer ["h1"])]
    (is (r/ok? res) (pr-str res))
    (is (nil? (sp/get-entry layer "h1")) "the read cannot see it")
    (is (= [{:id "h1" :outcome :unreadable :reason :hidden}]
           (mapv #(select-keys % [:id :outcome :reason]) (-> res :ok :batch-result :outcomes))))
    (is (= "edited since migration" (get-in (sp/get-entry inner "h1") [:content :title]))
        "the stored card is untouched")
    (is (= 1 (-> @st :stats :unreadable)))
    (is (zero? (-> @st :stats :written)))
    (is (= [{:id "h1" :reason :hidden}] (:unreadable @st)))))

(deftest an-unopenable-row-is-not-overwritten
  (let [inner  (stub/create-store)
        layer  (->UnopenableLayer inner)
        _      (sp/add-entry! layer (card "u1" "edited since migration"))
        before (:content (sp/get-entry inner "u1"))
        [res st] (step-over layer ["u1"])]
    (is (r/ok? res) (pr-str res))
    (is (string? (:content (sp/get-entry layer "u1"))) "content stays a string")
    (is (= [{:id "u1" :outcome :unreadable :reason :unopenable}]
           (mapv #(select-keys % [:id :outcome :reason]) (-> res :ok :batch-result :outcomes))))
    (is (= before (:content (sp/get-entry inner "u1"))) "the stored envelope is untouched")
    (is (zero? (-> @st :stats :written)))))

(deftest an-absent-id-is-still-written
  (testing "behind a hiding layer that hides nothing for this id"
    (let [inner (stub/create-store)
          layer (->HidingLayer inner #{"other"})
          [res st] (step-over layer ["n1"])]
      (is (r/ok? res) (pr-str res))
      (is (= [:written] (mapv :outcome (-> res :ok :batch-result :outcomes))))
      (is (= "stale" (get-in (sp/get-entry inner "n1") [:content :title])))
      (is (= 1 (-> @st :stats :written))))))

(deftest a-probe-that-cannot-answer-writes-nothing
  (mig-events/init!)
  (let [inner  (stub/create-store)
        writes (atom 0)
        writer (reify ports/IEntryWriter
                 (write-entries [_ es] (swap! writes + (count es)) (r/ok [])))
        probe  (reify ports/IIdProbe
                 (held-ids [_] (r/err :store/no-id-probe {})))
        deps   {:source-lister     (->FixedLister ["a"])
                :source-reader     (->MapReader {"a" (card "a" "stale")})
                :target-reader     (mig-store/make-entry-reader inner)
                :target-probe      probe
                :target-durability (mig-store/make-durability inner)
                :writer            writer
                :state             (->MemState (atom state/initial-state))}
        _      (uc/init-ids! deps)]
    (is (r/err? (uc/step! deps {:batch-size 10})))
    (is (r/err? (uc/run! deps {:batch-size 10})))
    (is (zero? @writes))))

(deftest the-store-probe-lists-ids-through-every-decorator
  (let [inner (stub/create-store)
        _     (sp/add-entry! inner (card "p1" "x"))
        layer (->HidingLayer (->UnopenableLayer inner) #{"p1"})]
    (is (= (r/ok #{"p1"}) (ports/held-ids (mig-store/make-id-probe layer))))
    (is (r/err? (ports/held-ids (mig-store/make-id-probe nil))))))
