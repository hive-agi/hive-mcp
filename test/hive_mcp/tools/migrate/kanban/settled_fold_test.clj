(ns hive-mcp.tools.migrate.kanban.settled-fold-test
  "A step folds into saved state only the part of its batch the cursor has
   passed. A batch held by a :queued id or by an unconfirmed (not durable)
   write is classified again on the next step, so counting it now would
   count its ids twice: :stats :scanned would exceed the ids seen, and
   :failed / :unreadable would list an id twice, or keep a failed id that a
   retry wrote.

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
;; Pure fold
;; =============================================================================

(deftest settled-outcomes-is-the-part-before-the-cursor
  (let [os [{:id "a"} {:id "b"} {:id "c"}]]
    (is (= os (pure/settled-outcomes 4 7 os)) "nothing held")
    (is (= [{:id "a"}] (pure/settled-outcomes 4 5 os)) "held at b")
    (is (= [] (pure/settled-outcomes 4 4 os)) "held at the batch start")))

(deftest fold-batch-counts-only-settled-ids
  (let [s0  state/initial-state
        res {:start 0 :cursor 2 :done? false
             :outcomes [{:id "a" :outcome :already-full}
                        {:id "b" :outcome :failed :error "e1"}
                        {:id "c" :outcome :queued}
                        {:id "d" :outcome :unreadable :reason :hidden}]}
        s1  (pure/fold-batch s0 (assoc res :tally (pure/tally-outcomes (:outcomes res))) "t1")]
    (is (= 2 (:cursor s1)))
    (is (= :running (:phase s1)))
    (is (= 2 (-> s1 :stats :scanned)))
    (is (= 1 (-> s1 :stats :already-full)))
    (is (= 1 (-> s1 :stats :failed)))
    (is (zero? (-> s1 :stats :unreadable)) "d is after the held cursor")
    (is (= [{:id "b" :error "e1"}] (:failed s1)))
    (is (= [] (:unreadable s1)))
    (is (not-any? #(contains? (:stats s1) %) pure/transient-outcome-types))
    (is (= {:at "t1" :start 0 :cursor 2 :settled 2 :held 2}
           (dissoc (:last-step s1) :tally)))
    (is (= 1 (-> s1 :last-step :tally :queued)))))

(deftest fold-batch-keys-failed-and-unreadable-rows-by-id
  (let [s0 (assoc state/initial-state
                  :failed     [{:id "a" :error "old"} {:id "b" :error "b1"}]
                  :unreadable [{:id "u" :reason :hidden}])
        s1 (pure/fold-batch s0 {:start 0 :cursor 3 :done? true
                                :outcomes [{:id "a" :outcome :written}
                                           {:id "b" :outcome :failed :error "b2"}
                                           {:id "u" :outcome :unreadable :reason :unopenable}]}
                            "t")]
    (is (= [{:id "b" :error "b2"}] (:failed s1))
        "a wrote and left :failed; b is listed once, with its last error")
    (is (= [{:id "u" :reason :unopenable}] (:unreadable s1)))
    (is (= :done (:phase s1)))))

(deftest fold-batch-drops-transient-counts-from-an-older-state
  (let [s0 (update state/initial-state :stats assoc :queued 3 :unconfirmed 2)
        s1 (pure/fold-batch s0 {:start 0 :cursor 0 :outcomes [] :done? false} "t")]
    (is (not (contains? (:stats s1) :queued)))
    (is (not (contains? (:stats s1) :unconfirmed)))))

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

(defrecord ScriptWriter [target script drop-after-write durable]
  ;; SCRIPT (an atom id => [answer ...]) answers each add-entry! of an id
  ;; with the next of :fail / :queue / :ok (default :ok); :ok upserts into
  ;; TARGET. With DROP-AFTER-WRITE set, the target stops being durable once
  ;; this call has written.
  ports/IEntryWriter
  (write-entries [_ es]
    (let [out (mapv (fn [{:keys [id] :as e}]
                      (let [a (or (first (get @script id)) :ok)]
                        (swap! script update id #(vec (rest %)))
                        (case a
                          :fail  {:id id :ok? false :error "boom"}
                          :queue {:id id :ok? false :queued? true}
                          :ok    (do (swap! target assoc id e) {:id id :ok? true}))))
                    es)]
      (when @drop-after-write (reset! durable false))
      (r/ok out))))

(defrecord FlagDurability [durable]
  ports/ITargetDurability
  (check-durable [_]
    (if @durable
      (r/ok {:connected? true})
      (r/err :target/not-durable {:message "client dropped"}))))

(defn- card [id]
  {:id id :type :note :tags ["kanban" "todo"]
   :content {:task-type "kanban" :title (str "t-" id) :status "todo"}})

(defn- fixture
  "Deps over IDS: a source card per id; the target holds a full card for
   \"f\" and hides a row for \"h\" (the probe lists it, the read does not)."
  [ids script drop?]
  (mig-events/init!)
  (let [target  (atom {"f" (card "f")})
        durable (atom true)
        drop    (atom drop?)
        st      (atom state/initial-state)]
    {:target target :durable durable :drop drop :st st
     :deps {:source-lister     (->FixedLister ids)
            :source-reader     (->MapReader (atom (into {} (map (juxt identity card)) ids)))
            :target-reader     (->MapReader target)
            :target-probe      (->SetProbe #{"h"})
            :target-durability (->FlagDurability durable)
            :writer            (->ScriptWriter target (atom script) drop durable)
            :state             (->MemState st)}}))

;; =============================================================================
;; A not-durable batch, then a resume
;; =============================================================================

(deftest a-not-durable-batch-then-a-resume-counts-each-id-once
  (let [ids ["f" "h" "x" "w"]
        {:keys [deps st target durable drop]} (fixture ids {"x" [:fail :ok]} true)
        _   (uc/init-ids! deps)
        r1  (uc/step! deps {:batch-size 4})]
    (testing "the batch is held at its start and nothing of it is counted"
      (is (= :target/not-durable (:error r1)))
      (is (= 0 (:cursor @st)))
      (is (= {:already-full 1 :unreadable 1 :failed 1 :unconfirmed 1}
             (select-keys (-> @st :last-step :tally)
                          [:already-full :unreadable :failed :unconfirmed])))
      (is (zero? (-> @st :stats :scanned)))
      (is (empty? (:failed @st)))
      (is (empty? (:unreadable @st))))
    ;; the backend comes back; its RAM fallback (w) is gone
    (swap! target dissoc "w")
    (reset! drop false)
    (reset! durable true)
    (let [r2 (uc/step! deps {:batch-size 4})]
      (is (r/ok? r2) (pr-str r2))
      (is (= [:already-full :unreadable :written :written]
             (mapv :outcome (-> r2 :ok :batch-result :outcomes))))
      (is (= :done (:phase @st)))
      (is (= (count (distinct ids)) (-> @st :stats :scanned)))
      (is (= 1 (-> @st :stats :already-full)))
      (is (= 1 (-> @st :stats :unreadable)))
      (is (= 2 (-> @st :stats :written)))
      (is (zero? (-> @st :stats :failed)))
      (is (empty? (:failed @st)) "x failed in the held batch, then wrote")
      (is (= [{:id "h" :reason :hidden}] (:unreadable @st)) "h listed once"))))

;; =============================================================================
;; A :queued hold mid-batch, then a resume
;; =============================================================================

(deftest a-queued-hold-mid-batch-then-a-resume-counts-each-id-once
  (let [ids ["f" "h" "q" "x" "w"]
        {:keys [deps st]} (fixture ids {"q" [:queue :ok] "x" [:fail :ok]} false)
        _   (uc/init-ids! deps)
        r1  (uc/step! deps {:batch-size 5})]
    (testing "held at q: only f and h are settled"
      (is (r/ok? r1) (pr-str r1))
      (is (= [:already-full :unreadable :queued :failed :written]
             (mapv :outcome (-> r1 :ok :batch-result :outcomes))))
      (is (= 2 (:cursor @st)))
      (is (= 2 (-> @st :stats :scanned)))
      (is (zero? (-> @st :stats :written)) "w is after the held cursor")
      (is (empty? (:failed @st)) "x is after the held cursor")
      (is (= [{:id "h" :reason :hidden}] (:unreadable @st)))
      (is (= {:settled 2 :held 3} (select-keys (:last-step @st) [:settled :held]))))
    (let [r2 (uc/step! deps {:batch-size 5})]
      (is (r/ok? r2) (pr-str r2))
      (is (= [:written :written :already-full]
             (mapv :outcome (-> r2 :ok :batch-result :outcomes))))
      (is (= :done (:phase @st)))
      (is (= (count (distinct ids)) (-> @st :stats :scanned)))
      (is (= 2 (-> @st :stats :written)) "q and x")
      (is (= 2 (-> @st :stats :already-full)) "f, and w written by the held step")
      (is (= 1 (-> @st :stats :unreadable)))
      (is (zero? (-> @st :stats :failed)))
      (is (empty? (:failed @st)) "x failed in the held part, then wrote")
      (is (= [{:id "h" :reason :hidden}] (:unreadable @st)) "h listed once"))))
