(ns hive-mcp.tools.memory.crud.write-timeout-test
  "Stage attribution of the memory-add timeout (kanban 20260529003400-5c19ae4f,
   P0 (a)): a write that outruns the outer budget names the stage it was in,
   how long the write and that stage had run, and the cause, instead of the
   old bare `memory add timed out after 30000ms`.

   Facets for the pure `timeout-diagnostic`:
   - golden:   diagnostics for a queued write and for each store stage
   - property: elapsed >= stage-elapsed, both non-negative, cause follows stage
   - mutation: always blaming the queue, and swapping the two clocks, are caught

   The handle-add tests drive the real path through a stalled store stub."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.crystal.recall]
            [hive-mcp.knowledge-graph.connection]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.tools.memory.crud.write :as wr]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private ms->ns #(* % 1000000))

(defn- raw-diagnostic
  "`timeout-diagnostic` over a {:stage :started-ms :since-ms :now-ms
   :timeout-ms} description with the clocks given in milliseconds. Mutants
   build on this, never on the subject."
  [{:keys [stage started-ms since-ms now-ms timeout-ms]}]
  (wr/timeout-diagnostic {:stage stage
                          :started-ns (ms->ns started-ms)
                          :stage-since-ns (ms->ns since-ms)}
                         timeout-ms
                         (ms->ns now-ms)))

(defn diagnose
  "Subject: the timeout diagnostic for a millisecond-clock description."
  [c]
  (raw-diagnostic c))

(def ^:private cases
  {:queued-never-started {:stage :queued :started-ms 0 :since-ms 0
                          :now-ms 30000 :timeout-ms 30000}
   :stuck-in-dedup       {:stage :dedup-lookup :started-ms 0 :since-ms 100
                          :now-ms 30000 :timeout-ms 30000}
   :stuck-in-index       {:stage :index :started-ms 0 :since-ms 9000
                          :now-ms 30000 :timeout-ms 30000}
   :stuck-in-fetch       {:stage :fetch :started-ms 0 :since-ms 29000
                          :now-ms 30000 :timeout-ms 30000}})

(def ^:private gen-case
  (gen/let [stage (gen/elements [:queued :dedup-lookup :dedup-update :index
                                 :kg-edges :link-outgoing :fetch])
            started gen/nat
            a gen/nat
            b gen/nat]
    {:stage stage :started-ms started :since-ms (+ started a)
     :now-ms (+ started a b) :timeout-ms 30000}))

(defn- well-formed?
  "Property predicate (receives OUTPUT)."
  [{:keys [stage elapsed-ms stage-elapsed-ms cause]}]
  (and (keyword? stage)
       (<= 0 stage-elapsed-ms elapsed-ms)
       (= cause (if (= :queued stage) :pool-queue-wait :stage-stalled))))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(deftrifecta timeout-diagnostic-contract
  hive-mcp.tools.memory.crud.write-timeout-test/diagnose
  {:golden-path   "test/golden/tools/memory/write-timeout-diagnostic.edn"
   :cases         cases
   :gen           gen-case
   :pred          well-formed?
   :property-type :pred
   :num-tests     100
   :mutations
   [["always blames the pool queue"
     (fn [c] (assoc (raw-diagnostic c) :cause :pool-queue-wait))]
    ["swaps the write and stage clocks"
     (fn [c] (let [d (raw-diagnostic c)]
               (assoc d :elapsed-ms (:stage-elapsed-ms d)
                      :stage-elapsed-ms (:elapsed-ms d))))]]})

(defn- stub-store
  "IMemoryStore stub; FIND-DUP is the find-duplicate body, ADD the add-entry!."
  [find-dup add]
  (reify proto/IMemoryStore
    (connect!       [_ _] {:success? true})
    (disconnect!    [_]   nil)
    (connected?     [_]   true)
    (health-check   [_]   {:healthy? true})
    (add-entry!     [_ e] (add e))
    (get-entry      [_ id] {:id id :type "note" :content "x" :tags []})
    (update-entry!  [_ id _] {:success? true :id id})
    (delete-entry!  [_ _] nil)
    (query-entries  [_ _] [])
    (search-similar [_ _ _] [])
    (supports-semantic-search? [_] false)
    (cleanup-expired! [_] {})
    (entries-expiring-soon [_ _ _] [])
    (find-duplicate [_ _ _ _] (find-dup))
    (store-status   [_] {})
    (reset-store!   [_] nil)))

(use-fixtures :each
  (fn [f]
    (let [snapshot (proto/registered-stores)]
      (proto/unregister-store! :default)
      (try
        (f)
        (finally
          (proto/unregister-store! :default)
          (doseq [[k store] snapshot]
            (when (= :default k)
              (proto/register-store! k store))))))))

(deftest handle-add-timeout-names-the-stalled-stage
  (proto/register-store! :default
                         (stub-store #(do (Thread/sleep 1500) nil)
                                     (fn [_] "id-never")))
  (let [res (binding [wr/*write-timeout-ms* 300]
              (wr/handle-add {:type "note" :content (str "stall-" (random-uuid))
                              :tags ["t"] :directory "/tmp"}))
        text (:text res)]
    (is (:isError res) "a timed-out write is an error")
    (is (str/includes? text "timed out after 300ms")
        "the historical prefix is kept")
    (is (str/includes? text ":dedup-lookup")
        "the stage the store stalled in is named")
    (is (str/includes? text ":stage-stalled") "the cause is named")
    (is (str/includes? text ":stage-elapsed-ms") "the stage clock is reported")))

(deftest handle-add-success-is-untouched-by-progress
  (let [seen (atom [])]
    (proto/register-store! :default
                           (stub-store (constantly nil)
                                       (fn [e] (swap! seen conj (:type e))
                                         (str "id-" (random-uuid)))))
    (let [res (with-redefs [hive-mcp.knowledge-graph.connection/with-tx-batch
                            (fn [body] (eval body))
                            hive-mcp.crystal.recall/register-created-id!
                            (constantly nil)]
                (wr/handle-add {:type "note" :content (str "ok-" (random-uuid))
                                :tags ["t"] :directory "/tmp"}))]
      (is (not (:isError res)) "a prompt write still succeeds")
      (is (= ["note"] @seen)))))

(deftest default-stage-budgets-fit-under-the-outer-ceiling
  (let [b wr/default-stage-budgets-ms
        longest (+ (:dedup-lookup b) (max (:index b) (:dedup-update b))
                   (:kg-edges b) (:link-outgoing b) (:fetch b))]
    (is (< longest wr/*write-timeout-ms*)
        "the longest stage path ends before the outer timeout, so a stage error fires first")))

(deftest handle-add-stage-over-budget-names-the-stage
  (proto/register-store! :default
                         (stub-store (constantly nil)
                                     (fn [_] (Thread/sleep 1500) "id-late")))
  (let [t0  (System/currentTimeMillis)
        res (binding [wr/*stage-budgets-ms* (assoc wr/default-stage-budgets-ms :index 200)]
              (wr/handle-add {:type "note" :content (str "slow-index-" (random-uuid))
                              :tags ["t"] :directory "/tmp"}))
        took (- (System/currentTimeMillis) t0)
        text (:text res)]
    (is (:isError res) "an over-budget stage is an error")
    (is (str/includes? text "stage :index exceeded its 200ms budget")
        "the stage that ran out of budget is named")
    (is (str/includes? text ":budget-ms 200") "the EDN diagnostic carries the budget")
    (is (< took 1400) "the add fails at the stage budget, not when the store returns")))

(deftest stage-budget-message-names-stage-and-budget
  (let [msg (wr/stage-budget-message {:stage :fetch :budget-ms 3000 :elapsed-ms 3001})]
    (is (str/includes? msg "stage :fetch exceeded its 3000ms budget"))
    (is (str/includes? msg "stage elapsed 3001ms"))))
