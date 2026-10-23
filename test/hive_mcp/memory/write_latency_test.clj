(ns hive-mcp.memory.write-latency-test
  "Write-path health for memory add (kanban 20260928204307-01ca0962).

   Facets for the pure ring + health fold (`ring-health`):
   - golden:   fast adds stay :ok, slow adds go :degraded, ring stays bounded
   - property: p50 <= p95 <= max per stage, ring never exceeds capacity,
               :degraded iff p95 total > threshold
   - mutation: ignoring capacity, and judging on p50 instead of p95, are caught

   The handle-add tests drive the real add path through a stub store whose
   upsert sleeps: no live Milvus, no with-redefs."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.memory.write-latency :as wl]
            [hive-mcp.tools.catchup.outcome :as outcome]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.tools.memory.crud.write :as wr]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private gauges0
  {:embed-gate {:capacity 8 :free 8 :queued 0}
   :memory-pool {:active 0 :queued 0 :pool-size 0}})

(defn- raw-ring-health
  "Fold `totals` (ms per add, index time = half) into a ring of `capacity`
   and judge it. Mutants build on this, never on the subject."
  [{:keys [capacity totals threshold]} {:keys [record-fn pick]
                                        :or {record-fn wl/record pick :p95}}]
  (let [ring (reduce (fn [r t]
                       (record-fn r (wl/canonical-sample
                                     {:timings {:dedup-lookup 1 :index (quot t 2) :fetch 1}
                                      :embed-ms (quot t 4) :total-ms t :outcome :ok})))
                     (wl/empty-ring capacity) totals)
        s (wl/summarize ring)
        judged (get-in s [:stages :total pick] 0)
        h (wl/health (assoc-in s [:stages :total :p95] judged) gauges0 threshold)]
    {:n (count (:samples ring))
     :capacity capacity
     :stages (:stages s)
     :threshold threshold
     :status (:status h)}))

(defn ring-health
  "Subject: ring size, per-stage stats and status for a description."
  [c]
  (raw-ring-health c {}))

(def ^:private cases
  {:fast-adds {:capacity 8 :threshold 10000 :totals [200 300 250 400]}
   :slow-tail {:capacity 8 :threshold 10000 :totals [200 300 31000 30000 29000 300 200 250]}
   :bounded   {:capacity 3 :threshold 10000 :totals [30000 30000 30000 100 100 100]}
   :empty     {:capacity 4 :threshold 10000 :totals []}})

(def ^:private gen-case
  (gen/let [capacity (gen/choose 1 16)
            totals (gen/vector (gen/choose 0 40000) 0 40)]
    {:capacity capacity :threshold 10000 :totals totals}))

(defn- well-formed?
  "Property predicate (receives OUTPUT)."
  [{:keys [n capacity stages threshold status]}]
  (and (<= n capacity)
       (every? (fn [[_ {:keys [p50 p95 max]}]] (<= p50 p95 max)) stages)
       (= status (if (> (get-in stages [:total :p95] 0) threshold) :degraded :ok))))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(deftrifecta ring-health-contract
  hive-mcp.memory.write-latency-test/ring-health
  {:golden-path   "test/golden/memory/write-latency-ring-health.edn"
   :cases         cases
   :gen           gen-case
   :pred          well-formed?
   :property-type :pred
   :num-tests     100
   :mutations
   [["ignores ring capacity"
     (fn [c] (raw-ring-health c {:record-fn (fn [r s] (update r :samples conj s))}))]
    ["judges health on p50 instead of p95"
     (fn [c] (raw-ring-health c {:pick :p50}))]]})

(deftest canonical-sample-splits-embed-out-of-index
  (is (= {:outcome :ok
          :stages {:total 900 :dedup 15 :embed 300 :upsert 500 :kg 40 :read-after-write 20}}
         (wl/canonical-sample {:timings {:dedup-lookup 15 :index 800 :kg-edges 30
                                         :link-outgoing 10 :fetch 20}
                               :embed-ms 300 :total-ms 900 :outcome :ok}))))

(deftest with-write-health-never-softens-a-store-failure
  (let [slow {:status :degraded :warnings [{:label "memory-write-latency" :status :degraded}]
              :write-path {:stages {}}}]
    (is (= :degraded (:status (outcome/with-write-health {:status :ok :warnings []} slow))))
    (is (= :error (:status (outcome/with-write-health
                            {:status :error :warnings [{:label "x" :status :error}]} slow))))
    (is (= {:status :ok :warnings []} (outcome/with-write-health {:status :ok :warnings []} nil)))))

(deftest gate-gauge-reads-free-permits-and-queue
  (let [g {:permits 8 :interactive-reserve 0 :active (atom 3)
           :waiting (atom {:interactive 2 :batch 5})}]
    (is (= {:capacity 8 :free 5 :queued 7} (wl/gate-gauge g)))))

;; ---------------------------------------------------------------------------
;; Real add path, stub store whose upsert sleeps
;; ---------------------------------------------------------------------------

(defn- sleepy-store
  "IMemoryStore stub whose add-entry! (the upsert) sleeps `upsert-ms`."
  [upsert-ms]
  (reify proto/IMemoryStore
    (connect!       [_ _] {:success? true})
    (disconnect!    [_]   nil)
    (connected?     [_]   true)
    (health-check   [_]   {:healthy? true})
    (add-entry!     [_ _] (Thread/sleep (long upsert-ms)) (str "id-" (random-uuid)))
    (get-entry      [_ id] {:id id :type "note" :content "x" :tags []})
    (update-entry!  [_ id _] {:success? true :id id})
    (delete-entry!  [_ _] nil)
    (query-entries  [_ _] [])
    (search-similar [_ _ _] [])
    (supports-semantic-search? [_] false)
    (cleanup-expired! [_] {})
    (entries-expiring-soon [_ _ _] [])
    (find-duplicate [_ _ _ _] nil)
    (store-status   [_] {})
    (reset-store!   [_] nil)))

(use-fixtures :each
  (fn [f]
    (let [snapshot (proto/registered-stores)
          ring-before @wl/ring]
      (proto/unregister-store! :default)
      (reset! wl/ring (wl/empty-ring))
      (try
        (f)
        (finally
          (reset! wl/ring ring-before)
          (proto/unregister-store! :default)
          (doseq [[k store] snapshot]
            (when (= :default k)
              (proto/register-store! k store))))))))

(defn- add! []
  (wr/handle-add {:type "note" :content (str "lat-" (random-uuid))
                  :tags ["t"] :directory "/tmp"}))

(deftest slow-upsert-is-attributed-to-the-upsert-stage
  (proto/register-store! :default (sleepy-store 250))
  (let [res (add!)
        {:keys [stages outcomes]} (wl/summarize @wl/ring)]
    (is (not (:isError res)) (str "add succeeds: " (:text res)))
    (is (= {:ok 1} outcomes))
    (is (>= (get-in stages [:upsert :max]) 250) "the sleep lands in :upsert")
    (is (>= (get-in stages [:total :max]) (get-in stages [:upsert :max])))
    (is (contains? stages :dedup))
    (is (contains? stages :read-after-write))))

(deftest slow-adds-turn-write-health-degraded
  (proto/register-store! :default (sleepy-store 120))
  (dotimes [_ 3] (add!))
  (let [s (wl/summarize @wl/ring)]
    (is (= :ok (:status (wl/health s gauges0 10000))) "120ms adds are healthy at 10s")
    (is (= :degraded (:status (wl/health s gauges0 100)))
        "the same adds are degraded against a 100ms threshold")
    (is (= #{:embed-gate :memory-pool} (set (keys (wl/gauges)))) "live gauges read")))

(deftest timed-out-add-is-recorded-as-timeout
  (proto/register-store! :default (sleepy-store 1500))
  (let [res (binding [wr/*write-timeout-ms* 200
                      wr/*stage-budgets-ms* {}]
              (add!))]
    (is (:isError res))
    (is (= 1 (get-in (wl/summarize @wl/ring) [:outcomes :timeout])))))
