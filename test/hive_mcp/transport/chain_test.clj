(ns hive-mcp.transport.chain-test
  "Trifecta (golden + property + mutation) for the pure TransportChain core.

   Subjects: chain/order, chain/step, chain/probe-candidates, chain/run-chain!.
   Laws pinned as properties: sticky law, config-order recovery, an addressing
   error never falls back, every Result names :via.

   Mutants wrap or replace the subject and never depend on the test fixtures."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-dsl.result :as r]
            [hive-mcp.transport.chain :as chain]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Fixtures
;; =============================================================================

(def ^:private spec-abc
  {:chain [:a :b :c]
   :retry {:attempts 2 :backoff-ms [10 20]}
   :probe-interval-ms 1000
   :sticky? true})

(defn- ad
  [v]
  (if (map? v) (merge {:fails 0} v) {:status v :fails 0}))

(defn- st
  "ChainState from an active adapter and {kw status-or-entry}."
  [active adapters]
  {:active active
   :adapters (into {} (map (fn [[k v]] [k (ad v)])) adapters)})

(def ^:private all-unknown-state (st nil {:a :unknown :b :unknown :c :unknown}))

(def ^:private fail-out (r/err :transport/connection-refused {:error/kind :unavailable}))
(def ^:private addr-out (r/err :addressing/unknown-id {:error/kind :addressing}))

(defn- scripted
  "Adapters from {kw behaviour-or-script}. A behaviour is :ok :fail :addr or
   :throw; a vector is a per-call script whose last entry repeats. Returns
   [adapters calls-atom]."
  [behaviours]
  (let [calls (atom {})]
    [(into {}
           (map (fn [[k b]]
                  [k (fn [_op]
                       (let [n   (get (swap! calls update k (fnil inc 0)) k)
                             sc  (if (vector? b) b [b])
                             beh (nth sc (min (dec n) (dec (count sc))))]
                         (case beh
                           :ok    (r/ok k)
                           :fail  fail-out
                           :addr  addr-out
                           :throw (throw (ex-info "boom" {})))))]))
           behaviours)
     calls]))

(defn- no-sleep [_] nil)

(defn- run-scenario
  "Drive `runner` (run-chain! or a mutant) with scripted adapters."
  [runner {:keys [spec state behaviours]}]
  (let [[adapters calls] (scripted behaviours)
        state-atom (atom (or state (chain/init-state spec)))
        result (runner spec state-atom adapters :op
                       {:now-fn (constantly 5000) :sleep-fn no-sleep})]
    {:result result :state @state-atom :calls @calls}))

(defn- via-of
  [result]
  (if (r/ok? result) (get-in result [:ok :via]) (:via result)))

;; =============================================================================
;; Generators
;; =============================================================================

(def ^:private gen-status (gen/elements [:ok :degraded :unknown]))

(def ^:private gen-state
  (gen/let [active (gen/elements [nil :a :b :c])
            sa gen-status sb gen-status sc gen-status
            t  (gen/choose 0 100000)]
    (st active {:a {:status sa :last-ok-at t}
                :b {:status sb :last-ok-at t}
                :c {:status sc :last-ok-at t}})))

(def ^:private gen-spec
  (gen/let [sticky? gen/boolean
            interval (gen/choose 0 5000)]
    (assoc spec-abc :sticky? sticky? :probe-interval-ms interval)))

(def ^:private gen-behaviour (gen/elements [:ok :fail :addr :throw]))

(def ^:private gen-scenario
  (gen/let [spec gen-spec
            state gen-state
            ba gen-behaviour bb gen-behaviour bc gen-behaviour]
    {:spec spec :state state :behaviours {:a ba :b bb :c bc}}))

;; =============================================================================
;; order — golden + property + mutation
;; =============================================================================

(def ^:private healthy (st :b {:a :ok :b :ok :c :ok}))

(deftrifecta order-contract
  hive-mcp.transport.chain/order
  {:golden-path "test/golden/transport/chain-order.edn"
   :apply?      true
   :cases       {:sticky-active-first   [healthy spec-abc]
                 :cold-config-order     [all-unknown-state spec-abc]
                 :degraded-skipped      [(st nil {:a :degraded :b :ok :c :ok}) spec-abc]
                 :degraded-active       [(st :b {:a :ok :b :degraded :c :ok}) spec-abc]
                 :non-sticky-ignores-active [healthy (assoc spec-abc :sticky? false)]
                 :all-degraded-empty    [(st nil {:a :degraded :b :degraded :c :degraded}) spec-abc]}
   :gen         (gen/tuple gen-state gen-spec)
   :pred        vector?
   :num-tests   200
   :mutations   [["ignores-sticky"
                  (fn [_state spec] (vec (:chain spec)))]
                 ["includes-degraded"
                  (fn [state spec]
                    (let [c (:chain spec) a (:active state)]
                      (if (some #{a} c) (into [a] (remove #{a}) c) (vec c))))]
                 ["always-sticky"
                  (fn [state spec]
                    (let [c (remove #(= :degraded (get-in state [:adapters % :status])) (:chain spec))
                          a (:active state)]
                      (if (some #{a} c) (into [a] (remove #{a}) c) (vec c))))]
                 ["active-last"
                  (fn [state spec]
                    (let [c (remove #(= :degraded (get-in state [:adapters % :status])) (:chain spec))
                          a (:active state)]
                      (if (some #{a} c) (conj (vec (remove #{a} c)) a) (vec c))))]]
   :assert      (fn []
                  (is (= [:b :a :c] (chain/order healthy spec-abc)))
                  (is (= [:b :c] (chain/order (st nil {:a :degraded :b :ok :c :ok}) spec-abc)))
                  (is (= [:a :c] (chain/order (st :b {:a :ok :b :degraded :c :ok}) spec-abc)))
                  (is (= [:a :b :c] (chain/order healthy (assoc spec-abc :sticky? false)))))})

;; =============================================================================
;; step — golden + property + mutation
;; =============================================================================

(def ^:private gen-outcome
  (gen/elements [(r/ok :v) fail-out addr-out]))

(deftrifecta step-contract
  hive-mcp.transport.chain/step
  {:golden-path "test/golden/transport/chain-step.edn"
   :apply?      true
   :cases       {:ok-becomes-active    [all-unknown-state :b (r/ok :v) 100]
                 :ok-recovers-degraded [(st :c {:a :degraded :b :ok :c :ok}) :a (r/ok :v) 200]
                 :fail-degrades        [(st :a {:a :ok :b :ok :c :ok}) :b fail-out 300]
                 :fail-active-clears   [(st :a {:a :ok :b :ok :c :ok}) :a fail-out 400]
                 :fail-counts          [(st nil {:a {:status :degraded :fails 2}}) :a fail-out 500]
                 :addressing-no-change [(st :a {:a :ok :b :ok :c :ok}) :a addr-out 600]}
   :gen         (gen/tuple gen-state (gen/elements [:a :b :c]) gen-outcome
                           (gen/choose 0 100000))
   :pred        map?
   :num-tests   200
   :mutations   [["ok-keeps-old-active"
                  (fn [state a out _now]
                    (if (r/ok? out) (assoc-in state [:adapters a :status] :ok) state))]
                 ["addressing-degrades"
                  (fn [state a out now]
                    (if (r/ok? out)
                      (assoc (assoc-in state [:adapters a] {:status :ok :last-ok-at now :fails 0}) :active a)
                      (assoc-in state [:adapters a :status] :degraded)))]
                 ["fail-keeps-active"
                  (fn [state a out now]
                    (cond (r/ok? out) (assoc (assoc-in state [:adapters a] {:status :ok :last-ok-at now :fails 0}) :active a)
                          (= :addressing (:error/kind out)) state
                          :else (assoc-in state [:adapters a :status] :degraded)))]
                 ["ok-forgets-time"
                  (fn [state a out _now]
                    (if (r/ok? out)
                      (assoc (assoc-in state [:adapters a] {:status :ok :fails 0}) :active a)
                      state))]]
   :assert      (fn []
                  (let [s1 (chain/step all-unknown-state :b (r/ok :v) 100)]
                    (is (= :b (:active s1)))
                    (is (= 100 (get-in s1 [:adapters :b :last-ok-at])))
                    (is (= :ok (get-in s1 [:adapters :b :status]))))
                  (let [s2 (chain/step (st :a {:a :ok :b :ok}) :a fail-out 400)]
                    (is (nil? (:active s2)))
                    (is (= :degraded (get-in s2 [:adapters :a :status])))
                    (is (= 1 (get-in s2 [:adapters :a :fails]))))
                  (let [s3 (st :a {:a :ok :b :ok})]
                    (is (= s3 (chain/step s3 :a addr-out 600)))))})

;; =============================================================================
;; probe-candidates — golden + property + mutation
;; =============================================================================

(def ^:private probe-state
  (st :c {:a {:status :degraded :last-ok-at 0 :last-attempt-at 4500}
          :b {:status :degraded :last-ok-at 0 :last-attempt-at 1000}
          :c {:status :ok :last-ok-at 5000}}))

(deftrifecta probe-candidates-contract
  hive-mcp.transport.chain/probe-candidates
  {:golden-path "test/golden/transport/chain-probe-candidates.edn"
   :apply?      true
   :cases       {:only-due-above-active [probe-state spec-abc 5000]
                 :all-due-later         [probe-state spec-abc 9000]
                 :active-first-nothing  [(st :a {:a :ok :b :ok :c :ok}) spec-abc 9000]
                 :nothing-active-all    [all-unknown-state spec-abc 0]
                 :interval-boundary     [probe-state spec-abc 5500]}
   :gen         (gen/tuple gen-state gen-spec (gen/choose 0 200000))
   :pred        vector?
   :num-tests   200
   :mutations   [["ignores-interval"
                  (fn [state spec _now]
                    (let [c (:chain spec) a (:active state)]
                      (vec (if (some #{a} c) (take-while #(not= a %) c) c))))]
                 ["includes-below-active"
                  (fn [state spec now]
                    (vec (filter #(>= (- now (or (get-in state [:adapters % :last-ok-at]) 0))
                                      (:probe-interval-ms spec))
                                 (:chain spec))))]
                 ["reverses-config-order"
                  (fn [state spec now]
                    (let [c (:chain spec) a (:active state)
                          above (if (some #{a} c) (take-while #(not= a %) c) c)]
                      (vec (reverse (filter #(>= (- now (max (or (get-in state [:adapters % :last-ok-at]) 0)
                                                             (or (get-in state [:adapters % :last-attempt-at]) 0)))
                                                 (:probe-interval-ms spec))
                                            above)))))]]
   :assert      (fn []
                  (is (= [:b] (chain/probe-candidates probe-state spec-abc 5000)))
                  (is (= [:a :b] (chain/probe-candidates probe-state spec-abc 5500)))
                  (is (= [] (chain/probe-candidates (st :a {:a :ok :b :ok :c :ok}) spec-abc 9000)))
                  (is (= [:a :b :c] (chain/probe-candidates all-unknown-state spec-abc 0))))})

;; =============================================================================
;; run-chain! — subject wrapper, mutant runners
;; =============================================================================

(defn run-real
  "Unary subject: scenario -> {:result :state :calls}. Goes through the var
   chain/run-chain!, so a mutation of that var is seen here."
  [scenario]
  (run-scenario chain/run-chain! scenario))

(defn- mini-runner
  "A self-contained runner for mutants, built on the pure fns only. Flags turn
   one behaviour off; the default is the correct fallback walk (no retry)."
  [{:keys [fallback? addr-fallback? annotate? update? resolved]
    :or {fallback? true annotate? true update? true}}]
  (fn [spec sa ads op opts]
    (let [spec (chain/normalize-spec spec)
          now  ((:now-fn opts))]
      (loop [todo (chain/order @sa spec), skipped []]
        (if-let [k (first todo)]
          (let [out ((get ads k) op)]
            (when update? (swap! sa chain/step k out now))
            (cond
              (r/ok? out)
              (if annotate?
                (r/ok {:value (:ok out) :via k :skipped skipped
                       :resolved-by (if resolved resolved
                                        (if (= k (first (:chain spec))) :primary :fallback))})
                out)

              (and (chain/addressing? out) (not addr-fallback?))
              (assoc out :via k :skipped skipped)

              (not fallback?)
              (assoc out :via k :skipped skipped)

              :else (recur (rest todo) (conj skipped {:adapter k}))))
          (r/err :transport/chain-exhausted {:via nil :skipped skipped}))))))

(defn- fresh
  "A scenario over spec-abc with the given behaviours and optional state."
  ([behaviours] (fresh behaviours nil))
  ([behaviours state]
   {:spec spec-abc :state state :behaviours behaviours}))

(def ^:private run-cases
  {:primary-ok        (fresh {:a :ok :b :ok :c :ok})
   :fallback-to-b     (fresh {:a :fail :b :ok :c :ok})
   :retry-then-ok     (fresh {:a [:fail :ok] :b :ok :c :ok})
   :thrown-is-failure (fresh {:a :throw :b :ok :c :ok})
   :sticky-b          (fresh {:a :ok :b :ok :c :ok} (st :b {:a :ok :b :ok :c :ok}))
   :degraded-skipped  (fresh {:a :ok :b :ok :c :ok} (st nil {:a :degraded :b :ok :c :ok}))
   :addressing-stops  (fresh {:a :addr :b :ok :c :ok})
   :all-fail          (fresh {:a :fail :b :fail :c :fail})
   :all-degraded-try  (fresh {:a :fail :b :ok :c :ok}
                             (st nil {:a :degraded :b :degraded :c :degraded}))})

(defn- mutant-runner
  "A subject mutant: scenario -> outcome, via a mini-runner with `flags`."
  [flags]
  (fn [scenario] (run-scenario (mini-runner flags) scenario)))

(deftrifecta run-chain-contract
  hive-mcp.transport.chain-test/run-real
  {:golden-path "test/golden/transport/chain-run.edn"
   :cases       run-cases
   :gen         gen-scenario
   :pred        (fn [out] (keyword? (via-of (:result out))))
   :num-tests   200
   :mutations   [["no-fallback" (mutant-runner {:fallback? false})]
                 ["addressing-falls-back" (mutant-runner {:addr-fallback? true})]
                 ["no-annotation" (mutant-runner {:annotate? false})]
                 ["state-never-updated" (mutant-runner {:update? false})]
                 ["always-primary" (mutant-runner {:resolved :primary})]]
   :assert      (fn []
                  (let [{:keys [result state]} (run-real (:fallback-to-b run-cases))]
                    (is (= :b (get-in result [:ok :via])))
                    (is (= :fallback (get-in result [:ok :resolved-by])))
                    (is (= :b (:active state)))
                    (is (= [:a] (mapv :adapter (get-in result [:ok :skipped])))))
                  (let [{:keys [result calls]} (run-real (:addressing-stops run-cases))]
                    (is (= :addressing (:error/kind result)))
                    (is (= {:a 1} calls)))
                  (let [{:keys [result state]} (run-real (:primary-ok run-cases))]
                    (is (= :primary (get-in result [:ok :resolved-by])))
                    (is (= :a (:active state))))
                  (let [{:keys [result]} (run-real (:all-fail run-cases))]
                    (is (r/err? result))
                    (is (= :transport/chain-exhausted (:error result)))))})

;; =============================================================================
;; Law properties
;; =============================================================================

(defn- ok-adapters
  "Adapters that always succeed for the keys in `up`, fail for the rest."
  [chain-keys up]
  (into {} (map (fn [k] [k (if (contains? up k) (fn [_] (r/ok k)) (fn [_] fail-out))]))
        chain-keys))

(defspec sticky-law-property 200
  ;; Law 2: once an adapter works it is :active and is tried first next call,
  ;; even when a higher-ranked adapter has since come back.
  (prop/for-all [up (gen/not-empty (gen/set (gen/elements [:a :b :c])))]
    (let [sa  (atom (chain/init-state spec-abc))
          opts {:now-fn (constantly 1) :sleep-fn no-sleep}
          r1  (chain/run-chain! spec-abc sa (ok-adapters [:a :b :c] up) :op opts)
          w1  (get-in r1 [:ok :via])
          r2  (chain/run-chain! spec-abc sa (ok-adapters [:a :b :c] #{:a :b :c}) :op opts)]
      (and (r/ok? r1)
           (= w1 (first (filter up [:a :b :c])))
           (= w1 (:active @sa))
           (= w1 (get-in r2 [:ok :via]))))))

(defspec config-order-recovery-property 200
  ;; Law 4: a recovered adapter ranked above :active is a probe candidate once
  ;; due, and a successful call from it takes :active back.
  (prop/for-all [lower (gen/elements [:b :c])
                 gap   (gen/choose 1000 50000)]
    (let [above (take-while #(not= lower %) [:a :b :c])
          state (-> (chain/init-state spec-abc)
                    (as-> s (reduce #(chain/step %1 %2 fail-out 0) s above))
                    (chain/step lower (r/ok :v) 10))
          cands (chain/probe-candidates state spec-abc (+ 10 gap))
          top   (first above)
          rec   (chain/step state top (r/ok :v) (+ 10 gap))]
      (and (= above cands)
           (= top (:active rec))
           (empty? (chain/probe-candidates rec spec-abc (+ 10 gap)))
           (= top (first (chain/order rec spec-abc)))))))

(defspec addressing-never-falls-back-property 200
  ;; Law 7: an addressing error stops the walk; no later adapter is called.
  (prop/for-all [pos (gen/elements [0 1 2])
                 later (gen/vector gen-behaviour 3)]
    (let [ks    [:a :b :c]
          at    (nth ks pos)
          behav (into {} (map-indexed (fn [i k]
                                        [k (cond (< i pos) :fail
                                                 (= i pos) :addr
                                                 :else (nth later i))]))
                      ks)
          {:keys [result calls]} (run-scenario chain/run-chain! (fresh behav))
          after (drop (inc pos) ks)]
      (and (= :addressing (:error/kind result))
           (= at (:via result))
           (every? #(nil? (get calls %)) after)))))

(deftest backoff-schedule
  (testing "delay per failed attempt; the last value repeats; empty is 0"
    (is (= [200 1000 5000 5000]
           (mapv #(chain/backoff-delay {:attempts 5 :backoff-ms [200 1000 5000]} %) [1 2 3 4])))
    (is (= 0 (chain/backoff-delay {:attempts 2 :backoff-ms []} 1))))
  (testing "the runner sleeps between attempts, never after the last one"
    (let [slept (atom [])
          sa    (atom (chain/init-state spec-abc))
          spec  (assoc spec-abc :retry {:attempts 3 :backoff-ms [7 11]})]
      (chain/run-chain! spec sa {:a (fn [_] fail-out) :b (fn [_] (r/ok :b))} :op
                        {:now-fn (constantly 1) :sleep-fn #(swap! slept conj %)})
      (is (= [7 11] @slept)))))

(defspec states-conform-to-schema-property 200
  (prop/for-all [sc gen-scenario]
    (let [{:keys [state]} (run-scenario chain/run-chain! sc)]
      (and (m/validate chain/ChainState state)
           (m/validate chain/ChainSpec (chain/normalize-spec (:spec sc)))))))

(defspec result-always-names-via-property 200
  ;; Law 6: every ok Result names :via, :resolved-by and :skipped.
  (prop/for-all [sc gen-scenario]
    (let [{:keys [result]} (run-scenario chain/run-chain! sc)]
      (if (r/ok? result)
        (let [v (:ok result)]
          (and (keyword? (:via v))
               (contains? #{:primary :fallback} (:resolved-by v))
               (vector? (:skipped v))
               (= (:resolved-by v) (if (= (:via v) :a) :primary :fallback))))
        (contains? result :via)))))
