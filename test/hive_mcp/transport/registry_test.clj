(ns hive-mcp.transport.registry-test
  "TransportChain registry: config-driven ports, the open adapter set, the
   probe that hands :active back to a recovered higher-ranked adapter, the
   switch announcement, config reload, and the `transport status` view.

   Adapters are scripted defmethods on test-only ports (:tc-test*), so no
   real transport is touched; the switch sink is captured, never shouted."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-dsl.result :as r]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.merge :as merge]
            [hive-mcp.config.source :as source]
            [hive-mcp.server.config-reload :as reload]
            [hive-mcp.tools.events.core :as events]
            [hive-mcp.transport.chain :as chain]
            [hive-mcp.transport.registry :as reg]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Fixtures
;; =============================================================================

(def ^:private up (atom {}))
(def ^:private switches (atom []))

(defn- outcome [k op]
  (if (get @up k)
    (r/ok [k op])
    (r/err :transport/down {:error/kind :unavailable :message (str k " down")})))

(defmethod reg/open-adapter [:tc-test :a] [_ _]
  {:call  (fn [op] (outcome :a op))
   :probe (fn [] (outcome :a :probe))})

(defmethod reg/open-adapter [:tc-test :b] [_ _]
  {:call  (fn [op] (outcome :b op))
   :probe (fn [] (outcome :b :probe))})

(defmethod reg/open-adapter [:tc-test :c] [_ _]
  (fn [op] (outcome :c op)))

(def ^:private fast {:retry {:attempts 1 :backoff-ms [0]} :probe-interval-ms 0})

(defn- transports [chain-kws]
  {:defaults fast :tc-test {:chain chain-kws}})

(use-fixtures :each
  (fn [t]
    (reg/clear!)
    (reset! up {})
    (reset! switches [])
    (let [prev (reg/set-switch-sink! #(swap! switches conj %))]
      (try (t)
           (finally
             (reg/set-switch-sink! prev)
             (reg/clear!))))))

(defn- active [] (get-in (reg/status) [:ports :tc-test :active]))
(defn- moves [] (mapv (juxt :from :to :reason) @switches))

;; =============================================================================
;; Pure core
;; =============================================================================

(deftest ports-and-specs-come-from-config-with-defaults
  (testing "absent :transports means the shipped ports"
    (is (= [:delivery :events :git-editor :spawn :task-dispatch] (reg/ports nil)))
    (is (= [:nats-inbox :tmux :emacs :hivemind-shout] (:chain (reg/port-spec nil :delivery)))))
  (testing "a port entry is merged over :defaults, which is merged over the shipped defaults"
    (let [spec (reg/port-spec {:defaults {:probe-interval-ms 5}
                               :x {:chain [:p :q] :retry {:attempts 7}}}
                              :x)]
      (is (= [:p :q] (:chain spec)))
      (is (= 5 (:probe-interval-ms spec)))
      (is (= {:attempts 7 :backoff-ms [200 1000 5000]} (:retry spec)))
      (is (true? (:sticky? spec)))
      (is (m/validate chain/ChainSpec spec))))
  (testing ":defaults is not a port, an unknown port has an empty chain"
    (is (= [:x] (reg/ports {:defaults {} :x {:chain [:p]}})))
    (is (= [] (:chain (reg/port-spec {} :nope)))))
  (testing "config.edn defaults carry the section"
    (is (= merge/default-transports (:transports merge/default-config)))))

(deftest rebind-state-keeps-history-and-follows-config-order
  (let [spec-ab (chain/normalize-spec {:chain [:a :b]})
        spec-ba (chain/normalize-spec {:chain [:b :a]})
        st      {:active :b :adapters {:a {:status :degraded :fails 2}
                                       :b {:status :ok :last-ok-at 9 :fails 0}}}]
    (is (= :b (:active (reg/rebind-state st [:a :b] spec-ab))) "unchanged order keeps :active")
    (is (= :b (:active (reg/rebind-state st [:a :b] spec-ba))) "active promoted to head stays")
    (is (= 9 (get-in (reg/rebind-state st [:a :b] spec-ba) [:adapters :b :last-ok-at])))
    (is (nil? (:active (reg/rebind-state {:active :a :adapters {}} [:a :b] spec-ba)))
        "a newly higher-ranked adapter clears :active so config order is retried")
    (is (nil? (:active (reg/rebind-state st [:a :b] (chain/normalize-spec {:chain [:a :c]})))))
    (is (= {:status :unknown :fails 0}
           (get-in (reg/rebind-state st [:a :b] (chain/normalize-spec {:chain [:a :c]})) [:adapters :c])))))

(deftest switch-reasons
  (is (= :elected (reg/switch-reason [:a :b] nil :b)))
  (is (= :lost (reg/switch-reason [:a :b] :a nil)))
  (is (= :recovered (reg/switch-reason [:a :b] :b :a)))
  (is (= :failover (reg/switch-reason [:a :b] :a :b))))

;; =============================================================================
;; Effectful: call, probe, reload
;; =============================================================================

(deftest call-falls-back-and-announces-one-switch
  (reg/rebind! (transports [:a :b]))
  (reset! up {:a true :b true})
  (is (= :primary (get-in (reg/call! :tc-test 1) [:ok :resolved-by])))
  (swap! up assoc :a false)
  (let [res (reg/call! :tc-test 2)]
    (is (= [:b 2] (get-in res [:ok :value])))
    (is (= :fallback (get-in res [:ok :resolved-by])) "a fallback says it fired")
    (is (= [:a] (mapv :adapter (get-in res [:ok :skipped])))))
  (is (= [[nil :a :elected] [:a :b :failover]] (moves))
      "the failover inside one call is a->b, not a->none->b"))

(deftest probe-recovers-a-higher-ranked-adapter
  (reg/rebind! (transports [:a :b]))
  (reset! up {:a false :b true})
  (reg/call! :tc-test 1)
  (is (= :b (active)))
  (testing "while :a stays down the probe leaves :b active"
    (is (= {:port :tc-test :probed [{:adapter :a :ok? false}] :active :b}
           (reg/probe-port! :tc-test))))
  (swap! up assoc :a true)
  (testing "once :a is back the probe hands :active back to it"
    (is (= :a (:active (reg/probe-port! :tc-test))))
    (is (= :primary (get-in (reg/call! :tc-test 2) [:ok :resolved-by])))
    (is (= [:elected :recovered] (mapv :reason @switches)))))

(deftest probe-loop-recovers-in-the-background
  (reg/rebind! {:defaults {:retry {:attempts 1 :backoff-ms [0]} :probe-interval-ms 20}
                :tc-test {:chain [:a :b]}})
  (reset! up {:a false :b true})
  (reg/call! :tc-test 1)
  (is (= {:tc-test 20} (select-keys (reg/start-probe-loops!) [:tc-test])))
  (swap! up assoc :a true)
  (is (= :a (loop [n 0]
              (let [a (active)]
                (if (or (= :a a) (> n 100)) a (do (Thread/sleep 20) (recur (inc n))))))))
  (is (true? (get-in (reg/status) [:ports :tc-test :probe-loop?])))
  (reg/stop-probe-loops!)
  (is (false? (:probe-loops? (reg/status)))))

(deftest config-reload-changes-the-order
  (let [cfg (atom (assoc merge/default-config :transports (transports [:a :b])))]
    (binding [config/*config-source* (source/atom-source cfg merge/default-config)]
      (reg/rebind-from-config!)
      (reset! up {:a true :b true})
      (is (= :a (get-in (reg/call! :tc-test 1) [:ok :via])))
      (let [old @cfg
            new (assoc old :transports (transports [:b :a]))]
        (reset! cfg new)
        (testing "the config-reload plan re-runs the transports step"
          (is (= [:transports] (reload/plan old new)))
          (is (= [:transports]
                 (:ran (reload/rewire! old new {:transports reg/rebind-from-config!}))))))
      (is (= [:b :a] (get-in (reg/status) [:ports :tc-test :chain])))
      (let [res (reg/call! :tc-test 2)]
        (is (= :b (get-in res [:ok :via])) "the new head of the chain serves")
        (is (= :primary (get-in res [:ok :resolved-by]))))
      (is (= [[nil :a :elected] [:a nil :reconfigured] [nil :b :elected]] (moves))))))

(deftest addressing-and-unregistered
  (reg/rebind! (assoc (transports [:z :c]) :tc-test {:chain [:z :c]}))
  (reset! up {:c true})
  (testing "an unconfigured port is an addressing error"
    (is (= :addressing (:error/kind (reg/call! :not-a-port 1)))))
  (testing "an adapter no defmethod answers is skipped as :unregistered and the next serves"
    (let [res (reg/call! :tc-test 1)]
      (is (= :c (get-in res [:ok :via])))
      (is (= [{:adapter :z :reason :unregistered}] (get-in res [:ok :skipped])))
      (is (false? (get-in (reg/status) [:ports :tc-test :adapters :z :registered?]))))))

(deftest status-view-and-events-verb
  (reg/rebind! (transports [:a :b :c]))
  (reset! up {:b true})
  (reg/call! :tc-test 1)
  (let [st (reg/status)
        v  (get-in st [:ports :tc-test])]
    (is (m/validate reg/Status st))
    (is (= [:a :b :c] (:chain v)))
    (is (= :b (:active v)))
    (is (= :fallback (:resolved-by v)))
    (is (= :degraded (get-in v [:adapters :a :status])))
    (is (int? (get-in v [:adapters :b :last-ok-at])))
    (is (false? (get-in v [:adapters :c :probe?])) "a bare-fn adapter has no probe")
    (is (= [[nil :b :elected]] (mapv (juxt :from :to :reason) (:switches v)))))
  (testing "events transport-status and \"transport status\" both answer"
    (doseq [cmd ["transport-status" "transport status"]]
      (let [resp (events/handle {:command cmd})]
        (is (not (:isError resp)) cmd)))))
