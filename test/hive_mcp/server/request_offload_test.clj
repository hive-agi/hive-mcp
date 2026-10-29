(ns hive-mcp.server.request-offload-test
  "MCP-CONCURRENT-REQUESTS: request handlers run off jsonrpc4clj's read loop.

   Each test drives a real jsonrpc4clj ChanServer over core.async channels with
   stub request methods registered in the SDK multimethod (removed afterwards).
   No with-redefs."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.core.async :as a]
            [jsonrpc4clj.server :as js]
            [hive-mcp.server.request-offload :as offload]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private gates (atom {}))
(def ^:private running (atom 0))
(def ^:private peak (atom 0))
(def ^:private interrupted (atom #{}))

(defn- block-method [_ context {:keys [gate]}]
  (let [n (swap! running inc)]
    (swap! peak max n)
    (try
      (deref (get @gates gate) 5000 :gate-timeout)
      {:ok :released :gate gate}
      (catch InterruptedException _
        (swap! interrupted conj gate)
        (throw (InterruptedException.)))
      (finally
        (when @(::js/req-cancelled? context (atom false))
          (swap! interrupted conj gate))
        (swap! running dec)))))

(defn- fast-method [_ _ _] {:ok :fast})

(defn- stubs-fixture [f]
  (.addMethod ^clojure.lang.MultiFn js/receive-request "probe/block" block-method)
  (.addMethod ^clojure.lang.MultiFn js/receive-request "probe/fast" fast-method)
  (offload/offload! "probe/block")
  (offload/offload! "probe/fast")
  (reset! gates {}) (reset! running 0) (reset! peak 0) (reset! interrupted #{})
  (try (f)
       (finally
         (remove-method js/receive-request "probe/block")
         (remove-method js/receive-request "probe/fast"))))

(use-fixtures :each stubs-fixture)

(defn- start-server
  "A ChanServer started over CONTEXT-FN's context. Returns {:in :out :srv :ctx}."
  [context-fn]
  (let [in (a/chan 64) out (a/chan 64)
        srv (js/chan-server {:input-ch in :output-ch out})
        ctx (context-fn {:server srv})]
    (js/start srv ctx)
    {:in in :out out :srv srv :ctx ctx}))

(defn- stop [{:keys [srv ctx]}]
  (doseq [[_ g] @gates] (deliver g true))
  (offload/detach! ctx)
  (js/shutdown srv))

(defn- req [{:keys [in]} id method params]
  (a/>!! in {:jsonrpc "2.0" :id id :method method :params params}))

(defn- notify [{:keys [in]} method params]
  (a/>!! in {:jsonrpc "2.0" :method method :params params}))

(defn- take-resp [{:keys [out]} ms]
  (a/alt!! out ([v] v) (a/timeout ms) :timeout))

(defn- gate! [k] (let [g (promise)] (swap! gates assoc k g) g))

(deftest baseline-read-loop-is-serial-without-executor
  (testing "measure: with no executor in the context a blocked call holds the next one"
    (let [s (start-server identity)]
      (try
        (gate! :a)
        (req s 1 "probe/block" {:gate :a})
        (req s 2 "probe/fast" {})
        (is (= :timeout (take-resp s 500)) "fast call waits behind the blocked one")
        (deliver (get @gates :a) true)
        (is (= #{1 2} (set (map :id [(take-resp s 2000) (take-resp s 2000)]))))
        (finally (stop s))))))

(deftest blocked-call-does-not-delay-second-call
  (let [s (start-server #(offload/attach % 4))]
    (try
      (gate! :a)
      (req s 1 "probe/block" {:gate :a})
      (req s 2 "probe/fast" {})
      (let [r (take-resp s 1000)]
        (is (= 2 (:id r)) "fast reply arrives while id 1 is still blocked")
        (is (= {:ok "fast"} (update (:result r) :ok name))))
      (deliver (get @gates :a) true)
      (let [r (take-resp s 2000)]
        (is (= 1 (:id r)) "blocked call answers later under its own id")
        (is (= :released (get-in r [:result :ok]))))
      (finally (stop s)))))

(deftest cancel-request-reaches-blocked-call
  (doseq [[label cancel!] [["$/cancelRequest" #(notify % "$/cancelRequest" {:id 7})]
                           ["notifications/cancelled" #(notify % "notifications/cancelled" {:requestId 7})]]]
    (testing label
      (reset! interrupted #{})
      (let [s (start-server #(offload/attach % 2))]
        (try
          (gate! :c)
          (req s 7 "probe/block" {:gate :c})
          (Thread/sleep 100)
          (cancel! s)
          (let [r (take-resp s 2000)]
            (is (= 7 (:id r)))
            (is (= -32800 (get-in r [:error :code])) "answered RequestCancelled"))
          (Thread/sleep 200)
          (is (contains? @interrupted :c) "the handler saw the cancel")
          (finally (stop s)))))))

(deftest executor-bound-is-honoured
  (let [s (start-server #(offload/attach % 2))]
    (try
      (doseq [k [:p1 :p2 :p3 :p4]] (gate! k))
      (doseq [[i k] (map-indexed vector [:p1 :p2 :p3 :p4])]
        (req s (inc i) "probe/block" {:gate k}))
      (Thread/sleep 300)
      (is (= 2 @running) "only two handlers run at once")
      (doseq [k [:p1 :p2 :p3 :p4]] (deliver (get @gates k) true))
      (is (= #{1 2 3 4} (set (map :id (repeatedly 4 #(take-resp s 2000))))))
      (is (= 2 @peak))
      (finally (stop s)))))

(deftest initialize-stays-on-read-loop
  (testing "initialize is not wrapped, so it answers before anything read after it"
    (offload/install!)
    (is (not (offload/offloaded? (get-method js/receive-request "initialize"))))
    (is (offload/offloaded? (get-method js/receive-request "tools/call")))))

(deftest concurrency-from-config
  (is (= 16 (offload/concurrency 16)))
  (is (= offload/default-concurrency (offload/concurrency nil)))
  (is (= offload/default-concurrency (offload/concurrency 0)))
  (is (= offload/default-concurrency (offload/concurrency "8"))))
