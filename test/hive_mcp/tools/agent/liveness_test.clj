(ns hive-mcp.tools.agent.liveness-test
  "A stalled headless ling must show a growing :liveness-age-ms in the
   `agent status` projection instead of reading as plain \"working\"."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.agent.helpers :as helpers]
            [hive-mcp.tools.agent.status :as status]
            [hive-spi.addon.headless :as headless]))

(defn- stub-backend
  "IHeadlessBackend whose status for any ling is `(status-of now-fn)`; the
   clock is a port so a test can let time pass without sleeping."
  [last-progress-at now-fn]
  (reify headless/IHeadlessBackend
    (headless-id [_] :stub)
    (headless-spawn! [_ _ _] nil)
    (headless-dispatch! [_ _ _] true)
    (headless-status [_ ctx _]
      {:slave/id (:id ctx) :slave/status :running
       :liveness-age-ms (max 0 (- (now-fn) last-progress-at))})
    (headless-kill! [_ _] {:killed? true})
    (headless-interrupt! [_ _] {:success? false :reason :not-supported})))

(def ^:private working-row
  {:slave/id "ling-1" :slave/status :working :slave/depth 1
   :ling/spawn-mode :stub})

(defn- status-rows
  "The `agent status` projection of `rows` over a registry of `backends`."
  [backends rows]
  (:agents (helpers/format-agents
            (status/annotate-liveness (status/backend-status-fn backends) rows))))

(deftest stalled-ling-shows-a-growing-age
  (let [clock (atom 1000)
        backend (stub-backend 1000 #(deref clock))
        age #(:liveness-age-ms (first (status-rows {:stub backend} [working-row])))]
    (is (= 0 (age)))
    (reset! clock 91000)
    (is (= 90000 (age)))
    (reset! clock 181000)
    (testing "same status :working, older age"
      (is (= :working (:status (first (status-rows {:stub backend} [working-row])))))
      (is (= 180000 (age))))))

(deftest rows-without-a-backend-are-untouched
  (let [claude-row {:slave/id "c" :slave/status :idle :slave/depth 1 :ling/spawn-mode :claude}
        plain-row  {:slave/id "p" :slave/status :idle :slave/depth 1}]
    (is (= [claude-row plain-row]
           (status/annotate-liveness (status/backend-status-fn {}) [claude-row plain-row])))
    (is (every? #(not (contains? % :liveness-age-ms))
                (status-rows {} [claude-row plain-row])))))

(deftest bad-ages-and-throwing-backends-never-break-status
  (is (= [working-row] (status/annotate-liveness (constantly {:liveness-age-ms nil}) [working-row])))
  (is (= [working-row] (status/annotate-liveness (constantly {:liveness-age-ms -5}) [working-row])))
  (is (= [working-row] (status/annotate-liveness (constantly nil) [working-row])))
  (is (= [working-row] (status/annotate-liveness (fn [_] (throw (ex-info "boom" {}))) [working-row]))))

(deftest annotate-keeps-order-and-annotates-each-row
  (let [rows [(assoc working-row :slave/id "a") (assoc working-row :slave/id "b")]
        out  (status/annotate-liveness (fn [r] {:liveness-age-ms (if (= "a" (:slave/id r)) 1 2)}) rows)]
    (is (= [["a" 1] ["b" 2]] (mapv (juxt :slave/id :slave/liveness-age-ms) out)))))
