(ns hive-mcp.telemetry.metrics-test
  "Metrics bridge, middleware op keys and `project metrics` verbs, observed
   through a recording stub recorder."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.server.routes.middleware :as mw]
            [hive-mcp.telemetry.metrics :as metrics]
            [hive-mcp.telemetry.metrics-tool :as mtool]
            [hive-mcp.telemetry.prometheus :as prom]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private seen (atom []))

(defn- recording [k ns outcome] (swap! seen conj [k ns outcome]))

(use-fixtures :each
  (fn [t]
    (reset! seen [])
    (let [old (metrics/install-recorder! recording)]
      (try (t) (finally (metrics/install-recorder! old))))))

(defn- keys-seen [] (set (map first @seen)))

(deftest timed-records-ok-and-error
  (is (= 3 (metrics/timed [:t :op] (+ 1 2))))
  (is (thrown? clojure.lang.ExceptionInfo
               (metrics/timed [:t :boom] (throw (ex-info "x" {})))))
  (is (= [[[:t :op] :ok] [[:t :boom] :error]]
         (mapv (fn [[k _ o]] [k o]) @seen)))
  (is (every? (fn [[_ ns _]] (and (int? ns) (>= ns 0))) @seen)))

(deftest a-throwing-recorder-never-fails-the-call
  (metrics/install-recorder! (fn [_ _ _] (throw (RuntimeException. "sink down"))))
  (let [before (metrics/recorder-error-count)]
    (is (= :v (metrics/timed [:t :op] :v)))
    (is (= (inc before) (metrics/recorder-error-count)))))

(deftest command-verb-is-bounded
  (is (= "search" (mw/command-verb {:command "search foo bar"})))
  (is (= "add" (mw/command-verb {"command" :add})))
  (is (= "-" (mw/command-verb {})))
  (is (= "-" (mw/command-verb {:command "   "})))
  (is (= "-" (mw/command-verb {:command (apply str (repeat 80 "x"))}))))

(deftest chain-records-tool-and-layers
  (let [h (mw/build-middleware-chain (fn [_] [{:type "text" :text "ok"}]) "memory" nil)
        out (h {"command" "search q" "_caller_id" "metrics-test"})]
    (is (map? out))
    (testing "outermost key is [:mcp/tool tool verb], recorded last"
      (is (= [[:mcp/tool "memory" "search"] :ok]
             (let [[k _ o] (last @seen)] [k o]))))
    (testing "real-work layers are timed"
      (is (every? (keys-seen)
                  [[:mcp/mw :context] [:mcp/mw :grant]
                   [:mcp/mw :piggyback :async] [:mcp/mw :piggyback :hivemind]
                   [:mcp/mw :piggyback :descendants]])))))

(deftest chain-records-error-outcome
  (let [h (mw/build-middleware-chain (fn [_] (throw (Error. "die"))) "kanban" nil)]
    (is (thrown? Throwable (h {"command" "list"})))
    (is (some (fn [[k _ o]] (and (= k [:mcp/tool "kanban" "list"]) (= o :error))) @seen))))

(deftest op-labels-are-closed
  (is (= {:area "mcp/tool" :op "memory" :sub "search" :outcome "ok"}
         (prom/op-labels [:mcp/tool "memory" "search"] :ok)))
  (is (= {:area "mcp/mw" :op "guard" :sub "-" :outcome "error"}
         (prom/op-labels [:mcp/mw :guard] :error))))

(deftest hotspots-opts-from-params
  (is (= {:by :total :limit 20} (mtool/hotspots-opts {})))
  (is (= {:by :p99 :limit 5 :area "mcp/mw"}
         (mtool/hotspots-opts {"by" "p99" "limit" "5" "area" "mcp/mw"})))
  (is (= :total (:by (mtool/hotspots-opts {:by "nonsense"})))))

(deftest verbs-answer-readable-edn
  (doseq [f [mtool/handle-snapshot mtool/handle-hotspots mtool/handle-reset]]
    (let [{:keys [type text]} (f {})]
      (is (= "text" type))
      (is (map? (edn/read-string text))))))
