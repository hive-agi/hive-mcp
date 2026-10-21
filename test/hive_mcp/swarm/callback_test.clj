(ns hive-mcp.swarm.callback-test
  "Facets for hive-mcp.swarm.callback.

     property  the registration table behaves as a map model under any
               sequence of register! / unregister! / cleanup-for-agent!
     golden    status and registrations after a fixed script, cleanup-stale!
               by age, and the fire path: a :task-completed event published on
               the in-process channel reaches the registered format-fn (an
               injected recording fn); the ling lookup then needs
               hive-datascript, absent on the unit classpath, so the
               registration is KEPT for a retry, which is what the snapshot
               pins

   Every task id here starts with \"f00-\", and the fixture removes those, so
   the global table is left as found."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.channel.core :as channel]
            [hive-mcp.swarm.callback :as callback]
            [hive-test.golden :refer [deftest-golden]]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- mine? [task-id] (str/starts-with? task-id "f00-"))

(defn- clear-mine! []
  (doseq [tid (filter mine? (keys (callback/registrations)))]
    (callback/unregister! tid)))

(use-fixtures :each
  (fn [f]
    (clear-mine!)
    (try (f) (finally (callback/stop-listener!) (clear-mine!)))))

(defn- my-table
  "task-id -> ling-id for this suite's registrations."
  []
  (into (sorted-map)
        (keep (fn [[tid {:keys [ling-id]}]] (when (mine? tid) [tid ling-id])))
        (callback/registrations)))

(def ^:private Op
  [:or
   [:tuple [:= :register] [:enum "f00-t1" "f00-t2" "f00-t3"] [:enum "ling-a" "ling-b"]]
   [:tuple [:= :unregister] [:enum "f00-t1" "f00-t2" "f00-t3"]]
   [:tuple [:= :cleanup-for] [:enum "ling-a" "ling-b"]]])

(defn- step-model [m [op x y]]
  (case op
    :register    (assoc m x y)
    :unregister  (dissoc m x)
    :cleanup-for (into (sorted-map) (remove (fn [[_ l]] (= l x))) m)))

(defn- step! [[op x y]]
  (case op
    :register    (callback/register! x {:ling-id y})
    :unregister  (callback/unregister! x)
    :cleanup-for (callback/cleanup-for-agent! x)))

(defspec registrations-follow-the-map-model 150
  (prop/for-all [ops (mg/generator [:vector {:gen/max 12} Op])]
    (clear-mine!)
    (run! step! ops)
    (let [model (reduce step-model (sorted-map) ops)]
      (and (= model (my-table))
           (every? (fn [t] (= (contains? model t) (callback/registered? t)))
                   ["f00-t1" "f00-t2" "f00-t3"])))))

(defn- scripted-status []
  (callback/register! "f00-a" {:ling-id "ling-a"})
  (callback/register! "f00-b" {:ling-id "ling-b"})
  (callback/register! "f00-c" {:ling-id "ling-a"})
  (let [after-register (my-table)
        _              (callback/cleanup-for-agent! "ling-a")
        after-cleanup  (my-table)
        _              (callback/cleanup-stale! 3600000)
        fresh-kept     (my-table)
        _              (Thread/sleep 5)
        _              (callback/cleanup-stale! 0)
        stale-dropped  (my-table)]
    {:after-register after-register
     :after-cleanup-for-ling-a after-cleanup
     :cleanup-stale-keeps-fresh fresh-kept
     :cleanup-stale-zero-age stale-dropped
     :listener-active? (:listener-active? (callback/status))}))

(deftest-golden registration-lifecycle-golden
  "test/golden/hive-mcp/swarm/callback-lifecycle.edn"
  (scripted-status))

(defn- fire-once
  "Register f00-fire with a recording format-fn, publish EVENT, and answer what
   the format-fn saw and whether the registration survived."
  [event]
  (let [seen (promise)]
    (callback/register! "f00-fire" {:ling-id "f00-no-such-ling"
                                    :format-fn (fn [tid ev]
                                                 (deliver seen [tid (select-keys ev [:status :result])])
                                                 "formatted")})
    (callback/start-listener!)
    (Thread/sleep 50)
    (channel/publish! event)
    (let [got (deref seen 2000 :not-fired)]
      (Thread/sleep 50)
      {:format-fn-saw got
       :still-registered? (callback/registered? "f00-fire")})))

(deftest-golden fire-path-golden
  "test/golden/hive-mcp/swarm/callback-fire.edn"
  {:completed (fire-once {:type :task-completed :task-id "f00-fire" :status "completed" :result "ok"})})

(deftest register-requires-string-ids-test
  ;; refused by the :pre, or by the contract first when it is instrumented
  (is (thrown? Throwable (callback/register! :f00-kw {:ling-id "ling-a"})))
  (is (= "f00-x" (callback/register! "f00-x" {:ling-id "ling-a"}))))
