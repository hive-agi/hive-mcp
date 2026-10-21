(ns hive-mcp.extensions.departures-test
  "Facets for the departures ledger (process state, so no trifecta: the
   contracted vars are impure). A property replays generated histories
   through record! and checks departure against an independent oracle, and
   a golden pins record-owned! against a contribution map."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.extensions.departures :as dep]
            [hive-test.golden :refer [deftest-golden-fn]]
            [malli.generator :as mg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (dep/clear!) (try (f) (finally (dep/clear!)))))

(def ^:private Tool [:enum "agent" "memory"])

(def ^:private Cmd [:enum "spawn" "Spawn" :spawn "kill" :status])

(def ^:private History
  "[[tool addon commands at-ms] ...] in order."
  [:vector {:max 5} [:tuple Tool [:enum :a :b] [:vector {:max 3} Cmd] [:int {:min 0 :max 9}]]])

(defn- oracle
  "The docstring, restated: the LAST record naming COMMAND (case-folded,
   keyword or string) in TOOL wins."
  [history tool command]
  (let [k (str/lower-case (name command))]
    (some (fn [[t a cs at]]
            (when (and (= t tool) (some #(= k (str/lower-case (name %))) cs))
              {:addon a :at-ms at}))
          (reverse history))))

(defspec departure-is-the-last-recorded 300
  (prop/for-all [history (mg/generator History)
                 tool    (mg/generator Tool)
                 command (mg/generator Cmd)]
    (dep/clear!)
    (let [rets (mapv (fn [[t a cs at]] (dep/record! t a cs at)) history)]
      (and (every? nil? rets)
           (= (oracle history tool command) (dep/departure tool command))))))

(defn- record-owned-scenario []
  (dep/clear!)
  (let [contribs {"agent"  {"spawn" {:addon :hive-agent} "kill" {:addon :other}}
                  "memory" {"index" {:addon :hive-agent}}
                  "kg"     {"walk" {:addon :other}}
                  "empty"  nil}
        tools    (dep/record-owned! contribs :hive-agent)
        stamp    #(some-> % (update :at-ms int?))]
    {:tools        (vec (sort tools))
     :spawn        (stamp (dep/departure "agent" "SPAWN"))
     :kill         (dep/departure "agent" "kill")
     :index        (stamp (dep/departure "memory" :index))
     :other-addon  (dep/departure "kg" "walk")
     :none-owned   (dep/record-owned! contribs :nobody)
     :empty-record (dep/record! "agent" :x [] 1)
     :replayed     (do (dep/record! "agent" :a ["Kill"] 4)
                       (dep/record! "agent" :b [:kill] 7)
                       (dep/departure "agent" "kill"))}))

(deftest-golden-fn record-owned-golden
  "test/golden/extensions/departures-record-owned.edn"
  record-owned-scenario)

(deftest clear-forgets-everything
  (dep/record! "agent" :a ["spawn"] 3)
  (is (= {:addon :a :at-ms 3} (dep/departure "agent" :spawn)))
  (is (nil? (dep/clear!)))
  (is (nil? (dep/departure "agent" "spawn"))))
