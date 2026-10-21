(ns hive-mcp.recall.canary.live-test
  "Golden over the runtime arm of the recall canary. The IMemoryStore port
   is satisfied by a recording stub registered through the store registry
   (no with-redefs): it finds no fixtures, so ensure-fixtures! writes all
   three, and a second stub that already holds them is only read. Probes run
   on the unit classpath, where the embedding service, semantic search and
   hive-carto are absent: those arms are skips or faults, never passes, which
   is the contract the golden pins."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.recall.canary :as canary]
            [hive-mcp.recall.canary.live :as live]
            [hive-test.golden :refer [deftest-golden-fn]]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- empty-store
  "Holds nothing; answers an id per write and records every call."
  [calls]
  (reify mem-proto/IMemoryStore
    (query-entries [_ q] (swap! calls conj [:query (vec (:tags q))]) [])
    (add-entry! [_ e]
      (swap! calls conj [:add (:type e) (:duration e) (:project-id e)])
      (str "fx-" (count (filter #(= :add (first %)) @calls))))))

(defn- seeded-store
  "Already holds every fixture: each tag query answers that role's entry."
  [calls]
  (reify mem-proto/IMemoryStore
    (query-entries [_ q]
      (swap! calls conj [:query (vec (:tags q))])
      (let [role (some (fn [{:keys [role]}] (when (= (vec (:tags q)) (canary/fixture-tags role)) role))
                       canary/fixtures)]
        (if role [{:id (str "seeded-" (name role))}] [])))
    (add-entry! [_ _] (swap! calls conj [:add]) "never")))

(defn- with-store [store f]
  (let [before (mem-proto/registered-stores)]
    (mem-proto/reset-registry!)
    (try
      (when store (mem-proto/register-store! :default store))
      (f)
      (finally
        (mem-proto/reset-registry!)
        (doseq [[k s] before] (mem-proto/register-store! k s))))))

(defn- verdict-view [v]
  (-> v
      (select-keys [:ok? :ran :passed :faults :skipped])
      (update :faults #(mapv :fault %))
      (update :skipped #(vec (sort-by :label %)))))

(defn- steady-run
  "run! folds in the previous verdict (skip regressions), which is process
   state another test may have left behind. Prime it with the same opts so
   the recorded verdict depends only on this run."
  [opts]
  (with-store nil #(live/run! opts))
  (verdict-view (with-store nil #(live/run! opts))))

(defn- scenario []
  (let [fresh-calls  (atom [])
        seeded-calls (atom [])
        fresh        (empty-store fresh-calls)
        seeded       (seeded-store seeded-calls)
        created      (live/ensure-fixtures! fresh)
        found        (live/ensure-fixtures! seeded)]
    {:ensure-created  created
     :ensure-found    found
     :fresh-calls     @fresh-calls
     :seeded-writes   (count (filter #(= :add (first %)) @seeded-calls))
     :supersession    {:no-ids     (live/probe-supersession nil)
                       :not-linked (live/probe-supersession {:superseded "a" :current "b" :linked? false})}
     :anchor-absent   (live/probe-anchor fresh {})
     :carto           {:tag      (live/probe-carto-tag "hive-mcp")
                       :semantic (live/probe-carto-semantic "hive-mcp")}
     :dimension       (:status (live/probe-dimension))
     :run-no-store    (steady-run {:carto? false})
     :run-carto       (steady-run {:scope "x"})}))

(deftest-golden-fn canary-live-golden
  "test/golden/recall/canary-live.edn"
  scenario)

(deftest run-never-throws-and-validates
  (let [v (with-store nil #(live/run!))]
    (is (m/validate live/RunVerdict v))
    (is (false? (:ok? v)) "a skipped or faulted probe is never a pass")))
