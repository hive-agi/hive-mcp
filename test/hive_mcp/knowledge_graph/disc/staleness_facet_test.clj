(ns hive-mcp.knowledge-graph.disc.staleness-facet-test
  "Goldens over staleness surfacing: a fresh disc, a stale one (hash
   mismatch) and a missing path, against a fresh DataScript store and probe
   files the test owns."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [hive-mcp.knowledge-graph.disc.crud :as crud]
            [hive-mcp.knowledge-graph.disc.staleness :as st]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- probe-path [n]
  (.getPath (io/file (System/getProperty "java.io.tmpdir") (str "m14-staleness-" n ".txt"))))

(defn- with-world
  "Run F against a fresh store holding a FRESH disc (read now, hash matching)
   and a STALE one (hash mismatch, never read). Probe files are removed after."
  [f]
  (let [fresh (probe-path "fresh") stale (probe-path "stale")]
    (try
      (spit fresh "fresh content")
      (spit stale "stale content")
      (fixtures/datascript-fixture
       (fn []
         (crud/touch-disc! fresh)
         (crud/add-disc! {:path stale :content-hash "not-the-hash"})
         (f fresh stale)))
      (finally
        (io/delete-file fresh true)
        (io/delete-file stale true)))))

(defn- shown [m] (into (sorted-map) (map (fn [[k v]] [k (if (string? v) (.replace ^String v (System/getProperty "java.io.tmpdir") "<tmp>") v)])) m))

(defn- surface []
  (with-world
   (fn [fresh stale]
     (let [missing "/m14/staleness/missing.clj"
           ctx (st/kg-first-context [fresh stale missing nil "" stale])
           tmp (System/getProperty "java.io.tmpdir")
           strip #(.replace ^String % tmp "<tmp>")]
       {:score-fresh (st/staleness-score (crud/get-disc fresh))
        :score-stale (st/staleness-score (crud/get-disc stale))
        :report-stale (st/staleness-report (crud/get-disc stale))
        :score-nil (st/staleness-score nil)
        :warnings (mapv shown (st/staleness-warnings [fresh stale missing]))
        :warnings-set (count (st/staleness-warnings #{fresh stale}))
        :warnings-empty (st/staleness-warnings [])
        :top (mapv shown (st/top-stale-files))
        :top-map (mapv shown (st/top-stale-files {:n 1 :threshold 0.0}))
        :top-high (st/top-stale-files :threshold 0.95)
        :ctx-summary (:summary ctx)
        :ctx-known (mapv strip (keys (:kg-known ctx)))
        :ctx-known-status (mapv #(dissoc % :disc :last-read-at) (vals (:kg-known ctx)))
        :ctx-needs-read (:needs-read ctx)
        :ctx-stale (mapv strip (:stale ctx))
        :ctx-strict (:summary (st/kg-first-context [fresh stale] {:staleness-threshold 0.0}))}))))

(golden/deftest-golden staleness-surface-golden
  "test/golden/hive-mcp/m14-disc-staleness.edn"
  (surface))

(deftest kg-first-context-output-holds-its-contract
  (with-world
   (fn [fresh stale]
     (let [out-schema (-> (m/function-schemas)
                          (get-in ['hive-mcp.knowledge-graph.disc.staleness 'kg-first-context :schema])
                          m/children second)]
       (is (m/validate out-schema (st/kg-first-context [fresh stale "/m14/none"])))))))
