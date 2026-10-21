(ns hive-mcp.protocols.memory-liveness-test
  "Facets for the IMemoryStoreLiveness re-export. The delegating method
   wrappers are driven against a recording stub that extends the CANONICAL
   ports protocol after this namespace loaded, which is exactly the case a
   value alias would miss."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.protocols.memory-liveness :as live]
            [hive-spi.memory.ports :as ports]
            [hive-test.golden :refer [deftest-golden-fn]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord RecordingStore [calls healthy?]
  ports/IMemoryStoreLiveness
  (-probe! [_] (swap! calls conj [:probe]) healthy?)
  (-kick-reconnect! [_] (swap! calls conj [:kick]) :kicked)
  (-await-reconnect! [_ budget-ms] (swap! calls conj [:await budget-ms]) (<= budget-ms 1000)))

(defn- scenario []
  (let [up   (->RecordingStore (atom []) true)
        down (->RecordingStore (atom []) false)]
    {:liveness-store? {:stub      (live/liveness-store? up)
                       :plain-map (live/liveness-store? {:probe true})
                       :nil       (live/liveness-store? nil)}
     :up   {:probe (live/-probe! up)
            :kick  (live/-kick-reconnect! up)
            :await (live/-await-reconnect! up 500)}
     :down {:probe (live/-probe! down)
            :await (live/-await-reconnect! down 5000)}
     :calls {:up @(:calls up) :down @(:calls down)}}))

(deftest-golden-fn memory-liveness-delegation-golden
  "test/golden/protocols/memory-liveness.edn"
  scenario)

(deftest alias-is-the-canonical-protocol
  (is (identical? ports/IMemoryStoreLiveness live/IMemoryStoreLiveness)))
