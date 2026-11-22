(ns hive-mcp.channel.memory-piggyback-stages-test
  "Characterization of `drain!` across every path its CPPB stages split:
   FIFO, ranked (ctx :tokens), strict char budget, context-refs priority and
   the exhaustion tombstone. The golden pins both the returned batches and the
   buffer written back after each drain, so a stage split that changes either
   fails here."
  (:require [clojure.test :refer [use-fixtures]]
            [hive-mcp.channel.memory-piggyback :as mp]
            [hive-dsl.bounded-atom :refer [bget]]
            [hive-dsl.context.identity :as ctx-id]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f]
    (mp/reset-all!)
    (try (f) (finally (mp/reset-all!)))))

(def ^:private entries
  [{:id "ax-1" :type "axiom" :content "Never delete data" :severity "INVIOLABLE" :tags ["axiom"]}
   {:id "c-1" :type "convention" :content "Use carto for every read" :tags ["carto"]}
   {:id "n-1" :type "note" :content (apply str (repeat 120 "n")) :tags ["memory"]}
   {:id "d-1" :type "decision" :content "Prefer Result over catch" :tags ["carto" "dsl"]
    :access-count 3 :helpful-count 2 :unhelpful-count 0}])

(defn- buffer-of [caller]
  (bget mp/buffers (ctx-id/caller-id-key (ctx-id/parse-caller-id caller))))

(defn- drain-all
  "Drain `caller` with `ctx` until nil, recording each result and the buffer
   written back after it. Bounded so a regression cannot loop forever."
  [caller ctx]
  (loop [acc [] n 0]
    (let [d (when (< n 12) (mp/drain! caller ctx))]
      (if d
        (recur (conj acc {:result (update d :batch #(mapv :id %))
                          :buffer (-> (buffer-of caller)
                                      (update :entries #(mapv :id %)))})
               (inc n))
        acc))))

(defn- scenario [caller entries refs ctx]
  (mp/reset-all!)
  (mp/enqueue! caller entries refs)
  (drain-all caller ctx))

(deftest-golden drain-stage-paths
  "test/golden/channel/memory-piggyback-stages.edn"
  {:fifo          (scenario "stg-fifo" entries nil nil)
   :fifo-refs     (scenario "stg-refs" entries {:axioms "ctx-1" :decisions "ctx-2"} nil)
   :strict-small  (scenario "stg-strict" entries {:axioms "ctx-1"} {:char-budget 400})
   :strict-tiny   (scenario "stg-tiny" entries {:axioms "ctx-1"} {:char-budget 60})
   :ranked        (scenario "stg-ranked" entries nil {:tokens #{"carto"}})
   :ranked-strict (scenario "stg-ranked-strict" entries {:axioms "ctx-9"}
                            {:tokens #{"dsl"} :char-budget 450})
   :zero-budget   (scenario "stg-zero" entries nil {:char-budget 0})
   :refs-only     (scenario "stg-refs-only" [] {:axioms "ctx-only"} nil)
   :empty         (scenario "stg-empty" [] nil nil)})
