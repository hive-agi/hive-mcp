(ns hive-mcp.channel.context-store-golden-test
  "Golden over one scripted session of the ephemeral context store: puts
   (single and batch, live and already-expired), gets with access counting,
   tag queries, evictions, the reaper sweep and stats. Ids and wall-clock
   fields are projected away so the snapshot is deterministic."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.channel.context-store :as cs]
            [hive-test.golden :refer [deftest-golden-fn]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (cs/reset-all!) (try (f) (finally (cs/reset-all!)))))

(defn- view [entry]
  (some-> entry (select-keys [:data :tags :ttl-ms :access-count]) (assoc :accessed? (some? (:last-accessed entry)))))

(defn- datas
  "Query result data, sorted: two puts in the same millisecond tie on the
   newest-first order, so the snapshot does not pin their relative order."
  [entries]
  (sort-by pr-str (map :data entries)))

(deftest-golden-fn context-store-session-golden
  "test/golden/channel/context-store-session.edn"
  (fn []
    (cs/reset-all!)
    (let [a     (cs/context-put! {:kind "axioms"} :tags #{"catchup" "axioms"})
          b     (cs/context-put! [1 2 3] :tags ["catchup"] :ttl-ms 600000)
          dead  (cs/context-put! "stale" :tags #{"catchup" "stale"} :ttl-ms -1)
          batch (cs/context-put-batch! {:decisions {:data ["d1"] :tags #{"decisions"}}
                                        :empty     {:data []}
                                        :none      {:data nil}})]
      ;; steps run in order; a map literal would evaluate them in hash order
      [[:ids-valid?    (every? #(m/validate cs/CtxId %) (concat [a b dead] (vals batch)))]
       [:batch-keys    (set (keys batch))]
       [:first-get     (view (cs/context-get a))]
       [:second-get    (view (cs/context-get a))]
       [:expired-get   (cs/context-get dead)]
       [:missing-get   (cs/context-get "ctx-0-00000000")]
       [:query-catchup (datas (cs/context-query :tags #{"catchup"}))]
       [:query-limit   (count (cs/context-query :limit 1))]
       [:query-all     (datas (cs/context-query))]
       [:stats         (dissoc (cs/context-stats) :oldest :newest)]
       [:evict-tag     (cs/evict-by-tags! #{"decisions" "nope"})]
       [:evict-b       (cs/context-evict! b)]
       [:evict-b-again (cs/context-evict! b)]
       [:put-dead      (string? (cs/context-put! "stale-2" :ttl-ms -1))]
       [:reaped        (cs/reap-expired!)]
       [:after         (datas (cs/context-query))]
       [:final-stats   (dissoc (cs/context-stats) :oldest :newest)]])))

(deftest reaper-lifecycle-is-idempotent-test
  (is (nil? (cs/start-reaper!)))
  (is (nil? (cs/start-reaper!)))
  (is (some? @cs/reaper-executor))
  (is (nil? (cs/stop-reaper!)))
  (is (nil? (cs/stop-reaper!)))
  (is (nil? @cs/reaper-executor)))
