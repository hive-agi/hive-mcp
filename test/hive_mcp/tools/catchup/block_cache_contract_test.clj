(ns hive-mcp.tools.catchup.block-cache-contract-test
  "catchup.block-cache: policy and key as trifectas; the cache, status
   providers, shared refs and invalidation driven store-free (no store set,
   so the weak store reference is nil on both sides)."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.tools.catchup.block-cache :as blc]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [t] (blc/reset-cache!) (try (t) (finally (blc/reset-cache!)))))

(def ^:private BlockCacheDecl
  [:map [:block/id [:enum :kanban :x/y]]
   [:block/cache {:optional true} [:or :boolean [:map [:fresh-ms [:enum 10 1000]]] :string]]])

(deftrifecta-from-schema block-policy-opt-in
  hive-mcp.tools.catchup.block-cache/block-policy
  {:in              [:cat BlockCacheDecl]
   :out             [:maybe blc/Policy]
   :rel             (fn [[b] out]
                      (let [c (:block/cache b)]
                        (cond (true? c) (= out blc/default-block-policy)
                              (map? c)  (= out (merge blc/default-block-policy c))
                              :else     (nil? out))))
   :classify        (fn [_ out] (if out :cached :uncached))
   :classify-domain #{:cached :uncached}
   :golden-path     "test/golden/catchup/block-policy.edn"
   :num-tests       200})

(deftrifecta-from-schema block-key-shape
  hive-mcp.tools.catchup.block-cache/block-key
  {:in  [:cat BlockCacheDecl
         [:maybe [:map [:key-by {:optional true} [:vector {:gen/max 3} [:enum :project-id :directory :caller-id]]]]]
         [:map [:project-id [:enum "p" "q"]] [:directory [:enum "/d"]] [:caller-id [:enum "c"]]]]
   :out [:vector :any]
   :rel (fn [[b policy ctx] out]
          (= out (into [:block (:block/id b)]
                       (map #(get ctx %))
                       (or (:key-by policy) [:project-id :directory]))))
   :num-tests 200})

(def ^:private StatsOut
  (-> (m/function-schemas) (get-in ['hive-mcp.tools.catchup.block-cache 'stats :schema]) m/schema m/children second))

(deftest cached-fresh-then-miss
  (let [n (atom 0) f #(swap! n inc) pol {:fresh-ms 60000 :max-age-ms 120000}]
    (is (= 1 (blc/cached [:k] pol f)))
    (is (= 1 (blc/cached [:k] pol f)) "fresh hit, no recompute")
    (is (= {:hit 1 :miss 1} (select-keys (blc/stats) [:hit :miss])))
    (is (m/validate StatsOut (blc/stats)))))

(deftest run-block-opt-in-and-out
  (let [n (atom 0)
        uncached {:block/id :b1 :block/fn (fn [_] (swap! n inc))}
        cached   (assoc uncached :block/id :b2 :block/cache true)]
    (blc/run-block uncached {:project-id "p"})
    (blc/run-block uncached {:project-id "p"})
    (is (= 2 @n) "an uncached block runs every time")
    (blc/run-block cached {:project-id "p"})
    (blc/run-block cached {:project-id "p"})
    (is (= 3 @n) "a cached block runs once")))

(deftest status-provider-and-invalidation
  (let [n (atom 0)]
    (is (= {:v 1} (blc/run-status-provider :carto (fn [_] {:v (swap! n inc)}) "p")))
    (is (= {:v 1} (blc/run-status-provider :carto (fn [_] {:v (swap! n inc)}) "p"))))
  (blc/cached [:t] {:fresh-ms 60000 :max-age-ms 60000 :drop-tags #{"kanban"}} (constantly :x))
  (is (nil? (blc/invalidate! {:op :added :tags ["decision"]})))
  (is (= 2 (:entries (blc/stats))) "an unrelated tag drops nothing")
  (blc/invalidate! {:op :added :tags ["kanban"]})
  (is (= 1 (:entries (blc/stats))))
  (is (= 1 (:dropped (blc/stats))))
  (is (= 0 (blc/evict-stale!)))
  (is (nil? (blc/ensure-subscribed!)))
  (is (int? (blc/now-ms))))

(deftest shared-context-refs-reuse-requires-refs
  (let [writes (atom 0)
        put    (fn [] (swap! writes inc) {})]
    (is (= {} (blc/shared-context-refs "p" {:axioms []} put)))
    (is (= {} (blc/shared-context-refs "p" {:axioms []} put)))
    (is (= 2 @writes) "empty refs are never shared")
    (is (nil? (blc/shared-context-refs "p" nil (constantly nil))))))
