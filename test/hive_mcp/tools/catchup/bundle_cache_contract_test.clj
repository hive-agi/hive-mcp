(ns hive-mcp.tools.catchup.bundle-cache-contract-test
  "catchup.bundle-cache: the content tier as a trifecta over a pure fetch fn,
   the bundle tier and single-flight driven directly."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.tools.catchup.bundle-cache :as bc]
            [hive-mcp.tools.catchup.private-view :as pv]
            [hive-mcp.tools.catchup.shapes :as shapes]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [t] (bc/reset-cache!) (try (t) (finally (bc/reset-cache!)))))

(defn fetch-known
  "A batch read answering only ids e1..e3, content derived from the id."
  [ids]
  (keep #(when (#{"e1" "e2" "e3"} %) {:id % :content (str "c-" %)}) ids))

(defn cached-entries-fresh
  "Unary adapter: cached-entries over `fetch-known` from an EMPTY cache, so
   every case is independent of the cases before it."
  [ids]
  (bc/reset-cache!)
  (bc/cached-entries ids fetch-known))

(deftrifecta-from-schema cached-entries-resolves-known
  hive-mcp.tools.catchup.bundle-cache-contract-test/cached-entries-fresh
  {:in          [:vector {:gen/max 5} [:maybe [:enum "e1" "e2" "e3" "e9"]]]
   :out         [:map-of [:enum "e1" "e2" "e3"] shapes/Entry]
   :rel         (fn [ids out]
                  (= out (into {} (for [id (distinct (remove nil? ids))
                                        :when (#{"e1" "e2" "e3"} id)]
                                    [id {:id id :content (str "c-" id)}]))))
   :golden-path "test/golden/catchup/bundle-cache-entries.edn"
   :num-tests   200})

(deftest bundle-tier-caches-and-private-bypasses
  (let [n (atom 0) f #(do (swap! n inc) {:axioms [{:id "e1"}]})]
    (is (= {:axioms [{:id "e1"}]} (bc/cached-bundle "p" f)))
    (is (= {:axioms [{:id "e1"}]} (bc/cached-bundle "p" f)))
    (is (= 1 @n))
    (pv/in-view true #(bc/cached-bundle "p" f))
    (is (= 2 @n) "a private view reads around the cache")))

(deftest single-flight-and-housekeeping
  (is (= :v (bc/single-flight ::k (constantly :v))))
  (is (nil? (bc/invalidate! {:op :added :memory-type "decision"})))
  (is (nat-int? (bc/evict-stale!)))
  (is (int? (bc/now-ms)))
  (is (nil? (bc/ensure-subscribed!)))
  (is (true? (bc/subscribed?))))
