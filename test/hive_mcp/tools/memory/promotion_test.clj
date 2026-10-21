(ns hive-mcp.tools.memory.promotion-test
  "Facets for cross-pollination promotion.

   walk-tiers and build-xpoll-plan are pure: trifectas from their contracts
   (Entry and DurationName generators in hive-mcp.tools.memory.schema). The
   side-effecting promotion, the hook cycle and the MCP handler get a golden
   with the memory port satisfied by the stub store (no with-redefs)."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.protocols.memory :as mp]
            [hive-mcp.test.stub.memory-store :as stub]
            [hive-mcp.tools.memory.duration :as dur]
            [hive-mcp.tools.memory.promotion :as promo]
            [hive-mcp.tools.memory.schema :as ms]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- idx [d] (.indexOf ^java.util.List dur/duration-order d))

(deftrifecta-from-schema walk-tiers-climbs-and-stops-at-permanent
  hive-mcp.tools.memory.promotion/walk-tiers
  {:in          [:cat ms/DurationName [:int {:min 0 :max 6}]]
   :out         ms/TierWalk
   :rel         (fn [[d n] out]
                  (let [steps (min n (- 4 (idx d)))]
                    (and (= steps (:tiers-promoted out))
                         (= (nth dur/duration-order (+ (idx d) steps)) (:final-duration out)))))
   :num-tests   300
   :n-cases     12
   :golden-path "test/golden/tools/memory/promotion-walk-tiers.edn"})

(deftrifecta-from-schema build-xpoll-plan-reads-the-xpoll-tags
  hive-mcp.tools.memory.promotion/build-xpoll-plan
  {:in        ms/Entry
   :out       ms/XpollPlan
   :rel       (fn [in out]
                (and (= (:id in) (:id out))
                     (= (count (:cross_projects out)) (:cross_project_count out))
                     (= (:next_duration out)
                        (:final-duration (promo/walk-tiers (str (:duration in)) (:tiers_to_promote out))))))
   :num-tests 200})

(def ^:private entries
  [{:id "e1" :type "decision" :duration "short" :content "x"
    :tags ["xpoll:project:a" "xpoll:project:b" "xpoll:project:c"]}
   {:id "e2" :type "note" :duration "medium" :content "y" :tags ["a"]}
   {:id "e3" :type "convention" :duration "permanent" :content "z"
    :tags ["xpoll:project:a" "xpoll:project:b"]}])

(deftest-golden promotion-over-a-stub-store
  "test/golden/tools/memory/promotion-stub-store.edn"
  (stub/with-stub-store
    (fn []
      (stub/seed! (mp/get-store) entries)
      {:edge-walks     {:nil-start   (promo/walk-tiers nil 1)
                        :blank-start (promo/walk-tiers "" 2)}
       :plans          (mapv promo/build-xpoll-plan entries)
       :promote-two    (promo/promote-entry-by-tiers! (first entries) 2)
       :promote-capped (promo/promote-entry-by-tiers! (nth entries 1) 9)
       :promote-none   (promo/promote-entry-by-tiers! (nth entries 2) 3)
       :stored         (sort-by :id (map #(select-keys % [:id :duration])
                                         (vals (stub/entries (mp/get-store)))))
       :cycle          (promo/run-xpoll-cycle! {:limit 10})
       :handler-dry    (promo/handle-xpoll-promote {:dry_run true :limit 10})})))

(deftest-golden promotion-without-a-store
  "test/golden/tools/memory/promotion-no-store.edn"
  (stub/with-no-store
    (fn [] {:cycle (promo/run-xpoll-cycle! {})})))

(deftest walk-tiers-docstring-examples
  (is (= {:final-duration "long" :tiers-promoted 2} (promo/walk-tiers "short" 2)))
  (is (= {:final-duration "permanent" :tiers-promoted 0} (promo/walk-tiers "permanent" 3)))
  (is (= {:final-duration "permanent" :tiers-promoted 1} (promo/walk-tiers "long" 5))))
