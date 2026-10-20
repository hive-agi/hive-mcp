(ns hive-mcp.tools.catchup.scope-filter-contract-test
  "Schema-derived trifectas for catchup.scope-filter (pure helpers)."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.catchup.scope-filter :as sf]
            [hive-mcp.tools.catchup.shapes :as shapes]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Row
  "An entry drawn from a tiny id/tag/project vocabulary so collisions and
   every scope branch occur."
  [:map
   [:id [:enum "e1" "e2" "e3"]]
   [:tags [:vector {:gen/max 3} [:enum "scope:project:p" "scope:project:other" "catchup-priority" "x"]]]
   [:project-id [:enum "p" "other" "global"]]
   [:created [:enum "2026-01-01T00:00:00Z" "2026-02-01T00:00:00Z" "2026-03-01T00:00:00Z"]]])

(def ^:private Rows [:vector {:gen/max 6} Row])

(deftrifecta-from-schema distinct-by-keeps-first
  hive-mcp.tools.catchup.scope-filter/distinct-by
  {:in          [:cat [:enum :id :project-id] Rows]
   :out         [:vector Row]
   :rel         (fn [[k rows] out]
                  (and (= (map k out) (distinct (map k rows)))
                       (every? (fn [o] (= o (first (filter #(= (k %) (k o)) rows)))) out)))
   :golden-path "test/golden/catchup/scope-filter-distinct-by.edn"
   :num-tests   200})

(deftrifecta-from-schema newest-first-orders
  hive-mcp.tools.catchup.scope-filter/newest-first
  {:in  [:cat Rows]
   :out [:sequential Row]
   :rel (fn [[rows] out]
          (and (= (frequencies rows) (frequencies out))
               (every? (fn [[a b]] (>= (compare (:created a) (:created b)) 0)) (partition 2 1 out))))
   :num-tests 200})

(deftrifecta-from-schema filter-by-tags-all-required
  hive-mcp.tools.catchup.scope-filter/filter-by-tags
  {:in  [:cat Rows [:maybe [:vector {:gen/max 2} [:enum "scope:project:p" "catchup-priority" "x"]]]]
   :out [:sequential Row]
   :rel (fn [[rows tags] out]
          (= out (if (seq tags)
                   (filter #(set/subset? (set tags) (set (:tags %))) rows)
                   rows)))
   :num-tests 200})

(deftrifecta-from-schema scope-filter-entries-authority
  hive-mcp.tools.catchup.scope-filter/scope-filter-entries
  {:in              [:cat Rows [:set [:enum "scope:project:p"]] [:set [:enum "p" "global"]]]
   :out             [:sequential Row]
   :rel             (fn [[rows st vis] out]
                      (= out (filter (fn [e]
                                       (let [scoped (filter #(.startsWith ^String % "scope:project:") (:tags e))]
                                         (if (seq scoped)
                                           (boolean (some st (:tags e)))
                                           (contains? vis (:project-id e)))))
                                     rows)))
   :golden-path     "test/golden/catchup/scope-filter-entries.edn"
   :num-tests       200})

(deftrifecta-from-schema scope-filter-entries-strict-no-fallback
  hive-mcp.tools.catchup.scope-filter/scope-filter-entries-strict
  {:in  [:cat Rows [:set [:enum "scope:project:p" "scope:project:other"]]]
   :out [:sequential Row]
   :rel (fn [[rows st] out] (= out (filter #(some st (:tags %)) rows)))
   :num-tests 200})

(def ^:private PierceRow
  [:map
   [:id [:enum "e1" "e2" "e3"]]
   [:type [:enum :axiom "axiom" "decision" :convention "note"]]
   [:tags [:vector {:gen/max 2} [:enum "catchup-priority" "session-summary" "scope:global" "x"]]]])

(deftrifecta-from-schema scope-pierce-entries-only-piercers
  hive-mcp.tools.catchup.scope-filter/scope-pierce-entries
  {:in  [:cat [:vector {:gen/max 5} PierceRow] [:maybe [:enum "p" "global"]]]
   :out [:maybe [:sequential PierceRow]]
   :rel (fn [[rows pid] out]
          (if (or (nil? pid) (= "global" pid))
            (nil? out)
            (every? #(or (= "axiom" (name (:type %)))
                         (some #{"catchup-priority" "session-summary"} (:tags %)))
                    out)))
   :num-tests 200})

(deftest compute-full-scope-tags-drops-global-in-project
  (is (not (contains? (sf/compute-full-scope-tags "p") "scope:global")))
  (is (contains? (sf/compute-full-scope-tags "p") "scope:project:p")))

(deftest entry-expiring-soon-boundaries
  (is (nil? (sf/entry-expiring-soon? {:id "e1"})) "no :expires, no answer")
  (is (true? (sf/entry-expiring-soon? {:expires (str (.plusDays (java.time.ZonedDateTime/now) 1))})))
  (is (false? (sf/entry-expiring-soon? {:expires (str (.plusDays (java.time.ZonedDateTime/now) 30))}))))

(deftest shared-entry-schema-generates
  (is (every? map? (repeatedly 5 #(malli.generator/generate shapes/Entry)))))
