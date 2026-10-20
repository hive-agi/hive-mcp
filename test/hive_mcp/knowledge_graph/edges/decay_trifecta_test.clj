(ns hive-mcp.knowledge-graph.edges.decay-trifecta-test
  "Trifectas for the pure decay predicates, and a golden over the decay
   cycle against a fresh store seeded with stale and fresh edges."
  (:require [hive-mcp.knowledge-graph.disc.schema :as ds]
            [hive-mcp.knowledge-graph.edges.decay :as decay]
            [hive-mcp.knowledge-graph.edges.m14-support :as sup]
            [hive-mcp.knowledge-graph.edges.queries :as q]
            [hive-mcp.knowledge-graph.edges.write :as write]
            [hive-schemas.test :as hst]
            [hive-test.golden :as golden]
            [malli.util :as mu]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private d 'hive-mcp.knowledge-graph.edges.decay)
(def ^:private day-ms (* 24 60 60 1000))

;; last-verified is a Date, a non-Date (read as epoch 0) or absent
(def ^:private EdgeProbe
  [:map
   [:kg-edge/last-verified {:optional true} [:or ds/Date [:enum "not-a-date" 42]]]
   [:kg-edge/relation {:optional true}
    [:enum :implements :supersedes :refines :contradicts :depends-on :derived-from
     :applies-to :co-accessed :relates :projects-to :contains :unknown]]])

(defn- lv-ms [edge]
  (let [lv (:kg-edge/last-verified edge)] (if (instance? java.util.Date lv) (.getTime ^java.util.Date lv) 0)))

(hst/deftrifecta-from-schema last-verified-millis-contract
  hive-mcp.knowledge-graph.edges.decay/last-verified-millis
  {:in [:cat [:maybe EdgeProbe]]
   :out (sup/out-schema d 'last-verified-millis)
   :rel (fn [[e] out] (= out (lv-ms e)))
   :golden-path "test/golden/hive-mcp/m14-last-verified-millis.edn"})

(hst/deftrifecta-from-schema edge-stale-contract
  hive-mcp.knowledge-graph.edges.decay/edge-stale?
  {:in [:cat {:gen/fmap (fn [[e days delta]] [e days (+ (lv-ms e) delta)])} (mu/required-keys EdgeProbe [:kg-edge/last-verified]) [:int {:min 0, :max 60, :gen/elements [0 7 30 60]}] [:int {:min 0, :gen/elements [0 3600000 864000000 3888000000 7776000000]}]]
   :out (sup/out-schema d 'edge-stale?)
   :rel (fn [[e days now] out]
          (= out (or (nil? (:kg-edge/last-verified e))
                     (> (- now (lv-ms e)) (* days day-ms)))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 200
   :golden-path "test/golden/hive-mcp/m14-edge-stale.edn"})

(def ^:private semantic
  #{:implements :supersedes :refines :contradicts :depends-on :derived-from :applies-to})

(hst/deftrifecta-from-schema decay-rate-for-edge-contract
  hive-mcp.knowledge-graph.edges.decay/decay-rate-for-edge
  {:in [:cat (mu/required-keys EdgeProbe [:kg-edge/relation])], :out (sup/out-schema d 'decay-rate-for-edge), :rel (fn [[e] out] (= out (if (semantic (:kg-edge/relation e))
                               decay/semantic-decay-rate
                               decay/co-access-decay-rate))), :classify (fn [_ out] out), :classify-domain #{decay/semantic-decay-rate decay/co-access-decay-rate}, :num-tests 200, :golden-path "test/golden/hive-mcp/m14-decay-rate.edn"})

(defn- cycle-run []
  (sup/in-fresh-store
   (fn []
     (let [old (java.util.Date. (- (System/currentTimeMillis) (* 90 day-ms)))
           now (System/currentTimeMillis)
           stale-sem (write/add-edge! {:from "d1" :to "d2" :relation :implements
                                       :confidence 0.5 :last-verified old})
           stale-co (write/add-edge! {:from "d1" :to "d3" :relation :co-accessed
                                      :confidence 0.12 :last-verified old})
           _fresh (write/add-edge! {:from "d2" :to "d3" :relation :refines})
           step-fresh (decay/decay-step! 30 now {:kg-edge/last-verified (java.util.Date. now)})
           step-decayed (decay/decay-step! 30 now (q/get-edge stale-sem))
           step-pruned (decay/decay-step! 30 now (q/get-edge stale-co))
           after (sup/shown-all (q/get-all-edges))
           cycle (decay/decay-unverified-edges!)
           cycle-scoped (decay/decay-unverified-edges! {:scope "none" :limit 5 :staleness-days 1})]
       {:step-fresh step-fresh :step-decayed step-decayed :step-pruned step-pruned
        :after after :cycle cycle :cycle-scoped cycle-scoped
        :remove-missing (decay/remove-edge! "no-such-edge")
        :final (sup/shown-all (q/get-all-edges))}))))

(golden/deftest-golden decay-cycle-golden
  "test/golden/hive-mcp/m14-edges-decay-cycle.edn"
  (cycle-run))
