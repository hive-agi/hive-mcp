(ns hive-mcp.telemetry.resources-facet-test
  "Facets for hive-mcp.telemetry.resources over its pure core: the bounded
   ring (property), the thread-group name folding (property) and the trend
   math over a fixed ring (golden). No MX bean is read."
  (:require [clojure.string :as str]
            [hive-mcp.telemetry.resources :as res]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema push-ring hive-mcp.telemetry.resources/push-ring
  {:in  [:cat [:maybe [:vector {:gen/max 8} res/Sample]] res/Sample [:int {:min 1 :max 6}]]
   :out res/Ring
   :rel (fn [[ring s cap] out]
          (let [all (conj (vec ring) s)]
            (and (= (min cap (count all)) (count out))
                 (= s (peek out))
                 (= out (vec (take-last (count out) all))))))
   :classify (fn [[ring _ cap] _] (if (>= (count ring) cap) :evicts :grows))
   :classify-domain #{:evicts :grows}
   :num-tests 200})

(deftrifecta-from-schema group-name hive-mcp.telemetry.resources/group-name
  {:in  [:enum "worker-1" "worker-22" "pool-3-thread-7" "main" "" "  "
         "nREPL-session-0a1b2c3d-4e5f" "G1 Young Generation" "async-dispatch-12"]
   :out :string
   :rel (fn [in out]
          (and (not (str/blank? out))
               (not (re-find #"(?<![A-Za-z0-9])\d+" out))
               (if (str/blank? in) (= "<unnamed>" out) true)))
   :idempotent? true
   :num-tests 100})

(def ^:private mb 1048576)

(defn- sample
  "A derived sample at minute `t`."
  [t meta classes rss old]
  {:at-ms (* t 60000)
   :non-heap {:metaspace (* meta mb)}
   :classes {:loaded classes}
   :process {:rss-bytes (* rss mb)}
   :heap {:old-gen-after-gc (* old mb)}})

(golden/deftest-golden trend-golden
  "test/golden/hive-mcp/telemetry/resources-trend.edn"
  (into (sorted-map)
        (for [[label ring] {"empty"   []
                            "one"     [(sample 0 800 189000 4300 900)]
                            "growing" [(sample 0 800 189000 4300 900)
                                       (sample 30 810 189500 4350 910)
                                       (sample 60 820 190000 4400 920)]
                            "flat"    [(sample 0 800 189000 4300 900)
                                       (sample 60 800 189000 4300 900)]}]
          [label (res/trend ring)])))
