(ns hive-mcp.knowledge-graph.disc.volatility-trifecta-test
  "Trifectas for the pure disc scoring fns, driven by their registered
   contracts: the output schema is read back from the function registry, so a
   contract change is felt here."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.knowledge-graph.disc.schema :as ds]
            [hive-mcp.knowledge-graph.disc.volatility :as vol]
            [hive-schemas.test :as hst]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- out-schema
  "The output schema of the contract registered for ns/fn (the first arity of
   a :function contract)."
  [nsym fsym]
  (let [s (get-in (m/function-schemas) [nsym fsym :schema])
        s (if (= :function (m/type s)) (first (m/children s)) s)]
    (second (m/children s))))

(def ^:private v 'hive-mcp.knowledge-graph.disc.volatility)

(defn- alpha [d] (or (:disc/certainty-alpha d) 5.0))
(defn- beta [d] (or (:disc/certainty-beta d) 2.0))
(defn- close? [a b] (< (Math/abs (double (- a b))) 1e-9))

;; Paths that reach every pattern arm, not only :moderate.
(def ^:private PathProbe
  [:string {:gen/elements ["deps.edn" "sub/project.clj" "pom.xml" ".gitignore"
                           "server.log" "x.tmp" "target/classes/a.class" ".nrepl-port"
                           "src/a.clj" "README.md" "deps.edn.bak"]}])

(defn- expected-class [p]
  (cond
    (some #(str/ends-with? p %) ["deps.edn" "project.clj" "pom.xml" ".gitignore"]) :stable
    (or (some #(str/ends-with? p %) [".log" ".tmp" ".nrepl-port"])
        (str/includes? p "target/")) :volatile
    :else :moderate))

(hst/deftrifecta-from-schema classify-volatility-contract
  hive-mcp.knowledge-graph.disc.volatility/classify-volatility
  {:in [:cat PathProbe]
   :out (out-schema v 'classify-volatility)
   :rel (fn [[p] out] (= out (expected-class p)))
   :classify (fn [_ out] out)
   :classify-domain ds/VolatilityClass
   :num-tests 200
   :golden-path "test/golden/hive-mcp/m14-classify-volatility.edn"})

(hst/deftrifecta-from-schema current-certainty-contract
  hive-mcp.knowledge-graph.disc.volatility/current-certainty
  {:in [:cat [:maybe ds/Disc]]
   :out (out-schema v 'current-certainty)
   :rel (fn [[d] out] (close? out (/ (alpha d) (+ (alpha d) (beta d)))))
   :golden-path "test/golden/hive-mcp/m14-current-certainty.edn"})

(hst/deftrifecta-from-schema beta-lower-bound-contract
  hive-mcp.knowledge-graph.disc.volatility/beta-lower-bound
  {:in [:cat [:maybe ds/Disc]]
   :out (out-schema v 'beta-lower-bound)
   ;; the lower bound of the credible interval never exceeds its mean
   :rel (fn [[d] out] (<= out (+ 1e-9 (vol/current-certainty d))))
   :golden-path "test/golden/hive-mcp/m14-beta-lower-bound.edn"})

(hst/deftrifecta-from-schema needs-read-contract
  hive-mcp.knowledge-graph.disc.volatility/needs-read?
  {:in [:cat [:maybe ds/Disc] [:double {:min 0.0 :max 1.0}]]
   :out :boolean
   :rel (fn [[d t] out] (= out (or (< (vol/current-certainty d) t)
                                   (< (vol/beta-lower-bound d) (* 0.5 t)))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 200})

(def ^:private Event
  [:enum :read-confirmed :hash-mismatch :git-commit-touched :time-decay :unknown-event])

(hst/deftrifecta-from-schema update-certainty-contract
  hive-mcp.knowledge-graph.disc.volatility/update-certainty
  {:in [:cat [:maybe ds/Disc] Event]
   :out (out-schema v 'update-certainty)
   :rel (fn [[d e] out]
          (let [a (alpha d) b (beta d)
                [a' b'] (case e
                          :read-confirmed [(+ a 3) b]
                          :hash-mismatch [a (+ b 5)]
                          :git-commit-touched [a (+ b 2)]
                          :time-decay [a (+ b 0.5)]
                          [a b])]
            (and (close? a' (:disc/certainty-alpha out))
                 (close? b' (:disc/certainty-beta out)))))
   :classify (fn [[_ e] _] e)
   :classify-domain Event
   :num-tests 200
   :golden-path "test/golden/hive-mcp/m14-update-certainty.edn"})

(hst/deftrifecta-from-schema entry-staleness-score-contract
  hive-mcp.knowledge-graph.disc.volatility/entry-staleness-score
  {:in [:cat [:maybe ds/Entry]]
   :out (out-schema v 'entry-staleness-score)
   :rel (fn [[e] out]
          (let [a (or (:staleness-alpha e) 1.0) b (or (:staleness-beta e) 1.0)]
            (close? 1.0 (+ out (/ a (+ a b))))))
   :golden-path "test/golden/hive-mcp/m14-entry-staleness-score.edn"})

(hst/deftrifecta-from-schema entry-staleness-report-contract
  hive-mcp.knowledge-graph.disc.volatility/entry-staleness-report
  {:in [:cat [:maybe ds/Entry]]
   :out (out-schema v 'entry-staleness-report)
   :rel (fn [[e] out]
          (and (= (:id e) (:id out))
               (= (:grounded-from e) (:grounded-from out))
               (= (vol/entry-staleness-score e) (:score out))))})

(def ^:private Warnings
  [:maybe [:vector {:max 4} [:map [:message [:enum "NOTE: a is stale" "NOTE: b is stale"]]]]])

(hst/deftrifecta-from-schema format-staleness-warnings-contract
  hive-mcp.knowledge-graph.disc.volatility/format-staleness-warnings
  {:in [:cat Warnings]
   :out (out-schema v 'format-staleness-warnings)
   :rel (fn [[ws] out]
          (if (seq ws)
            (and (str/starts-with? out "## file-level Staleness Warnings\n")
                 (every? #(str/includes? out (:message %)) ws))
            (nil? out)))
   :classify (fn [[ws] _] (boolean (seq ws)))
   :classify-domain #{true false}
   :golden-path "test/golden/hive-mcp/m14-format-staleness-warnings.edn"})

(deftest time-fns-score-a-known-disc
  (let [day 86400000
        old (java.util.Date. (- (System/currentTimeMillis) (* 40 day)))
        disc {:disc/path "a.clj" :disc/content-hash "h1" :disc/analyzed-at old
              :disc/last-read-at old :disc/certainty-beta 2.0
              :disc/volatility-class :volatile :disc/last-observation old}]
    (is (= {:score 1.0 :days-since-read 40 :hash-mismatch? true :never-analyzed? false}
           (vol/staleness-report disc {:exists? true :hash "h2"})))
    (is (== 1.0 (vol/staleness-score disc {:exists? true :hash "h2"})))
    (is (== 0.5 (vol/staleness-score (dissoc disc :disc/content-hash) {:exists? false})))
    (is (== 0.5 (vol/staleness-score nil nil)) "never read, never analyzed")
    (let [decayed (vol/apply-time-decay disc)]
      (is (m/validate (out-schema v 'apply-time-decay) decayed))
      (is (< 7.9 (:disc/certainty-beta decayed) 8.1) "40 days at the volatile 0.15/day"))))
