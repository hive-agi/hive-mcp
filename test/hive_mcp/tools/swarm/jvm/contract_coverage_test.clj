(ns hive-mcp.tools.swarm.jvm.contract-coverage-test
  "Gate: every public fn under src/hive_mcp/tools/swarm/jvm carries an `m/=>`
   contract or a REASONED exemption, and no exemption is stale.

   Every namespace there is a compat shim over hive-agent, so their trifectas
   live in the swarm suite (test-swarm/hive_mcp/tools/swarm/jvm), where the
   addon is on the classpath. What runs here without the addon: the gate,
   every contract generating both sides, and the shims failing loudly when
   hive-agent is absent."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.swarm.delegate :as delegate]
            [hive-mcp.tools.swarm.jvm.classifier :as classifier]
            [hive-mcp.tools.swarm.jvm.memory :as memory]
            [hive-mcp.tools.swarm.jvm.orphan :as orphan]
            [hive-mcp.tools.swarm.jvm.parser]
            [hive-mcp.tools.swarm.jvm.summary :as summary]
            [hive-schemas.coverage :as coverage]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private paths ["src/hive_mcp/tools/swarm/jvm"])

(coverage/deftest-contract-coverage tools-swarm-jvm-contracts-gate
  {:paths paths
   :exempt {}})

(deftest tools-swarm-jvm-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths paths :exempt {}})]
    (is (= 20 (count (:universe report))) "every public defn of the five shims")
    (is (= 20 (count (:covered report))))))

(def ^:private jvm-nses
  '[hive-mcp.tools.swarm.jvm.classifier hive-mcp.tools.swarm.jvm.memory
    hive-mcp.tools.swarm.jvm.orphan hive-mcp.tools.swarm.jvm.parser
    hive-mcp.tools.swarm.jvm.summary])

(deftest every-jvm-contract-generates-both-sides
  (doseq [ns-sym jvm-nses
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (let [sch (m/schema schema)]
                  (if (= :function (m/type sch)) (m/children sch) [sch]))
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

(deftest jvm-value-schemas-generate
  (doseq [sch [classifier/JvmType classifier/Pid classifier/ProcessRow classifier/ParentInfo
               classifier/ParentMap classifier/SwarmInfo classifier/ClassifiableProcess
               classifier/ClassifiedProcess classifier/ClassifiedRow
               memory/MemInfo memory/MemError memory/MemReading memory/MemoryStatus
               memory/Recommendation
               orphan/ProtectedTypes orphan/DetectorOpts orphan/TypedProcess orphan/AgedProcess
               orphan/EnrichableProcess orphan/ParentMap orphan/EnrichedProcess
               orphan/DetectableProcess orphan/IdentifiedProcess orphan/Detector
               summary/Pid summary/SummaryProcess summary/CleanupOpts summary/CleanupSummary
               summary/GuardStatus summary/CleanupData summary/GuardSummary]]
    (is (seq (mg/sample sch {:size 4})) (pr-str (m/form sch)))))

(deftest kwargs-tail-accepts-pairs-and-a-trailing-map
  (testing "the orphan options tail is empty, alternating pairs, or one map"
    (is (m/validate orphan/DetectorOpts []))
    (is (m/validate orphan/DetectorOpts [:protected-types #{"nrepl"} :min-age-minutes 30]))
    (is (m/validate orphan/DetectorOpts [{:true-orphans-only false}]))
    (is (not (m/validate orphan/DetectorOpts [:min-age-minutes])) "an odd tail is refused")
    (is (not (m/validate orphan/DetectorOpts [:bogus 1])) "an undeclared key is refused")))

(deftest memory-reading-arms-discriminate
  (testing "a reading and the read error are disjoint arms"
    (let [ok {:total-mb 10 :used-mb 4 :available-mb 6 :percent-used 40}
          bad {:error "Failed to read /proc/meminfo"}]
      (is (m/validate memory/MemInfo ok))
      (is (not (m/validate memory/MemError ok)))
      (is (m/validate memory/MemError bad))
      (is (not (m/validate memory/MemInfo bad))))))

(deftest jvm-shims-fail-loudly-without-the-addon
  (if (delegate/available? "hive-agent.swarm.tools.jvm.classifier")
    (is true "hive-agent is on the classpath; the swarm suite drives the shims")
    (doseq [call [#(classifier/classify-type "java") #(memory/memory-status {} 80 2048)
                  #(orphan/truly-orphaned? {}) #(summary/build-cleanup-summary [] [] [] [] nil {})]]
      (let [e (try (call) nil (catch clojure.lang.ExceptionInfo e e))]
        (is (true? (:swarm/addon-missing (ex-data e))))
        (is (= "hive-agent" (:addon (ex-data e))))))))
