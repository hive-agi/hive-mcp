(ns hive-mcp.kernel.frozen-extraction-test
  "The ratchet behind :extraction/listed. A frozen owner (:hive-agent, CORE-X1)
   keeps the namespaces it has today; a namespace that appears under one of
   its :extraction prefixes without being listed fails the census gate, so new
   swarm, agent and hivemind code is written in hive-agent, not in core.

   Golden: test/golden/kernel/hive-agent-listed.edn pins the listed set; it
   may only shrink. Run with UPDATE_GOLDEN=true after an extraction PR."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.kernel.census :as census]
            [hive-test.golden :refer [deftest-golden]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private src-root "src/hive_mcp")

(defn- state []
  (let [allowlist (census/load-allowlist)]
    {:allowlist allowlist :rows (census/census src-root)}))

(def ^:private gen-segment
  (gen/fmap #(str "x" %) (gen/not-empty gen/string-alphanumeric)))

(def ^:private gen-new-ns
  "A namespace symbol under a frozen hive-agent prefix that no file has."
  (gen/let [prefix (gen/elements ["agent" "swarm" "hivemind" "tools.swarm"
                                  "agent.provider" "swarm.claim"
                                  "tools.consolidated.swarm"])
            leaf gen-segment]
    (symbol (str "hive-mcp." prefix ".zz-new-" (.toLowerCase ^String leaf)))))

(def ^:private gen-kernel-ns
  (gen/let [prefix (gen/elements ["server" "spi" "tools.multi" "kernel"])
            leaf gen-segment]
    (symbol (str "hive-mcp." prefix ".zz-new-" (.toLowerCase ^String leaf)))))

(defspec a-new-namespace-under-a-frozen-prefix-fails-the-gate 50
  (let [{:keys [allowlist rows]} (state)]
    (prop/for-all [ns gen-new-ns]
      (= [{:ns ns :target :hive-agent}]
         (census/unlisted-extractions allowlist (conj rows {:ns ns :requires []}))))))

(defspec a-new-kernel-namespace-is-not-a-frozen-extraction 50
  (let [{:keys [allowlist rows]} (state)]
    (prop/for-all [ns gen-kernel-ns]
      (empty? (census/unlisted-extractions allowlist (conj rows {:ns ns :requires []}))))))

(deftest unfrozen-owners-still-accept-new-namespaces
  (let [{:keys [allowlist rows]} (state)]
    (testing "memory is extracting but not frozen"
      (is (empty? (census/unlisted-extractions
                   allowlist (conj rows {:ns 'hive-mcp.memory.zz-new :requires []})))))))

(deftest a-listed-namespace-whose-file-is-gone-is-stale
  (let [{:keys [allowlist rows]} (state)
        gone (first (sort (get (census/listed-extractions allowlist) :hive-agent)))]
    (is (= [{:ns gone :target :hive-agent}]
           (census/stale-listings allowlist (remove #(= gone (:ns %)) rows))))))

(deftest-golden hive-agent-listed
  "test/golden/kernel/hive-agent-listed.edn"
  (vec (sort (get (census/listed-extractions (census/load-allowlist)) :hive-agent))))
