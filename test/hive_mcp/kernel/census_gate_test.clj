(ns hive-mcp.kernel.census-gate-test
  "Kernel census gate. The universe is the FILES under src/hive_mcp; the
   allowlist is resources/hive-mcp/kernel.edn. Fails when a namespace is
   unclaimed, when a kernel require leaves the kernel without a waiver, when
   a waiver no longer matches a real edge, or when the waiver count exceeds
   :waiver-baseline."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.kernel.census :as census]))

(def ^:private src-root "src/hive_mcp")

(def ^:private minimum-namespaces 500)

(defn- gate-state []
  (let [allowlist (census/load-allowlist)
        rows (census/census src-root)]
    {:allowlist allowlist
     :rows rows
     :classified (census/classified allowlist rows)
     :edges (census/kernel-edges allowlist rows)}))

(defn- fmt-edges [edges]
  (->> edges
       (sort-by (juxt (comp str :from) (comp str :to)))
       (map (fn [{:keys [from to target]}] (str "  " from " -> " to " (" (name target) ")")))
       (str/join "\n")))

(deftest census-is-not-vacuous
  (let [{:keys [rows]} (gate-state)]
    (is (> (count rows) minimum-namespaces)
        (str "file walk under " src-root " found only " (count rows) " namespaces"))))

(deftest every-namespace-is-claimed
  (let [{:keys [classified]} (gate-state)
        unknown (->> classified (filter #(= :unknown (:class %))) (map :ns) sort)]
    (is (empty? unknown)
        (str "namespaces neither :kernel nor :extract in kernel.edn:\n  "
             (str/join "\n  " unknown)))))

(deftest kernel-edges-are-waived
  (let [{:keys [allowlist edges]} (gate-state)
        waivers (:waivers allowlist)]
    (testing "every kernel -> non-kernel require carries a waiver"
      (let [uncovered (census/uncovered-edges waivers edges)]
        (is (empty? uncovered)
            (str "unwaived kernel edges:\n" (fmt-edges uncovered)))))
    (testing "every waiver still matches a real edge"
      (let [stale (census/stale-waivers waivers edges)]
        (is (empty? stale)
            (str "stale waivers (edge is gone, drop the waiver):\n" (fmt-edges (map #(assoc % :target :stale) stale))))))))

(deftest waiver-count-ratchets-down
  (let [{:keys [allowlist]} (gate-state)
        baseline (:waiver-baseline allowlist)
        current (count (:waivers allowlist))
        by-step (->> (:waivers allowlist) (map :retired-by) frequencies (into (sorted-map)))]
    (println "kernel census gate: waivers" current "of baseline" baseline "by retiring step" by-step)
    (is (integer? baseline) ":waiver-baseline must be recorded in kernel.edn")
    (is (<= current baseline)
        (str "waiver list grew: " current " > baseline " baseline "; lower the count, never raise the baseline"))
    (is (every? #(and (:from %) (:to %) (:since %) (:retired-by %)) (:waivers allowlist))
        "every waiver carries :from :to :since :retired-by")))
