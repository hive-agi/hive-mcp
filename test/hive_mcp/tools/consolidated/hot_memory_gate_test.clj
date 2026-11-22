(ns hive-mcp.tools.consolidated.hot-memory-gate-test
  "The memory gate on heavy hot reloads, over an injected meminfo reader."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.consolidated.hot-memory-gate :as gate]))

(def ^:private gb 1048576)

(def ^:private meminfo-text
  (str "MemTotal:       65000000 kB\n"
       "MemAvailable:    7428880 kB\n"
       "SwapTotal:       8388604 kB\n"
       "SwapFree:            748 kB\n"))

(deftest parse-meminfo-reads-the-three-figures
  (is (= {:mem-available-kb 7428880 :swap-free-kb 748 :swap-total-kb 8388604}
         (gate/parse-meminfo meminfo-text)))
  (is (= {} (gate/parse-meminfo "")) "no figures, no keys")
  (is (= {:mem-available-kb 1} (gate/parse-meminfo "MemAvailable: 1 kB\nBogus: x kB"))))

(deftest verdict-refuses-low-memory-or-full-swap
  (testing "plenty of memory and swap: allowed"
    (is (nil? (gate/verdict {:mem-available-kb (* 20 gb) :swap-free-kb 8000000 :swap-total-kb 8388604}))))
  (testing "MemAvailable under the floor"
    (let [r (gate/verdict {:mem-available-kb (* 7 gb) :swap-free-kb 8000000 :swap-total-kb 8388604})]
      (is (= :hot/memory-low (:reason r)))
      (is (true? (:refused? r)))
      (is (= 7.0 (:mem-available-gb r)))
      (is (str/includes? (:message r) "MemAvailable 7.0 GB < 15 GB floor"))
      (is (str/includes? (:message r) "force_memory=true") "the refusal names the override")))
  (testing "swap nearly full, even with memory above the floor"
    (let [r (gate/verdict {:mem-available-kb (* 20 gb) :swap-free-kb 748 :swap-total-kb 8388604})]
      (is (= :hot/memory-low (:reason r)))
      (is (str/includes? (:message r) "SwapFree 748 of 8388604 kB"))))
  (testing "no swap configured is not a full swap"
    (is (nil? (gate/verdict {:mem-available-kb (* 20 gb) :swap-free-kb 0 :swap-total-kb 0}))))
  (testing "an unreadable host is not refused"
    (is (nil? (gate/verdict {}))))
  (testing "the floor is configurable"
    (is (nil? (gate/verdict {:mem-available-kb (* 7 gb)} {:floor-gb 4})))))

(deftest only-heavy-reloads-are-gated
  (let [starved (constantly {:mem-available-kb (* 2 gb) :swap-free-kb 0 :swap-total-kb 8388604})]
    (is (nil? (gate/refusal :reload {:addon "hive.git"} starved))
        "a plain reload of one changed addon is never gated")
    (is (some? (gate/refusal :reload {:addon "hive.git" :force true} starved)))
    (is (some? (gate/refusal :reload {:addon "hive.git" :force "true"} starved)))
    (is (some? (gate/refusal :reload-all {} starved)))
    (is (nil? (gate/refusal :reload-all {:force_memory true} starved)) "the operator override runs it")
    (is (nil? (gate/refusal :reload {:force true :force_memory "true"} starved)))
    (is (nil? (gate/refusal :reload-all {} (constantly nil))) "no meminfo, no refusal")))
