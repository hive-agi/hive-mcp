(ns hive-mcp.tools.consolidated.hot-memory-gate-test
  "The memory gate on heavy hot reloads, over an injected meminfo reader."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.consolidated.hot :as hot]
            [hive-mcp.tools.consolidated.hot-memory-gate :as gate]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private gb 1048576)

(def ^:private meminfo-text
  (str "MemTotal:       65000000 kB\n"
       "MemAvailable:    7428880 kB\n"
       "SwapTotal:       8388604 kB\n"
       "SwapFree:            748 kB\n"))

(def ^:private starved
  (constantly {:mem-available-kb (* 2 gb) :swap-free-kb 0 :swap-total-kb 8388604}))

(def ^:private roomy
  (constantly {:mem-available-kb (* 30 gb) :swap-free-kb 8000000 :swap-total-kb 8388604}))

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
  (testing "the floor and ratio are configurable"
    (is (nil? (gate/verdict {:mem-available-kb (* 7 gb)} {:floor-gb 4})))
    (is (some? (gate/verdict {:mem-available-kb (* 20 gb)} {:floor-gb 24})))
    (is (nil? (gate/verdict {:mem-available-kb (* 20 gb) :swap-free-kb 748 :swap-total-kb 8388604}
                            {:min-swap-free-ratio 0}))))
  (testing "an invalid configured floor falls back to the default"
    (is (= 15 (:floor-gb (gate/verdict {:mem-available-kb (* 7 gb)} {:floor-gb -3}))))
    (is (= 15 (:floor-gb (gate/verdict {:mem-available-kb (* 7 gb)} {:floor-gb "big"}))))))

(deftest only-heavy-reloads-are-gated
  (let [opts {:read-mem starved}]
    (is (nil? (gate/refusal :reload {:addon "hive.git"} opts))
        "a plain reload of one changed addon is never gated")
    (is (some? (gate/refusal :reload {:addon "hive.git" :force true} opts)))
    (is (some? (gate/refusal :reload {:addon "hive.git" :force "true"} opts)))
    (is (some? (gate/refusal :reload-all {} opts)))
    (is (nil? (gate/refusal :reload-all {:force_memory true} opts)) "the operator override runs it")
    (is (nil? (gate/refusal :reload {:force true :force_memory "true"} opts)))
    (is (nil? (gate/refusal :reload-all {} {:read-mem (constantly nil)})) "no meminfo, no refusal")
    (is (nil? (gate/refusal :reload-all {} {:read-mem roomy})) "a healthy host runs it")))

(deftest a-ling-cannot-use-the-operator-override
  (let [r (gate/refusal :reload-all {:force_memory true} {:read-mem starved :ling? true})]
    (is (= :hot/memory-low (:reason r)))
    (is (= :ling-caller (:override-ignored r)))
    (is (str/includes? (:message r) "force_memory was ignored")))
  (is (nil? (gate/refusal :reload {:addon "x"} {:read-mem starved :ling? true}))
      "a ling's plain reload stays allowed"))

(defn- body [resp]
  (json/read-str (:text resp) :key-fn keyword))

(deftest handlers-refuse-before-any-reload-work
  (with-redefs [#_{:clj-kondo/ignore [:private-call]} hot/bridge-available? (constantly true)
                #_{:clj-kondo/ignore [:private-call]} hot/prepared (fn [] (throw (ex-info "the gate must refuse before prepared runs" {})))]
    (binding [hot/*read-meminfo* starved]
      (testing "reload force=true"
        (let [b (body (hot/handle-reload {:addon "hive.carto" :force true}))]
          (is (= "hot/memory-low" (:reason b)))
          (is (true? (:refused? b)))))
      (testing "reload-all"
        (let [b (body (hot/handle-reload-all {}))]
          (is (= "hot/memory-low" (:reason b)))))
      (testing "a ling caller's force_memory is ignored"
        (let [b (body (hot/handle-reload-all {:force_memory true :_caller_id "ling-abc"}))]
          (is (= "ling-caller" (:override-ignored b))))))))

(deftest memory-gate-opts-reads-the-caller-and-config
  (binding [hot/*read-meminfo* roomy]
    (is (true? (:ling? (hot/memory-gate-opts {:_caller_id "swarm-x"}))))
    (is (false? (:ling? (hot/memory-gate-opts {:_caller_id "coordinator:123"}))))
    (is (= roomy (:read-mem (hot/memory-gate-opts {}))))))
