(ns hive-mcp.channel.digest-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.channel.digest :as digest]))

(def opts (digest/settings {}))

(defn- run [rows]
  (mapv digest/finalize (digest/coalesce-rows opts rows)))

(deftest lifecycle-rows-fold-into-terminal
  (let [rows [{:a "l1" :e "started" :m "go" :ts 1}
              {:a "l2" :e "progress" :m "other" :ts 2}
              {:a "l1" :e "progress" :m "turn 1" :ts 3 :n 4}
              {:a "l1" :e "completed" :m "done: 3 tests" :ts 5}]
        out (run rows)]
    (is (= [{:a "l2" :e "progress" :m "other"}
            {:a "l1" :e "completed" :m "done: 3 tests" :n 6 :ts 1}]
           out))))

(deftest deliberate-and-ask-rows-survive-folding
  (let [rows [{:a "l1" :e "progress" :m "I found the bug" :deliberate? true :ts 1}
              {:a "l1" :e "ask" :m "which branch?" :ts 2}
              {:a "l1" :e "completed" :m "ok" :ts 3}]]
    (is (= ["progress" "ask" "completed"] (mapv :e (run rows))))
    (is (not-any? :ts (run rows)))))

(deftest no-terminal-means-no-folding
  (let [rows [{:a "l1" :e "started" :m "go" :ts 1}
              {:a "l1" :e "progress" :m "p" :ts 2}]]
    (is (= (mapv digest/strip rows) (run rows)))))

(deftest error-reduces-to-class-status-line
  (let [raw (str "clojure.lang.ExceptionInfo: openrouter API error: 429 - rate limited {:status 429 :body \"...\"}\n"
                 "\tat hive_mcp.agent.openrouter$chat_request.invokeStatic(openrouter.clj:240)\n"
                 "\tat clojure.lang.AFn.run(AFn.java:22)")
        [row] (run [{:a "l1" :e "error" :m raw :ts 9}])]
    (is (str/starts-with? (:m row) "ExceptionInfo 429: openrouter API error"))
    (is (not (str/includes? (:m row) "\n")))
    (is (<= (count (:m row)) 160))
    (is (= 9 (:ts row)))))

(deftest short-error-is-untouched
  (is (= [{:a "l1" :e "error" :m "timeout"}]
         (run [{:a "l1" :e "error" :m "timeout" :ts 1}]))))

(deftest long-message-clips-with-count
  (let [m (str/join " " (repeat 100 "word"))
        [row] (run [{:a "l1" :e "completed" :m m :ts 7}])]
    (is (= 40 (:clip row)))
    (is (= 61 (count (digest/words (:m row)))))
    (is (= 7 (:ts row)))))

(deftest needs-model-only-for-long-non-error-terminals
  (let [long-m (str/join " " (repeat 80 "w"))
        [c e p] (digest/coalesce-rows opts [{:a "a" :e "completed" :m long-m}
                                            {:a "b" :e "error" :m long-m}
                                            {:a "c" :e "progress" :m long-m}])]
    (is (digest/needs-model? 60 c))
    (is (not (digest/needs-model? 60 e)))
    (is (not (digest/needs-model? 60 p)))))

(def gen-row
  (gen/let [a (gen/elements ["l1" "l2" "l3"])
            e (gen/elements ["started" "progress" "completed" "error" "ask" "blocked"])
            m (gen/fmap #(str/join " " %) (gen/vector gen/string-alphanumeric 0 120))
            deliberate? gen/boolean
            ts gen/nat]
    (cond-> {:a a :e e :m m :ts ts}
      deliberate? (assoc :deliberate? true))))

(defspec digest-never-grows-and-never-drops-signal 100
  (prop/for-all [rows (gen/vector gen-row 0 20)]
    (let [out (run rows)
          kept (set (map (juxt :a :e) out))]
      (and (<= (count out) (count rows))
           (every? #(<= (count (digest/words (:m %))) (inc (:max-words opts))) out)
           (every? #(contains? kept [(:a %) (:e %)])
                   (filter #(or (digest/terminal? %) (:deliberate? %) (= "ask" (:e %))) rows))
           (not-any? #(contains? % ::digest/texts) out)))))

(defspec untouched-rows-carry-no-ts 100
  (prop/for-all [rows (gen/vector gen-row 0 20)]
    (let [raw-ms (set (map :m rows))]
      (every? #(or (not (contains? % :ts)) (:n %) (:clip %) (not (contains? raw-ms (:m %))))
              (run rows)))))
