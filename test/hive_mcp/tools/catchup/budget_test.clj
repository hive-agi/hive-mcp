(ns hive-mcp.tools.catchup.budget-test
  "The inline catchup byte budget: the answer stays under budget, pinned
   blocks (header, axiom-review, kanban) always arrive whole, and every
   shed block's ref resolves to the full block."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.data.json :as json]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.tools.catchup.budget :as budget]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- block [m] {:type "text" :text (json/write-str m)})

(defn- filler [n] (apply str (repeat n "x")))

(defn- answer
  "A catchup-shaped answer of roughly 30k bytes."
  []
  [(block {:_block "header" :success true :project "p"
           :counts {:axioms 34 :principles 50}
           :memory-piggyback {:context-refs {:axioms "ctx-1-aaaaaaaa"}}})
   (block {:_block "axiom-review" :awaiting-human-review 1
           :candidates [{:id "c1" :preview (filler 200)}]})
   (block {:_block "context"
           :context {:sessions (vec (for [i (range 25)] {:id (str "s" i) :T "note" :P (filler 80)}))}
           :via-piggyback (filler 120)})
   (block {:_block "recent-wraps"
           :recent-wraps (vec (for [i (range 10)] {:id (str "w" i) :tags ["a" "b"] :preview (filler 240)}))
           :hint "Last 10 wraps."})
   (block {:_block "meta" :project-tree nil :hint (filler 150)})
   (block {:_block "kanban" :counts {:todo 218 :inprogress 2}
           :recent-todos (vec (for [i (range 10)] {:id (str "k" i) :title (filler 100)}))})
   (block {:_block "tool-friction" :open (vec (repeat 5 (filler 120)))})
   (block {:_block "kg-digest" :top-nodes (vec (for [i (range 40)] {:id (str "n" i) :label (filler 60)}))
           :clusters (vec (repeat 20 {:members (vec (range 20))}))
           :narrative (filler 2000)})
   (block {:_block "carto-status" :readiness "ready" :warnings (vec (repeat 30 (filler 80)))})])

(defn- atom-store
  "A put-ref! backed by an atom: returns [store put-ref!]."
  []
  (let [store (atom {}) n (atom 0)]
    [store (fn [_name data]
             (let [ref (str "ctx-test-" (swap! n inc))]
               (swap! store assoc ref data)
               ref))]))

(defn- names [blocks] (mapv budget/block-name blocks))

(defn- parsed [blocks] (mapv #(json/read-str (:text %)) blocks))

(def ^:private pinned ["header" "axiom-review" "kanban"])

(defn- by-name [blocks n] (first (filter #(= n (budget/block-name %)) blocks)))

(deftest under-budget-is-unchanged
  (let [blocks (answer)
        [_ put!] (atom-store)]
    (is (= blocks (budget/fit-blocks blocks {:budget 1000000 :put-ref! put!})))
    (testing "0 disables the trim"
      (is (= blocks (budget/fit-blocks blocks {:budget 0 :put-ref! put!}))))))

(deftest over-budget-fits-and-keeps-pinned
  (let [blocks (answer)
        before (budget/answer-bytes blocks)
        [store put!] (atom-store)
        fitted (budget/fit-blocks blocks {:budget 9000 :put-ref! put!})]
    (is (> before 15000) "the fixture is really over budget")
    (testing "the inline part is under budget"
      (is (<= (budget/answer-bytes fitted) 9000)))
    (testing "header, axiom-review and kanban arrive whole"
      (doseq [n pinned]
        (is (= (by-name blocks n) (by-name fitted n)) n)))
    (testing "one inline-budget note, right after the header"
      (is (= "inline-budget" (second (names fitted))))
      (let [note (json/read-str (:text (second fitted)))]
        (is (= 9000 (get note "budget")))
        (is (= before (get note "bytes-before")))
        (is (= (budget/answer-bytes fitted) (get note "bytes-inline")))
        (is (false? (get note "over-budget")))
        (is (seq (get note "shed-blocks")))))
    (testing "every block still has a section, in the original order"
      (is (= (names blocks) (vec (remove #{"inline-budget"} (names fitted))))))
    (testing "every shed stub's ref resolves to the full block"
      (let [stubs (filter #(get % "shed") (parsed fitted))
            originals (into {} (map (juxt #(get % "_block") identity)) (parsed blocks))]
        (is (seq stubs))
        (doseq [s stubs]
          (is (string? (get s "ref")))
          (is (= (budget/fetch-call (get s "ref")) (get s "fetch")))
          (is (= (get originals (get s "_block")) (get @store (get s "ref")))
              (get s "_block")))))))

(deftest sheds-bulkiest-first
  (let [blocks (answer)
        total (budget/answer-bytes blocks)
        kg (budget/utf8-bytes (:text (by-name blocks "kg-digest")))
        [_ put!] (atom-store)
        ;; Room for everything but most of kg-digest.
        fitted (budget/fit-blocks blocks {:budget (- total (quot kg 2)) :put-ref! put!})
        shed (set (map #(get % "_block") (filter #(get % "shed") (parsed fitted))))]
    (is (= #{"kg-digest"} shed))))

(deftest refs-resolve-through-the-context-store
  (let [blocks (answer)
        fitted (budget/fit-blocks
                blocks
                {:budget 9000
                 :put-ref! (fn [n data]
                             (context-store/context-put! data :tags #{"catchup-shed" n}
                                                         :ttl-ms budget/ref-ttl-ms))})
        stubs (filter #(get % "shed") (parsed fitted))]
    (is (seq stubs))
    (try
      (doseq [s stubs]
        (is (= (get s "_block") (get (:data (context-store/context-get (get s "ref"))) "_block"))))
      (finally
        (doseq [s stubs] (context-store/context-evict! (get s "ref")))))))

(deftest a-failing-ref-store-keeps-blocks-inline
  (let [blocks (answer)]
    (is (= blocks (budget/fit-blocks blocks {:budget 9000 :put-ref! (fn [_ _] nil)})))
    (is (= blocks (budget/fit-blocks blocks {:budget 9000
                                             :put-ref! (fn [_ _] (throw (ex-info "down" {})))})))))

(deftest pinned-over-budget-is-answered-whole
  (let [blocks (answer)
        [_ put!] (atom-store)
        fitted (budget/fit-blocks blocks {:budget 500 :put-ref! put!})
        note (json/read-str (:text (second fitted)))]
    (doseq [n pinned]
      (is (= (by-name blocks n) (by-name fitted n)) n))
    (is (true? (get note "over-budget")))
    (testing "stubs drop their summary before anything else goes"
      (is (every? #(not (contains? % "summary")) (filter #(get % "shed") (parsed fitted)))))))

(deftest resolve-budget-precedence
  (is (= budget/default-inline-budget (budget/resolve-budget {} nil)))
  (is (= 12000 (budget/resolve-budget {} 12000)))
  (is (= 12000 (budget/resolve-budget {} "12000")))
  (is (= 4000 (budget/resolve-budget {:inline_budget 4000} 12000)))
  (is (= 0 (budget/resolve-budget {:inline_budget "0"} 12000)))
  (is (= 12000 (budget/resolve-budget {:inline_budget -1} 12000)))
  (is (= budget/default-inline-budget (budget/resolve-budget {:inline_budget "lots"} "nope"))))

(deftest summarize-is-short
  (is (= {"count" 3 "ids" ["a" "b" "c"]}
         (budget/summarize [{"id" "a"} {"id" "b"} {"id" "c"}])))
  (is (= {"n" 1 "s" "ok"} (budget/summarize {"_block" "x" "hint" "h" "n" 1 "s" "ok"})))
  (is (< (count (budget/summarize (filler 500))) 100)))

(defspec fit-blocks-holds-its-promises 50
  (prop/for-all [b (gen/choose 0 40000)]
                (let [blocks (answer)
                      [store put!] (atom-store)
                      fitted (budget/fit-blocks blocks {:budget b :put-ref! put!})
                      sheddable-left (remove (set (conj pinned "inline-budget"))
                                             (map #(get % "_block") (remove #(get % "shed") (parsed fitted))))]
                  (and (every? #(= (by-name blocks %) (by-name fitted %)) pinned)
                       (or (zero? b)
                           (<= (budget/answer-bytes fitted) b)
                           (empty? sheddable-left))
                       (every? #(contains? @store (get % "ref"))
                               (filter #(get % "shed") (parsed fitted)))))))
