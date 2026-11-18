(ns hive-mcp.crystal.history-test
  "`session history` (SYNTHESIS-HISTORY slice (c)).

     1. Pure: clamp-limit, latest-summary, digest, walk-chain (golden cases and
        a 200-sample property over random edge graphs, cycles included).
     2. Pipeline: `history`/`render` over a map-backed ISummaryHistory port.
     3. Boundary: StoreHistory over hive-spi's reference store with the KG
        reader injected through the record, so no live KG and no with-redefs."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.crystal.history :as h]
            [hive-spi.memory.ports :as ports]
            [hive-spi.memory.stub :as stub]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- sid [day hhmmss n] (format "%s%s-%08d" day hhmmss n))

(def ^:private d1 (sid "20261116" "090000" 1))
(def ^:private d2 (sid "20261117" "090000" 2))
(def ^:private d3 (sid "20261118" "090000" 3))

(defn- summary [id pid & [day]]
  {:id id :project-id pid
   :tags (cond-> ["session-summary" "wrap-generated"] day (conj (str "session:" day)))})

;; =============================================================================
;; clamp-limit
;; =============================================================================

(deftest clamp-limit-bounds-the-walk
  (is (= h/default-limit (h/clamp-limit nil)))
  (is (= h/default-limit (h/clamp-limit "ten")))
  (is (= h/default-limit (h/clamp-limit :x)))
  (is (= 5 (h/clamp-limit 5)))
  (is (= 5 (h/clamp-limit " 5 ")))
  (is (= 1 (h/clamp-limit 0)))
  (is (= 1 (h/clamp-limit -3)))
  (is (= h/max-limit (h/clamp-limit 100000)))
  (is (= 7 (h/clamp-limit 7.9))))

;; =============================================================================
;; latest-summary
;; =============================================================================

(deftest latest-summary-is-the-newest-summary-in-scope
  (testing "the greatest id wins whatever the input order"
    (is (= d3 (h/latest-summary "a" [(summary d2 "a") (summary d3 "a") (summary d1 "a")]))))
  (testing "other projects and non-summaries are not heads"
    (is (= d1 (h/latest-summary "a" [(summary d1 "a")
                                     (summary d3 "b")
                                     {:id d2 :project-id "a" :tags ["decision"]}]))))
  (testing "scope by tag counts"
    (is (= d2 (h/latest-summary "a" [{:id d2 :tags ["session-summary" "scope:project:a"]}]))))
  (testing "nothing stored"
    (is (nil? (h/latest-summary "a" [])))))

;; =============================================================================
;; digest
;; =============================================================================

(deftest digest-is-the-first-content-line
  (is (= "Chose a two-phase park contract."
         (h/digest "## Session Synthesis: 2026-11-17\n\n# Session Synthesis\n\n**Key Decisions**\n- **Chose** a two-phase park contract.\n- other")))
  (is (= "first point" (h/digest "1. first point")))
  (is (nil? (h/digest "# only a header\n\n---")))
  (is (nil? (h/digest nil)))
  (is (nil? (h/digest "")))
  (testing "a long line is cut to 120 chars"
    (let [d (h/digest (apply str (repeat 300 "x")))]
      (is (= 120 (count d)))
      (is (str/ends-with? d "…")))))

;; =============================================================================
;; walk-chain
;; =============================================================================

(defn- next-of [edges] (fn [id] (get edges id [])))

(deftest walk-chain-follows-supersedes-newest-first
  (let [edges {d3 [d2] d2 [d1]}]
    (testing "a linear chain to its end"
      (is (= {:ids [d3 d2 d1] :stop :end} (h/walk-chain (next-of edges) d3 10))))
    (testing "the limit stops a chain that goes on"
      (is (= {:ids [d3 d2] :stop :limit} (h/walk-chain (next-of edges) d3 2))))
    (testing "a limit equal to the chain length reaches :end"
      (is (= {:ids [d3 d2 d1] :stop :end} (h/walk-chain (next-of edges) d3 3))))
    (testing "no head is :empty"
      (is (= {:ids [] :stop :empty} (h/walk-chain (next-of edges) nil 10))))))

(deftest walk-chain-guards-cycles
  (testing "a two-node cycle ends after both, not after the limit"
    (is (= {:ids [d3 d2] :stop :cycle}
           (h/walk-chain (next-of {d3 [d2] d2 [d3]}) d3 50))))
  (testing "a self-loop"
    (is (= {:ids [d3] :stop :cycle} (h/walk-chain (next-of {d3 [d3]}) d3 50))))
  (testing "a branch takes the newest unvisited predecessor"
    (is (= [d3 d2 d1] (:ids (h/walk-chain (next-of {d3 [d1 d2] d2 [d3 d1]}) d3 50))))))

(deftest walk-chain-never-throws
  (let [r (h/walk-chain (fn [id] (if (= id d2) (throw (ex-info "kg down" {})) [d2])) d3 10)]
    (is (= [d3 d2] (:ids r)))
    (is (= :error (:stop r)))
    (is (= "kg down" (:error r)))))

(def ^:private gen-graph
  "A random edge map over ids 0..7 (any shape: chains, branches, cycles,
   self-loops) plus a start and a limit."
  (gen/let [n     (gen/choose 1 8)
            edges (gen/map (gen/fmap str (gen/choose 0 7))
                           (gen/vector (gen/fmap str (gen/choose 0 7)) 0 3))
            start (gen/fmap str (gen/choose 0 (dec n)))
            limit (gen/choose 1 12)]
    {:edges edges :start start :limit limit}))

(deftest walk-chain-property-bounded-distinct-linked
  (let [r (tc/quick-check
           200
           (prop/for-all [{:keys [edges start limit]} gen-graph]
             (let [{:keys [ids stop]} (h/walk-chain (next-of edges) start limit)]
               (and (= start (first ids))
                    (<= (count ids) limit)
                    (apply distinct? ids)
                    ;; every step follows a stored edge
                    (every? (fn [[a b]] (some #{b} (get edges a))) (partition 2 1 ids))
                    ;; the stop reason is truthful about the last id
                    (let [nexts (set (get edges (peek ids)))]
                      (case stop
                        :end   (empty? nexts)
                        :cycle (and (seq nexts) (every? (set ids) nexts))
                        :limit (and (= limit (count ids)) (seq (remove (set ids) nexts)))
                        false))))))]
    (is (:pass? r) (pr-str (:shrunk r)))))

;; =============================================================================
;; history + render over a map-backed port
;; =============================================================================

(defn- map-port
  "ISummaryHistory over plain data: `summaries` by project, `edges` id ->
   superseded ids, `entries` id -> entry."
  [summaries edges entries]
  (reify h/ISummaryHistory
    (-summaries [_ pid] (get summaries pid []))
    (-superseded-by [_ id] (get edges id []))
    (-entry [_ id] (get entries id))))

(def ^:private entries
  {d3 {:id d3 :tags ["session:2026-11-18"] :content "## Session Synthesis\n- third day work"}
   d2 {:id d2 :tags ["session:2026-11-17"] :content "second day work"}})

(deftest history-walks-from-the-latest-summary
  (let [port (map-port {"a" [(summary d1 "a") (summary d3 "a") (summary d2 "a")]}
                       {d3 [d2] d2 [d1]}
                       entries)
        hist (h/history port "a" nil)]
    (is (= {:project-id "a" :head d3 :limit 10 :count 3 :stop :end
            :entries [{:id d3 :date "2026-11-18" :digest "third day work"}
                      {:id d2 :date "2026-11-17" :digest "second day work"}
                      {:id d1 :date "2026-11-16" :digest nil :missing true}]}
           hist))
    (testing "the render is one header plus one line per summary"
      (is (= (str "session history a: 3 summaries, newest first (end of chain)\n"
                  "2026-11-18  " d3 "  third day work\n"
                  "2026-11-17  " d2 "  second day work\n"
                  "2026-11-16  " d1 "  (missing from store)")
             (h/render hist))))))

(deftest history-respects-the-limit
  (let [port (map-port {"a" [(summary d3 "a")]} {d3 [d2] d2 [d1]} entries)
        hist (h/history port "a" 2)]
    (is (= [d3 d2] (mapv :id (:entries hist))))
    (is (= :limit (:stop hist)))
    (is (str/includes? (h/render hist) "limit 2; older summaries remain"))))

(deftest history-of-a-project-with-no-summary
  (let [hist (h/history (map-port {} {} {}) "nobody" 5)]
    (is (= {:project-id "nobody" :head nil :limit 5 :count 0 :stop :empty :entries []} hist))
    (is (= "session history nobody: no wrap summary stored" (h/render hist)))))

(deftest history-never-throws-on-a-failing-port
  (let [port (reify h/ISummaryHistory
               (-summaries [_ _] (throw (ex-info "store down" {})))
               (-superseded-by [_ _] [])
               (-entry [_ _] nil))
        hist (h/history port "a" 5)]
    (is (= :error (:stop hist)))
    (is (= "store down" (:error hist)))
    (is (= [] (:entries hist)))
    (is (str/includes? (h/render hist) "read error: store down"))))

(deftest report-carries-data-and-text
  (let [r (h/report (map-port {"a" [(summary d2 "a")]} {} entries) "a" 3)]
    (is (= [d2] (mapv :id (:entries r))))
    (is (= (h/render (dissoc r :text)) (:text r)))))

;; =============================================================================
;; Boundary: StoreHistory over the reference store
;; =============================================================================

(deftest store-history-reads-the-store-and-filters-supersedes-edges
  (let [store (stub/create-store)
        kg    {d3 [{:kg-edge/from d3 :kg-edge/to d2 :kg-edge/relation :supersedes}
                   {:kg-edge/from d3 :kg-edge/to d1 :kg-edge/relation :relates}]
               d2 [{:kg-edge/from d2 :kg-edge/to d1 :kg-edge/relation "supersedes"}]}
        port  (h/->StoreHistory store (fn [id] (get kg id [])))]
    (doseq [[id day content] [[d1 "2026-11-16" "first"]
                              [d2 "2026-11-17" "second"]
                              [d3 "2026-11-18" "third"]]]
      (ports/add-entry! store {:id id :type "note" :project-id "a" :content content
                               :tags ["session-summary" "wrap-generated" (str "session:" day)]}))
    (ports/add-entry! store {:id (sid "20261119" "090000" 9) :type "note" :project-id "b"
                             :content "other project" :tags ["session-summary"]})
    (let [hist (h/history port "a" 10)]
      (testing "the head is a's newest summary, not b's"
        (is (= d3 (:head hist))))
      (testing "only :supersedes edges are walked (keyword or string relation)"
        (is (= [d3 d2 d1] (mapv :id (:entries hist)))))
      (testing "digests come from the stored content"
        (is (= ["third" "second" "first"] (mapv :digest (:entries hist)))))
      (is (= :end (:stop hist))))))
