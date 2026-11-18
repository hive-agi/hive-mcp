(ns hive-mcp.crystal.chain-test
  "The per-scope wrap-summary chain: predecessor choice, the pipeline over a
   stub port, and the store adapter over hive-spi's reference store."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.crystal.chain :as chain]
            [hive-spi.memory.ports :as ports]
            [hive-spi.memory.stub :as stub]))

;; =============================================================================
;; previous-summary
;; =============================================================================

(defn- eligible
  "Oracle: candidate ids `previous-summary` may answer with."
  [new-id pid candidates]
  (set (for [{:keys [id tags project-id]} candidates
             :when (and (string? id)
                        (neg? (compare id new-id))
                        (some #{"session-summary"} tags)
                        (or (= pid project-id)
                            (some #{(str "scope:project:" pid)} tags)))]
         id)))

(defn prev-of
  "Unary port over `previous-summary` for the trifecta."
  [[new-id pid candidates :as input]]
  {:in input :out (chain/previous-summary new-id pid candidates)})

(defn- sid [n] (format "20261031%06d-0000000%d" n (mod n 10)))

(def ^:private gen-candidate
  (gen/let [n    (gen/choose 0 40)
            pid  (gen/elements ["a" "b"])
            tagd (gen/elements [:project-id :scope-tag])
            tags (gen/elements [["session-summary"] ["other"] ["session-summary" "other"]])]
    (cond-> {:id (sid n) :tags (cond-> tags (= tagd :scope-tag) (conj (str "scope:project:" pid)))}
      (= tagd :project-id) (assoc :project-id pid))))

(deftrifecta previous-summary-picks-newest-older-same-scope
  hive-mcp.crystal.chain-test/prev-of
  {:golden-path "test/golden/hive-mcp/crystal/chain-previous-summary.edn"
   :cases {:none        [(sid 10) "a" []]
           :newest-of-2 [(sid 10) "a" [{:id (sid 1) :project-id "a" :tags ["session-summary"]}
                                       {:id (sid 5) :project-id "a" :tags ["session-summary"]}]]
           :other-scope [(sid 10) "a" [{:id (sid 1) :project-id "a" :tags ["session-summary"]}
                                       {:id (sid 7) :project-id "b" :tags ["session-summary"]}]]
           :by-tag      [(sid 10) "a" [{:id (sid 3) :tags ["session-summary" "scope:project:a"]}]]
           :not-summary [(sid 10) "a" [{:id (sid 2) :project-id "a" :tags ["session-summary"]}
                                       {:id (sid 8) :project-id "a" :tags ["decision"]}]]
           :self-newer  [(sid 10) "a" [{:id (sid 10) :project-id "a" :tags ["session-summary"]}
                                       {:id (sid 12) :project-id "a" :tags ["session-summary"]}
                                       {:id (sid 4) :project-id "a" :tags ["session-summary"]}]]}
   :gen (gen/tuple (gen/fmap sid (gen/choose 0 40))
                   (gen/elements ["a" "b"])
                   (gen/vector gen-candidate 0 8))
   :pred (fn [{[new-id pid candidates] :in out :out}]
           (let [ok (eligible new-id pid candidates)]
             (if (empty? ok)
               (nil? out)
               (= out (last (sort ok))))))
   :num-tests 200
   :mutations [["ignores-scope"
                (fn [[new-id _ candidates :as input]]
                  {:in input
                   :out (->> candidates
                             (filter #(and (some #{"session-summary"} (:tags %))
                                           (neg? (compare (:id %) new-id))))
                             (map :id) sort last)})]
               ["takes-oldest"
                (fn [[new-id pid candidates :as input]]
                  {:in input :out (first (sort (eligible new-id pid candidates)))})]
               ["includes-self-and-newer"
                (fn [[_ pid candidates :as input]]
                  {:in input
                   :out (->> candidates
                             (filter #(and (some #{"session-summary"} (:tags %))
                                           (= pid (:project-id %))))
                             (map :id) sort last)})]]})

;; =============================================================================
;; chain-wraps! over a stub port
;; =============================================================================

(defn- recording-chain
  "ISummaryChain stub: serves `by-pid` candidates, records links, throws for
   project ids in `fail-pids`."
  [by-pid links fail-pids]
  (reify chain/ISummaryChain
    (-prior-summaries [_ pid]
      (when (contains? fail-pids pid) (throw (ex-info "store down" {:pid pid})))
      (get by-pid pid []))
    (-link! [_ from to pid] (swap! links conj [from to pid]) :edge-id)))

(deftest chain-wraps-links-each-successful-result
  (let [links (atom [])
        c     (recording-chain {"a" [{:id (sid 1) :project-id "a" :tags ["session-summary"]}
                                     {:id (sid 9) :project-id "a" :tags ["session-summary"]}]
                                "b" []}
                               links #{"down"})
        report (chain/chain-wraps!
                c {:results [{:pid "a" :project-id "a" :id (sid 9) :success? true}
                             {:pid "b" :project-id "b" :id (sid 9) :success? true}
                             {:pid "c" :project-id "c" :id nil :success? false}
                             {:pid "down" :project-id "down" :id (sid 9) :success? true}]})]
    (testing "a links to its own predecessor, never to itself"
      (is (= [[(sid 9) (sid 1) "a"]] @links)))
    (testing "report counts and per-result shape"
      (is (= 1 (:linked report)))
      (is (= 1 (:failed report)))
      (is (= [(sid 1) nil nil nil] (mapv :supersedes (:results report))))
      (is (true? (:skipped (nth (:results report) 2))))
      (is (= "store down" (:error (nth (:results report) 3)))))))

(deftest chain-wraps-tolerates-an-empty-report
  (is (= {:linked 0 :failed 0 :results []}
         (chain/chain-wraps! (recording-chain {} (atom []) #{}) {}))))

;; =============================================================================
;; StoreChain over hive-spi's reference store
;; =============================================================================

(deftest store-chain-reads-summaries-of-the-project-and-writes-supersedes
  (let [store (stub/create-store)
        edges (atom [])
        add!  (fn [spec] (swap! edges conj spec) "edge-1")
        c     (chain/->StoreChain store add! nil)]
    (doseq [e [{:id (sid 1) :type "note" :project-id "a" :tags ["session-summary" "scope:project:a"] :content "s1"}
               {:id (sid 2) :type "note" :project-id "b" :tags ["session-summary" "scope:project:b"] :content "s2"}
               {:id (sid 3) :type "decision" :project-id "a" :tags ["session-summary"] :content "d"}
               {:id (sid 4) :type "note" :project-id "a" :tags ["scope:project:a"] :content "n"}
               {:id (sid 5) :type "note" :project-id "a" :tags ["session-summary" "scope:project:a"] :content "s5"}]]
      (ports/add-entry! store e))
    (testing "only project a's session-summary notes come back"
      (is (= #{(sid 1) (sid 5)} (set (map :id (chain/-prior-summaries c "a"))))))
    (let [report (chain/chain-wraps! c {:results [{:pid "a" :project-id "a" :id (sid 5) :success? true}]})]
      (is (= 1 (:linked report)))
      (is (= [{:from (sid 5) :to (sid 1) :relation :supersedes :scope "a"
               :created-by "wrap-chain" :source-type :automated}]
             @edges)))))
