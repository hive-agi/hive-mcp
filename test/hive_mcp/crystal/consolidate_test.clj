(ns hive-mcp.crystal.consolidate-test
  "Same-day consolidation of wrap summaries (SYNTHESIS-HISTORY slice (b)).

   Three layers, mirroring `hive-mcp.crystal.chain-test`:

     1. `day-of` and `existing-for-day` each get a trifecta: golden cases, a
        200-sample property against an ORACLE written independently of the
        implementation, and mutants that must be killed.
     2. `folds-for` / `apply-folds` run over a recording `ISummaryChain` port,
        so the pipeline is proven with no live store and no `with-redefs`.
     3. The boundary is exercised against hive-spi's reference store: a
        same-day re-wrap must leave ONE entry carrying the SAME id, which is
        what preserves the `:supersedes` edge slice (a) wrote."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.crystal.chain :as chain]
            [hive-mcp.crystal.consolidate :as cons]
            [hive-mcp.crystal.persist :as persist]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-spi.memory.ports :as ports]
            [hive-spi.memory.stub :as stub]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.test.stub.memory-store :as obs]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- sid
  "An entry id in the shape the memory store hands out: `yyyyMMdd` + `HHmmss`
   + a uniqueness suffix. This is why id order is creation order."
  [day hhmmss n]
  (format "%s%s-%08d" day hhmmss n))

(defn- dtag [d] (str "session:" d))

(def ^:private day-from-id
  "Test-side re-derivation of the id prefix as a day, used only by oracles."
  (fn [id] (when (and (string? id) (>= (count id) 8)
                      (re-matches #"\d{8}" (subs id 0 8)))
              (let [d (subs id 0 8)]
                (str (subs d 0 4) "-" (subs d 4 6) "-" (subs d 6 8))))))

;; =============================================================================
;; day-of
;; =============================================================================

(defn day-of-of
  "Unary port over `day-of` for the trifecta: one entry in, {:in :out} out."
  [entry]
  {:in entry :out (cons/day-of entry)})

(defn- day-oracle
  "Independent day read: the first `session:` tag that holds a date, else the
   id prefix. Written as two separate walks rather than one early-exit chain,
   so it does not mirror the implementation's shape."
  [entry]
  (let [tagged (keep (fn [t]
                       (when (and (string? t) (str/starts-with? t "session:"))
                         (let [v (subs t 8)]
                           (when (re-matches #"\d{4}-\d{2}-\d{2}" v) v))))
                     (:tags entry))]
    (if (seq tagged) (first tagged) (day-from-id (:id entry)))))

(def ^:private gen-probe-entry
  "Entries covering every branch: a day tag, a uid tag (which is not a day), a
   contradicting id, a malformed tag, and no evidence at all."
  (gen/let [tag-day (gen/elements [nil "2026-11-17" "2026-12-01"])
            uid?    gen/boolean
            id      (gen/elements [nil "20261117120000-00000001"
                                   "20260101120000-00000002" "2026"
                                   "garbage" "2026111712"])
            extra   (gen/elements [nil "session:not-a-date" "other"])]
    (cond-> {}
      (or tag-day uid? extra)
      (assoc :tags (vec (remove nil? [(when tag-day (dtag tag-day))
                                      (when uid? "session-uid:abc123")
                                      extra])))
      id (assoc :id id))))

(deftrifecta day-of-reads-the-local-day
  hive-mcp.crystal.consolidate-test/day-of-of
  {:golden-path "test/golden/hive-mcp/crystal/consolidate-day-of.edn"
   :cases {:day-tag         {:tags [(dtag "2026-11-17")]}
           :uid-tag-ignored {:tags ["session-uid:abc123"]}
           :tag-beats-id    {:id "20260101120000-00000001" :tags [(dtag "2026-11-17")]}
           :id-fallback     {:id "20261117120000-00000001"}
           :short-id        {:id "2026"}
           :no-evidence     {}
           :nil-tags        {:id "20261117120000-00000001" :tags nil}
           :malformed-tag   {:id "20260101120000-00000001" :tags ["session:not-a-date"]}}
   :gen gen-probe-entry
   :pred (fn [{entry :in out :out}] (= out (day-oracle entry)))
   :num-tests 200
   :mutations [["always-nil" (fn [entry] {:in entry :out nil})]
               ["reads-uid-as-a-day"
                (fn [entry]
                  {:in entry
                   :out (some (fn [t] (when (and (string? t) (str/starts-with? t "session-uid:"))
                                        (subs t 12)))
                              (:tags entry))})]
               ["id-beats-tag"
                (fn [entry] {:in entry :out (day-from-id (:id entry))})]]})

;; =============================================================================
;; existing-for-day
;; =============================================================================

(defn existing-of
  "Unary port over `existing-for-day` for the trifecta: one
   [pid today candidates] vector in, {:in :out} out. Unary (not :apply?) so the
   golden, property and mutation facets all reach the subject the same way."
  [[pid today candidates :as input]]
  {:in input :out (cons/existing-for-day pid today candidates)})

(defn- eligible
  "Oracle: the ids `existing-for-day` may answer with. Scope is an explicit `or`
   over the two places a scope can live, and the day is `day-oracle`, so this
   shares no decision with the implementation."
  [pid today candidates]
  (set (for [{:keys [id tags project-id]} candidates
             :when (and (string? id)
                        (not (str/blank? id))
                        (some #{"session-summary"} tags)
                        (or (= pid project-id)
                            (some #{(str "scope:project:" pid)} tags))
                        (= today (day-oracle {:id id :tags tags})))]
         id)))

(def ^:private gen-candidate
  (gen/let [day   (gen/elements ["2026-11-17" "2026-11-16" "2026-12-01"])
            n     (gen/choose 0 40)
            pid   (gen/elements ["a" "b"])
            scope-where (gen/elements [:project-id :tag :none])
            summary? gen/boolean]
    (cond-> {:id (sid "20261117" (format "%06d" n) n)
             :tags (cond-> [(dtag day)]
                     summary? (conj "session-summary")
                     (= scope-where :tag) (conj (str "scope:project:" pid)))}
      (= scope-where :project-id) (assoc :project-id pid))))

(deftrifecta existing-for-day-picks-todays-newest
  hive-mcp.crystal.consolidate-test/existing-of
  {:golden-path "test/golden/hive-mcp/crystal/consolidate-existing-for-day.edn"
   :cases
   {:none             ["a" "2026-11-17" []]
    :same-day         ["a" "2026-11-17" [{:id (sid "20261117" "090000" 1) :project-id "a"
                                           :tags ["session-summary" (dtag "2026-11-17")]}]]
    :newest-of-two    ["a" "2026-11-17" [{:id (sid "20261117" "090000" 1) :project-id "a"
                                           :tags ["session-summary" (dtag "2026-11-17")]}
                                          {:id (sid "20261117" "150000" 2) :project-id "a"
                                           :tags ["session-summary" (dtag "2026-11-17")]}]]
    :yesterday-only   ["a" "2026-11-17" [{:id (sid "20261116" "090000" 1) :project-id "a"
                                           :tags ["session-summary" (dtag "2026-11-16")]}]]
    :other-scope      ["a" "2026-11-17" [{:id (sid "20261117" "090000" 1) :project-id "b"
                                           :tags ["session-summary" (dtag "2026-11-17")]}]]
    :by-scope-tag     ["a" "2026-11-17" [{:id (sid "20261117" "090000" 1)
                                           :tags ["session-summary" (dtag "2026-11-17")
                                                  "scope:project:a"]}]]
    :not-a-summary    ["a" "2026-11-17" [{:id (sid "20261117" "090000" 1) :project-id "a"
                                           :tags ["decision" (dtag "2026-11-17")]}
                                          {:id (sid "20261117" "070000" 3) :project-id "a"
                                           :tags ["session-summary" (dtag "2026-11-17")]}]]
    :day-from-id-only ["a" "2026-11-17" [{:id (sid "20261117" "090000" 1) :project-id "a"
                                           :tags ["session-summary"]}]]
    :umbrella-scope   ["multi-project" "2026-11-17"
                       [{:id (sid "20261117" "090000" 1) :project-id "multi-project"
                         :tags ["session-summary" (dtag "2026-11-17") "umbrella"]}]]}
   :gen (gen/tuple (gen/elements ["a" "b"])
                   (gen/elements ["2026-11-17" "2026-11-16"])
                   (gen/vector gen-candidate 0 8))
   :pred (fn [{:keys [in out]}]
           (let [[pid today candidates] in
                 ok (eligible pid today candidates)]
             (if (empty? ok) (nil? out) (= out (last (sort ok))))))
   :num-tests 200
   :mutations
   [["ignores-the-day"
     (fn [[pid _today candidates :as input]]
       {:in input
        :out (->> candidates
                  (filter #(and (some #{"session-summary"} (:tags %))
                                (= pid (:project-id %))))
                  (map :id) sort last)})]
    ["takes-the-oldest"
     (fn [[pid today candidates :as input]]
       {:in input :out (first (sort (eligible pid today candidates)))})]
    ["ignores-the-summary-tag"
     (fn [[pid today candidates :as input]]
       {:in input
        :out (->> candidates
                  (filter #(or (= pid (:project-id %))
                               (some #{(str "scope:project:" pid)} (:tags %))))
                  (filter #(= today (day-oracle %)))
                  (map :id) sort last)})]]})

;; =============================================================================
;; Pipeline over a recording port (no store, no with-redefs)
;; =============================================================================

(defn- recording-chain
  "`ISummaryChain` stub: serves `by-pid` candidates, records the project ids it
   was asked for, and throws for the ids in `fail-pids`."
  [by-pid asked fail-pids]
  (reify chain/ISummaryChain
    (-prior-summaries [_ pid]
      (swap! asked conj pid)
      (when (contains? fail-pids pid)
        (throw (ex-info "store down" {:pid pid})))
      (get by-pid pid []))
    (-link! [_ _ _ _] :edge-id)))

(def ^:private today-summary-a
  {:id (sid "20261117" "090000" 1) :project-id "a"
   :tags ["session-summary" (dtag "2026-11-17")]})

(deftest folds-for-answers-one-fold-per-project
  (let [asked (atom [])
        c (recording-chain {"a" [today-summary-a]} asked #{})
        wraps [{:pid "a" :entry {:content "one"}}
               {:pid "a" :entry {:content "two"}}
               {:pid "b" :entry {:content "three"}}]
        report (cons/folds-for c wraps "2026-11-17")]
    (testing "one read per distinct project, not per wrap"
      (is (= #{"a" "b"} (set @asked)))
      (is (= 2 (count @asked))))
    (testing "only the project with a summary stored today folds"
      (is (= {:folds {"a" (:id today-summary-a)} :failed 0 :scanned 2}
             report)))))

(deftest folds-for-never-throws-and-folds-nothing-on-a-read-failure
  (let [c (recording-chain {} (atom []) #{"down"})
        report (cons/folds-for c [{:pid "down" :entry {:content "x"}}
                                  {:pid "ok" :entry {:content "y"}}]
                              "2026-11-17")]
    (is (= {} (:folds report)))
    (is (= 1 (:failed report)))
    (is (= 2 (:scanned report)))))

(deftest folds-for-keys-the-umbrella-by-its-store-project-id
  (let [c (recording-chain {"multi-project" [{:id (sid "20261117" "090000" 1)
                                               :project-id "multi-project"
                                               :tags ["session-summary" (dtag "2026-11-17")]}]}
                           (atom []) #{})
        wraps [{:pid :umbrella :entry {:content "cross-cutting"}}]
        id (sid "20261117" "090000" 1)]
    (testing "the sentinel pid is keyed as persist writes it"
      (is (= {:folds {"multi-project" id} :failed 0 :scanned 1}
             (cons/folds-for c wraps "2026-11-17"))))
    (testing "apply-folds stamps the marker on that same sentinel wrap"
      (is (= [id] (mapv :consolidate-into (cons/apply-folds wraps {"multi-project" id})))))))

(deftest folds-for-skips-wraps-with-no-project
  (is (= {:folds {} :failed 0 :scanned 0}
         (cons/folds-for (recording-chain {} (atom []) #{})
                         [{:pid "" :entry {}} {:pid nil :entry {}}]
                         "2026-11-17"))))

(deftest apply-folds-leaves-unfolded-wraps-untouched
  (let [wraps [{:pid "a" :entry {:content "one"}}
               {:pid "b" :entry {:content "two"}}]]
    (testing "an empty fold map is today's behaviour, entry for entry"
      (is (= wraps (cons/apply-folds wraps {}))))
    (testing "only the folded project is marked"
      (is (= ["id-a" nil]
             (mapv :consolidate-into (cons/apply-folds wraps {"a" "id-a"})))))))

;; =============================================================================
;; Boundary: the fold against hive-spi's reference store
;; =============================================================================

(defn- with-store [store f]
  (let [before (mem-proto/registered-stores)]
    (mem-proto/reset-registry!)
    (try
      (when store (mem-proto/register-store! :default store))
      (f)
      (finally
        (mem-proto/reset-registry!)
        (doseq [[k s] before] (mem-proto/register-store! k s))))))

(def ^:private am-wrap
  {:id (sid "20261117" "090000" 1) :type "note" :project-id "a"
   :tags ["session-summary" "wrap-generated" (dtag "2026-11-17") "scope:project:a"]
   :content "morning wrap" :duration "short"})

(defn- w [content day]
  {:pid "a" :entry {:type :note :content content
                     :tags ["session-summary" (dtag day)]
                     :duration :short}})

(deftest a-same-day-rewrap-rewrites-the-summary-in-place
  (let [store (stub/create-store)]
    (ports/add-entry! store am-wrap)
    (let [c (chain/->StoreChain store (fn [_] "edge-id") nil)
          wraps [(w "afternoon wrap" "2026-11-17")]
          planned (cons/apply-folds wraps (:folds (cons/folds-for c wraps "2026-11-17")))
          report (with-store store #(persist/persist-wraps! planned))
          after (ports/query-entries store {})]
      (testing "one entry for the day, not two"
        (is (= 1 (count after))))
      (testing "the surviving entry keeps the id the :supersedes edge hangs off"
        (is (= [(:id am-wrap)] (mapv :id after)))
        (is (true? (:upgraded? (first (:results report))))))
      (testing "its content is the newer wrap's"
        (is (= "afternoon wrap" (:content (first after))))))))

(deftest a-second-rewrap-in-the-same-day-folds-again
  (let [store (stub/create-store)]
    (ports/add-entry! store am-wrap)
    (let [c (chain/->StoreChain store (fn [_] "edge-id") nil)
          fold! (fn [wraps today]
                  (with-store store
                    #(persist/persist-wraps!
                      (cons/apply-folds wraps (:folds (cons/folds-for c wraps today))))))
          _ (fold! [(w "evening wrap" "2026-11-17")] "2026-11-17")
          report (fold! [(w "late wrap" "2026-11-17")] "2026-11-17")]
      (testing "still ONE entry after a third wrap of the day"
        (is (= 1 (:total report)))
        (is (= 1 (count (ports/query-entries store {})))))
      (testing "and the same id is still the one being rewritten"
        (is (= [(:id am-wrap)] (mapv :id (ports/query-entries store {}))))))))

(deftest a-rewrap-on-another-day-adds-and-does-not-fold
  (let [store (stub/create-store)]
    (ports/add-entry! store am-wrap)
    (let [c (chain/->StoreChain store (fn [_] "edge-id") nil)
          wraps [(w "next day" "2026-11-18")]
          folds (:folds (cons/folds-for c wraps "2026-11-18"))
          report (with-store store #(persist/persist-wraps! (cons/apply-folds wraps folds)))]
      (testing "nothing folds, so the wrap is ADDED"
        (is (= {} folds))
        (is (= 1 (:total report)))
        (is (nil? (:upgraded? (first (:results report))))))
      (testing "and the store holds both days"
        (is (= 2 (count (ports/query-entries store {}))))))))

(deftest an-empty-store-degrades-to-a-fresh-add
  (let [store (stub/create-store)
        c (chain/->StoreChain store (fn [_] "edge-id") nil)
        wraps [(w "first ever" "2026-11-17")]
        folds (:folds (cons/folds-for c wraps "2026-11-17"))]
    (testing "nothing folds against an empty store"
      (is (= {} folds)))
    (testing "and the wrap is written, never dropped"
      (let [report (with-store store #(persist/persist-wraps! (cons/apply-folds wraps folds)))]
        (is (= 1 (:persisted report)))
        (is (= 1 (count (ports/query-entries store {}))))))))

;; =============================================================================
;; Fold x chain: a folded re-wrap must not re-link (reviewer defect 1)
;; =============================================================================

(defn- recording-kg
  "A KG edge port over an atom: `add!` appends the spec and answers a fresh id
   (as knowledge-graph.edges.write/add-edge! does, with no uniqueness on
   from/to/relation), `find` answers the first stored edge matching all three."
  [edges]
  {:add!  (fn [spec] (swap! edges conj spec) (str "edge-" (count @edges)))
   :find  (fn [from to relation]
            (some #(when (and (= from (:from %)) (= to (:to %)) (= relation (:relation %))) %)
                  @edges))})

(def ^:private yesterday-summary
  {:id (sid "20261116" "090000" 1) :type "note" :project-id "a"
   :tags ["session-summary" "wrap-generated" (dtag "2026-11-16") "scope:project:a"]
   :content "yesterday" :duration "short"})

(deftest two-same-day-wraps-write-exactly-one-supersedes-edge
  (let [store (stub/create-store)
        edges (atom [])
        {:keys [add! find]} (recording-kg edges)
        c (chain/->StoreChain store add! find)
        wrap! (fn [content]
                (let [wraps [(w content "2026-11-17")]
                      planned (cons/apply-folds wraps (:folds (cons/folds-for c wraps "2026-11-17")))
                      persisted (with-store store #(persist/persist-wraps! planned))]
                  {:persisted persisted :chained (chain/chain-wraps! c persisted)}))]
    (ports/add-entry! store yesterday-summary)
    (let [first-wrap (wrap! "morning wrap")
          today-id (:id (first (:results (:persisted first-wrap))))
          second-wrap (wrap! "afternoon wrap")]
      (testing "the first wrap of the day is added and linked to yesterday"
        (is (not= (:id yesterday-summary) today-id))
        (is (= 1 (:linked (:chained first-wrap)))))
      (testing "the second wrap folds into the same entry"
        (is (true? (:upgraded? (first (:results (:persisted second-wrap))))))
        (is (= today-id (:id (first (:results (:persisted second-wrap)))))))
      (testing "and the chain reports the existing edge without writing another"
        (is (= 0 (:linked (:chained second-wrap))))
        (is (true? (:existing (first (:results (:chained second-wrap))))))
        (is (= (:id yesterday-summary) (:supersedes (first (:results (:chained second-wrap)))))))
      (testing "exactly ONE :supersedes edge, today -> yesterday"
        (is (= [[today-id (:id yesterday-summary) :supersedes]]
               (mapv (juxt :from :to :relation) @edges))))
      (testing "two entries in the store: yesterday and today's single summary"
        (is (= 2 (count (ports/query-entries store {}))))))))

;; =============================================================================
;; A throwing update must not lose the wrap (reviewer defect 2)
;; =============================================================================

(deftest a-throwing-update-falls-back-to-a-fresh-add
  (let [inner (stub/create-store)
        _ (ports/add-entry! inner am-wrap)
        store (obs/->observing inner {:update-entry! "store down"})
        c (chain/->StoreChain store (fn [_] "edge-id") nil)
        wraps [(w "afternoon wrap" "2026-11-17")]
        planned (cons/apply-folds wraps (:folds (cons/folds-for c wraps "2026-11-17")))
        report (with-store store #(persist/persist-wraps! planned))
        result (first (:results report))
        after (ports/query-entries inner {})]
    (testing "the fold was planned and the update was attempted through the port"
      (is (= [(:id am-wrap)] (mapv :consolidate-into planned)))
      (is (= 1 (count (obs/calls-of store :update-entry!)))))
    (testing "the throw is not reported as an upgrade of the old id"
      (is (not (true? (:upgraded? result))))
      (is (not= (:id am-wrap) (:id result)))
      (is (= (:id am-wrap) (:upgraded-from result))))
    (testing "the wrap is ADDED instead, so it is not lost"
      (is (true? (:success? result)))
      (is (string? (:id result)))
      (is (= 1 (:persisted report)))
      (is (= 1 (count (obs/calls-of store :add-entry!))))
      (is (= 2 (count after)))
      (is (= "afternoon wrap" (:content (ports/get-entry inner (:id result))))))
    (testing "the stored morning wrap is untouched"
      (is (= "morning wrap" (:content (ports/get-entry inner (:id am-wrap))))))))
