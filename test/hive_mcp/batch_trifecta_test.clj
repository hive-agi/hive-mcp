(ns hive-mcp.batch-trifecta-test
  "Trifecta for the batch runner (`hive-mcp.batch`).

   Property + mutation facets are SYNTHESIZED from the malli contracts by
   hive-schemas.test (`deftrifecta-from-schema`); the golden pins the seeded
   cases. With no `:bx/*` extension registered the runner falls back to the
   in-core Kahn sort and the pure validators, which is the deterministic
   behaviour pinned here. Handlers are plain functions injected through the
   runner's `:resolve-handler` port: no redefs."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.batch :as batch]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Narrowed op vocabulary: a few ids so that dependency edges actually land.
(def ^:private OpId [:enum "a" "b" "c" "d"])

(def ^:private SmallOps
  "Well-formed ops over a tiny id space, dependencies naming other ids."
  [:vector {:max 4
            :gen/fmap (fn [ops] (vec (vals (into (sorted-map) (map (juxt :id identity)) ops))))}
   [:map
    [:id OpId]
    [:tool [:enum "memory" "kg" "kanban"]]
    [:command {:optional true} [:enum "add" "query"]]
    [:depends_on {:optional true} [:vector {:max 2} OpId]]]])

;; =============================================================================
;; Schema-synthesized facets
;; =============================================================================

(hst/deftrifecta-from-schema normalize-op-contract
  hive-mcp.batch/normalize-op
  {:in        [:map
               [:id [:enum "a" "$0" "op-1"]]
               [:tool [:enum "memory" "kg"]]
               [:depends_on {:optional true} [:or [:enum "a" "b"] [:vector [:enum "a" "b"]]]]]
   :out       batch/NormalizedOp
   :rel       (fn [in out]
                (and (= (:id in) (:id out))
                     ;; deps are always coerced to a vector
                     (or (not (contains? in :depends_on)) (vector? (:depends_on out)))))
   :idempotent? true
   :num-tests 200
   :golden-path "test/golden/batch/normalize-op.edn"})

(hst/deftrifecta-from-schema validate-ops-contract
  hive-mcp.batch/validate-ops
  {:in        [:cat SmallOps]
   :out       batch/Validation
   :rel       (fn [[ops] {:keys [valid errors]}]
                ;; a verdict of valid carries no errors; an invalid one carries some
                (if valid (nil? errors) (seq errors)))
   :num-tests 200
   :golden-path "test/golden/batch/validate-ops.edn"})

(hst/deftrifecta-from-schema assign-waves-contract
  hive-mcp.batch/assign-waves
  {:in        [:cat [:fn {:gen/elements [[{:id "a" :tool "t"}
                                          {:id "b" :tool "t" :depends_on ["a"]}
                                          {:id "c" :tool "t" :depends_on ["a" "b"]}]
                                         [{:id "x" :tool "t"} {:id "y" :tool "t"}]
                                         []]}
                      vector?]]
   :out       [:sequential [:map [:wave :int]]]
   :rel       (fn [[ops] waved]
                (let [wave-of (into {} (map (juxt :id :wave)) waved)]
                  ;; a dependent lands in a STRICTLY later wave than each dependency
                  (and (= (count ops) (count waved))
                       (every? (fn [{:keys [id depends_on]}]
                                 (every? #(< (wave-of %) (wave-of id)) depends_on))
                               ops))))
   :num-tests 30})

;; =============================================================================
;; run-operations / execute-op through the injected handler port
;; =============================================================================

(defn- ok-handler [_tool] (fn [op] {:echo (:command op)}))

(defn run-batch
  "Unary adapter: ops -> the runner's answer, handlers always succeed."
  [ops]
  (batch/run-operations ops {:resolve-handler ok-handler}))

(hst/deftrifecta-from-schema run-operations-contract
  hive-mcp.batch-trifecta-test/run-batch
  {:in        SmallOps
   :out       batch/RunResult
   :rel       (fn [ops {:keys [success summary errors]}]
                (cond
                  errors                  (and (false? success) (zero? (:success summary)))
                  :else                   (and (true? success)
                                               (= (count ops) (:total summary) (:success summary))
                                               (zero? (:failed summary)))))
   :num-tests 100
   :golden-path "test/golden/batch/run-operations.edn"})

(deftest execute-op-isolates-errors-test
  (testing "a missing handler is a failed result, not a throw"
    (let [r (batch/execute-op (constantly nil) {:id "a" :tool "nope" :command "x"})]
      (is (false? (:success r)))
      (is (= "Tool not found: nope" (:error r)))
      (is (m/validate batch/OpResult r))))
  (testing "a throwing handler is isolated into :error"
    (let [r (batch/execute-op (constantly (fn [_] (throw (ex-info "boom" {}))))
                              {:id "a" :tool "t" :command :x})]
      (is (false? (:success r)))
      (is (= "boom" (:error r)))))
  (testing "plumbing keys never reach the handler; a keyword :command arrives as a name"
    (let [seen (atom nil)
          r    (batch/execute-op (constantly (fn [op] (reset! seen op) :done))
                                 {:id "a" :tool "t" :command :go :depends_on ["z"] :wave 3 :q 1})]
      (is (true? (:success r)))
      (is (= {:command "go" :q 1} @seen)))))

(deftest run-operations-arms-test
  (testing "the validation-refusal arm validates against RunResult and reports :errors"
    (let [r (batch/run-operations [{:id "a"}] {:resolve-handler ok-handler})]
      (is (m/validate batch/RunResult r))
      (is (false? (:success r)))
      (is (seq (:errors r)))))
  (testing "the dry-run arm plans without executing"
    (let [called (atom 0)
          r (batch/run-operations [{:id "a" :tool "t" :command "x"}]
                                  {:resolve-handler (fn [_] (swap! called inc) (fn [_] nil))
                                   :dry-run? true})]
      (is (m/validate batch/RunResult r))
      (is (true? (:dry-run r)))
      (is (zero? @called))))
  (testing "the executed arm counts a failing op as failed, and skips its dependents"
    (let [r (batch/run-operations
             [{:id "a" :tool "bad" :command "x"}
              {:id "b" :tool "good" :command "y" :depends_on ["a"]}]
             {:resolve-handler (fn [tool] (when (= tool "good") (fn [_] :fine)))})]
      (is (m/validate batch/RunResult r))
      (is (false? (:success r)))
      (is (= {:total 2 :success 0 :failed 2 :waves 2} (:summary r)))))
  (testing "an emit-fx hook sees one wave-complete per wave"
    (let [fx (atom [])]
      (batch/run-operations [{:id "a" :tool "t" :command "x"}
                             {:id "b" :tool "t" :command "x" :depends_on ["a"]}]
                            {:resolve-handler ok-handler
                             :emit-fx (fn [id _] (swap! fx conj id))})
      (is (= [:multi/wave-complete :multi/wave-complete] @fx))))
  (testing "a missing :resolve-handler is refused loudly (the :assert, or the contract
            when instrumented), never a silent no-op"
    (is (thrown? Throwable (batch/run-operations [] {})))))

(deftest enrich-op-result-only-downgrades-test
  (testing "a degraded creation result flips success false with the classifier's message"
    (let [r (batch/enrich-op-result {:tool "memory" :success true :result {:id nil}})]
      (is (false? (:success r)))
      (is (string? (:error r)))))
  (testing "an op that already failed keeps its own error and is never upgraded"
    (let [r (batch/enrich-op-result {:tool "x" :success false :error "e"})]
      (is (false? (:success r)))
      (is (= "e" (:error r)))))
  (testing "a healthy result passes through with :data attached"
    (let [r (batch/enrich-op-result {:tool "memory" :success true :result {:id "abc"}})]
      (is (true? (:success r)))
      (is (contains? r :data)))))

(deftest ref-helpers-without-extensions-test
  (testing "with no :bx/* extension the delegating helpers answer their defaults"
    (is (true? (batch/ref? "$ref:a.data.id")))
    (is (false? (batch/ref? "plain")))
    (is (false? (batch/ref? nil)))
    (is (nil? (batch/parse-ref "$ref:a.data.id")))
    (is (= #{} (batch/collect-ref-op-ids {:id "a"})))
    (is (= batch/ref-not-found (batch/resolve-ref nil {})))
    (is (= {:a 1} (batch/resolve-refs-in-value {:a 1} {})))
    (is (= {:id "a"} (batch/resolve-op-refs {:id "a"} {})))
    (is (= {:a 1} (batch/extract-result-data {:a 1})))))

(deftest runner-record-test
  (let [runner (batch/make-default-runner {:resolve-handler ok-handler})]
    (testing "make-default-runner yields the declared record"
      (is (m/validate batch/DefaultRunner runner))
      (is (m/validate batch/DefaultRunner (batch/make-default-runner nil))))
    (testing "the baked-in handler drives a run through the Batchable port"
      (let [r ((requiring-resolve 'hive-mcp.batch.protocol/batch-execute)
               runner [{:id "a" :tool "t" :command "x"}] {})]
        (is (true? (:success r)))))))

(deftest schemas-generate-test
  (doseq [s [batch/Op batch/NormalizedOp batch/ResolveHandler batch/EmitFx batch/OpResult
             batch/Validation batch/RunOpts batch/Summary batch/RunResult
             batch/RunnerConfig batch/DefaultRunner]]
    (is (seq (mg/sample s {:size 5})) (str "generates: " (pr-str s)))))
