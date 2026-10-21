(ns hive-mcp.saa.foundation-contract-test
  "Contract facets for the SAA foundation: the phase model, phase-prompt
   construction, the raw-message lift, the default record constructors and
   the :saa/core seed. Seeded registry state is owned per test through
   support/with-fresh-registry."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.protocols.saa :as psaa]
            [hive-mcp.saa.adapters :as adapters]
            [hive-mcp.saa.core-seed :as core-seed]
            [hive-mcp.saa.model :as model]
            [hive-mcp.saa.planner :as planner]
            [hive-mcp.saa.prompt :as prompt]
            [hive-mcp.saa.scorer :as scorer]
            [hive-mcp.saa.support :as support]
            [hive-mcp.saa.types :as types]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each support/with-fresh-registry)

;;; ---------------------------------------------------------------------------
;;; model
;;; ---------------------------------------------------------------------------

(def ^:private PhaseProbe
  "Known phases, near-misses and nil — both arms of every model lookup."
  [:enum :silence :abstract :act :observe "silence" nil :act/x])

(deftrifecta-from-schema descriptor-matches-its-phase
  #'model/phase-descriptor
  {:in [:cat PhaseProbe]
   :out [:maybe model/PhaseDescriptor]
   :rel (fn [[p] out]
          (if (contains? #{:silence :abstract :act} p)
            (= p (:phase out))
            (nil? out)))
   :classify (fn [_ out] (if out :found :missing))
   :classify-domain #{:found :missing}
   :num-tests 100})

(deftrifecta-from-schema intent-is-the-descriptor-slice
  #'model/phase->intent
  {:in [:cat PhaseProbe]
   :out [:maybe model/PhaseIntent]
   :rel (fn [[p] out]
          (= out (some-> (model/phase-descriptor p)
                         (select-keys [:tool-intent :permission-intent]))))
   :golden-path "test/golden/hive-mcp/saa/phase-intent.edn"
   :num-tests 100})

(deftrifecta-from-schema phases-are-canonical
  #'model/ordered-phases
  {:in [:cat]
   :out [:vector model/SaaPhase]
   :rel (fn [_ out] (= out [:silence :abstract :act]))
   :num-tests 5})

;;; ---------------------------------------------------------------------------
;;; prompt
;;; ---------------------------------------------------------------------------

(def ^:private Content [:or :nil [:string {:min 1 :max 12}] [:map-of :keyword :int]])

(deftrifecta-from-schema prompt-leads-with-the-phase-fragment
  #'prompt/build-phase-prompt*
  {:in [:cat model/SaaPhase Content [:maybe [:string {:min 1 :max 12}]]]
   :out :string
   :rel (fn [[phase content extra] out]
          (and (str/starts-with? out (:goal-prompt-fragment (model/phase-descriptor phase)))
               (if (and (string? content) (not= :abstract phase))
                 (str/includes? out content)
                 true)
               (if extra (str/includes? out extra) true)))
   :golden-path "test/golden/hive-mcp/saa/phase-prompt.edn"
   :num-tests 100})

(deftrifecta-from-schema prompt-without-a-fragment-is-the-body
  #'prompt/build-phase-prompt
  {:in [:cat model/SaaPhase [:maybe [:string {:min 1 :max 12}]] :nil
        [:enum [] [{:phase :silence}] [{:phase :other :goal-prompt-fragment "F"}]]]
   :out :string
   ;; none of these models carries a fragment for the generated phase, so the
   ;; prompt is exactly the body the default model appends after its fragment
   :rel (fn [[phase content extra _] out]
          (= (str (:goal-prompt-fragment (model/phase-descriptor phase)) "\n\n" out)
             (prompt/build-phase-prompt* phase content extra)))
   :num-tests 100})

(deftest prompt-rejects-an-unknown-phase
  ;; the body's case has no default branch (IllegalArgumentException); under
  ;; instrumentation the SaaPhase input contract rejects it first
  (is (thrown? Exception (prompt/build-phase-prompt* :observe "x" nil))))

;;; ---------------------------------------------------------------------------
;;; types — raw bridge message lift
;;; ---------------------------------------------------------------------------

(def ^:private RawProbe
  ;; :type is drawn so every lifted variant is reached comfortably: a
  ;; uniform pick over the four variants first, then a key that lifts to it.
  ;; The old weighted :enum, thinned by :optional, left :pm/error and
  ;; :pm/phase-complete near 13 of 200 against a floor of 12.
  [:map {:gen/schema [:tuple
                      [:map
                       [:phase {:optional true} [:enum :silence :act]]
                       [:saa-phase {:optional true} [:enum :abstract]]
                       [:content {:optional true} [:string {:max 4}]]
                       [:data {:optional true} [:string {:max 4}]]
                       [:error {:optional true} [:string {:max 4}]]
                       [:message {:optional true} [:string {:max 4}]]]
                      [:enum [:message nil :other] [:complete :result] [:error] [:saa-complete]]
                      [:enum 0 1 2]]
         :gen/fmap (fn [[m types i]]
                     (let [t (nth types (mod i (count types)))]
                       (cond-> m t (assoc :type t))))}
   [:type {:optional true} [:enum :message :complete :result :error :error
                            :saa-complete :saa-complete :saa-complete :other]]
   [:phase {:optional true} [:enum :silence :act]]
   [:saa-phase {:optional true} [:enum :abstract]]
   [:content {:optional true} [:string {:max 4}]]
   [:data {:optional true} [:string {:max 4}]]
   [:error {:optional true} [:string {:max 4}]]
   [:message {:optional true} [:string {:max 4}]]])

(def ^:private expected-variant
  {:message :pm/chunk :complete :pm/phase-complete :result :pm/phase-complete
   :error :pm/error :saa-complete :pm/saa-complete})

(deftrifecta-from-schema raw-message-lands-on-its-variant
  #'types/raw-msg->phase-message
  {:in [:cat RawProbe [:maybe [:enum :silence :act]]]
   :out types/PhaseMessageValue
   :rel (fn [[raw phase] out]
          (and (= (get expected-variant (:type raw) :pm/chunk) (:adt/variant out))
               (types/phase-message? out)
               (if (= :pm/saa-complete (:adt/variant out))
                 (= (dissoc raw :type) (:summary out))
                 (= (or (:saa-phase raw) (:phase raw) phase) (:phase out)))))
   :classify (fn [_ out] (:adt/variant out))
   :classify-domain #{:pm/chunk :pm/phase-complete :pm/error :pm/saa-complete}
   :golden-path "test/golden/hive-mcp/saa/raw-msg-phase-message.edn"
   :num-tests 200})

;;; ---------------------------------------------------------------------------
;;; constructors and the core seed
;;; ---------------------------------------------------------------------------

(golden/deftest-golden default-records-golden
  "test/golden/hive-mcp/saa/default-records.edn"
  (let [p (adapters/->default-phase-provider)
        p2 (adapters/->default-phase-provider :backend)
        s (scorer/->default-scorer)
        n (planner/->noop-planner)]
    {:provider        (into {} p)
     :provider-bound  (into {} p2)
     :build-options   (dissoc (psaa/build-options p :act {:extra 1}) :prompt)
     :scored          (psaa/score s [{:data "a bug"} {:data "plain"}])
     :grounding       (psaa/grounding-score s [{:data "x"}] 2)
     :planned         (psaa/synthesize n [] "task")
     :satisfies       [(m/validate adapters/PhaseProvider p)
                       (m/validate scorer/ObservationScorer s)
                       (m/validate planner/PlanSynthesizer n)]}))

(golden/deftest-golden core-seed-install-golden
  "test/golden/hive-mcp/saa/core-seed-install.edn"
  (let [counts (core-seed/install!)]
    {:counts counts :validates (m/validate core-seed/SeedCounts counts)}))

(deftest dag-wave-dispatch-fn-drives-the-port
  (let [calls (atom [])
        start! (fn [plan-id opts] (swap! calls conj [plan-id opts]) {:plan-id 7})
        dispatch (core-seed/dag-wave-dispatch-fn start!)]
    (testing "a run id becomes the wave id and rides on the opts"
      (is (= {:wave-id "run-1" :result {:plan-id 7 :status :dispatched}}
             (dispatch {:id "plan-a"} "ag" {:run-id "run-1" :directory "/w"})))
      (is (= ["plan-a" {:cwd "/w" :run-id "run-1"}] (last @calls))))
    (testing "without a run id the scheduler's plan id is the wave id"
      (is (= "7" (:wave-id (dispatch {:id "plan-b"} "ag" {:directory "/w"}))))
      (is (= ["plan-b" {:cwd "/w"}] (last @calls))))))

;;; ---------------------------------------------------------------------------
;;; Coverage gate — src/hive_mcp/saa, the registry façade included
;;; ---------------------------------------------------------------------------

(def ^:private saa-files
  ["src/hive_mcp/saa/adapters.clj" "src/hive_mcp/saa/core_seed.clj"
   "src/hive_mcp/saa/model.clj" "src/hive_mcp/saa/planner.clj"
   "src/hive_mcp/saa/prompt.clj" "src/hive_mcp/saa/registry.clj"
   "src/hive_mcp/saa/scorer.clj" "src/hive_mcp/saa/types.clj"])

(coverage/deftest-contract-coverage saa-contracts
  {:paths saa-files})

(deftest saa-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths saa-files})]
    (is (= 23 (count (:universe report))) "every public defn of the saa files")
    (is (= 23 (count (:covered report))))))

(deftest every-saa-contract-generates-both-sides
  (doseq [ns-sym '[hive-mcp.saa.adapters hive-mcp.saa.core-seed hive-mcp.saa.model
                   hive-mcp.saa.planner hive-mcp.saa.prompt hive-mcp.saa.registry
                   hive-mcp.saa.scorer hive-mcp.saa.types]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))
