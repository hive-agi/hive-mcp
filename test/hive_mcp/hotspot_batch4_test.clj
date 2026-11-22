(ns hive-mcp.hotspot-batch4-test
  "Pure-stage tests for the batch-4 hotspot decompositions: happy, edge and
   error cases for each stage, with schema-driven trifectas where the stage
   has a schema. The stages are private seams, reached by var; no var is
   redefined, the one effectful seam (run-sweep-tasks) takes its port as an
   argument."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.openrouter :as openrouter]
            [hive-mcp.channel.piggyback :as piggyback]
            [hive-mcp.plan.gate :as gate]
            [hive-mcp.scheduler.housekeeping :as housekeeping]
            [hive-mcp.tools.kg.synthetics :as synthetics]
            [hive-mcp.tools.memory.crud.write :as write]
            [hive-schemas.test :as hst]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private validation-failures @#'gate/validation-failures)
(def ^:private validated-verdict @#'gate/validated-verdict)
(def ^:private thrown-verdict @#'gate/thrown-verdict)
(def ^:private resolve-backend-spec @#'openrouter/resolve-backend-spec)
(def ^:private spec-refusal @#'openrouter/spec-refusal)
(def ^:private run-sweep-tasks @#'housekeeping/run-sweep-tasks)
(def ^:private sweep-result @#'housekeeping/sweep-result)
(def ^:private sweep-log-events @#'housekeeping/sweep-log-events)
(def ^:private cursor-window @#'piggyback/cursor-window)
(def ^:private message->row @#'piggyback/message->row)
(def ^:private index-receipt @#'write/index-receipt)
(def ^:private dedup-merged-tags @#'write/dedup-merged-tags)
(def ^:private kg-link-params @#'write/kg-link-params)
(def ^:private canonical-receipt @#'write/canonical-receipt)
(def ^:private finalize-receipt @#'write/finalize-receipt)
(def ^:private cleanup-blind-spots @#'synthetics/cleanup-blind-spots)
(def ^:private cleanup-base @#'synthetics/cleanup-base)

;; =============================================================================
;; plan.gate
;; =============================================================================

(def ^:private ValidationIn
  [:map
   [:schema [:map [:valid :boolean] [:errors {:optional true} [:vector :string]]]]
   [:dependencies [:map [:valid :boolean]
                   [:invalid-refs {:optional true}
                    [:vector [:map [:step-id :string] [:missing-dep :string]]]]]]
   [:cycles [:map [:valid :boolean] [:cycle {:optional true} [:maybe :string]]]]
   [:fields [:map [:valid :boolean]
             [:errors {:optional true}
              [:vector [:map [:key :keyword] [:value :int] [:error :string]]]]]]])

(defn- unknown-iff-no-errors?
  [_ {:keys [errors phase]}]
  (= (= :unknown phase) (empty? errors)))

(hst/deftrifecta-from-schema validation-failures-names-a-phase
  hive-mcp.hotspot-batch4-test/validation-failures
  {:in  ValidationIn
   :out [:map [:errors [:vector :string]]
         [:phase [:enum :schema :dependencies :cycles :fields :unknown]]]
   :rel unknown-iff-no-errors?
   :num-tests 100})

(deftest validation-failures-golden
  (testing "happy: the first failing check names the phase, every failure is listed"
    (is (= {:errors ["Dependencies: steps reference non-existent IDs: s1 -> zz"
                     "Cycles: a->b"]
            :phase :dependencies}
           (validation-failures {:schema {:valid true}
                                 :dependencies {:valid false
                                                :invalid-refs [{:step-id "s1" :missing-dep "zz"}]}
                                 :cycles {:valid false :cycle "a->b"}
                                 :fields {:valid true}}))))
  (testing "edge: an invalid field check with no errors is not a failure"
    (is (= {:errors [] :phase :unknown}
           (validation-failures {:schema {:valid true} :dependencies {:valid true}
                                 :cycles {:valid true} :fields {:valid false :errors []}}))))
  (testing "error: nil validation reads every check as failed but finds no detail"
    (is (= :cycles (:phase (validation-failures nil))))))

(deftest validated-verdict-golden
  (testing "happy: a valid plan with steps carries its metadata"
    (is (= {:valid? true
            :plan {:steps [{:id "a"} {:id "b" :depends-on ["a"]}] :source-format :edn}
            :metadata {:steps-count 2 :decision-id nil :has-dependencies? true
                       :source-format :edn}}
           (validated-verdict {:valid? true
                               :plan {:steps [{:id "a"} {:id "b" :depends-on ["a"]}]
                                      :source-format :edn}}))))
  (testing "edge: a valid plan with no steps is refused as :empty-steps"
    (let [v (validated-verdict {:valid? true :plan {:steps []}})]
      (is (false? (:valid? v)))
      (is (= :empty-steps (:phase v)))
      (is (= "Empty steps vector" (:raw-error v)))))
  (testing "error: a failed validation keeps the raw validation as its raw-error"
    (let [validation {:schema {:valid false :errors ["bad"]}}
          v (validated-verdict {:valid? false :validation validation})]
      (is (= :schema (:phase v)))
      (is (= (pr-str validation) (:raw-error v)))
      (is (str/includes? (:hint v) "schema validation")))))

(deftest thrown-verdict-golden
  (testing "happy: a rejected plan takes its phase from the ex-data"
    (is (= {:valid? false :errors ["schema failed: nope"] :phase :schema}
           (select-keys (thrown-verdict {:error :plan-gate/rejected :message "nope"
                                         :data {:phase :schema}})
                        [:valid? :errors :phase]))))
  (testing "edge: a rejection without a phase is a parse failure"
    (is (= :parse (:phase (thrown-verdict {:error :plan-gate/rejected :message "m" :data nil})))))
  (testing "error: any other exception is :unexpected"
    (is (= {:valid? false :errors ["Unexpected error: npe"] :phase :unexpected
            :hint "The plan content caused an unexpected error. Check the format."
            :raw-error "npe"}
           (thrown-verdict {:error :plan-gate/unexpected :message "npe"})))))

(deftest validate-for-storage-unchanged
  (testing "the public gate still answers exactly as before"
    (is (true? (:valid? (gate/validate-for-storage
                         "{:id \"p\" :title \"t\" :steps [{:id \"s1\" :title \"a\"}]}"))))
    (is (= :empty-steps (:phase (gate/validate-for-storage "{:id \"p\" :title \"t\" :steps []}"))))
    (is (= :parse (:phase (gate/validate-for-storage "garbage"))))))

;; =============================================================================
;; agent.openrouter
;; =============================================================================

(def ^:private reg {:api-url "https://r.example/v1" :secret-key :r-api-key
                    :default-model "m-default"})

(deftest resolve-backend-spec-golden
  (testing "happy: registry fills what the options leave out"
    (is (= {:provider :r :dispatch nil :url "https://r.example/v1" :secret-key :r-api-key
            :api-key "sek" :model "m-default" :prov-name "r"
            :explicit-url? false :registered? true}
           (resolve-backend-spec {:provider :r} reg "sek"))))
  (testing "edge: explicit options win over the registry and the secret"
    (let [s (resolve-backend-spec {:provider :r :api-url "http://x" :api-key "k"
                                   :model "m" :secret-key :other}
                                  reg "sek")]
      (is (= ["http://x" "k" "m" :other true]
             ((juxt :url :api-key :model :secret-key :explicit-url?) s)))))
  (testing "error: no provider and no registry entry reads as custom"
    (is (= {:prov-name "custom" :registered? false :url nil}
           (select-keys (resolve-backend-spec {} nil nil) [:prov-name :registered? :url])))))

(defn- spec [& {:as kvs}]
  (merge {:provider :r :dispatch nil :url "u" :secret-key :r-api-key :api-key "k"
          :model "m" :prov-name "r" :explicit-url? false :registered? true}
         kvs))

(deftest spec-refusal-golden
  (testing "happy: a complete spec passes through"
    (is (= {:ok (spec)} (spec-refusal (spec)))))
  (testing "edge: ollama-compat needs no key; an explicit url bypasses dispatch"
    (is (contains? (spec-refusal (spec :provider :ollama-compat :api-key nil)) :ok))
    (is (contains? (spec-refusal (spec :dispatch :anthropic :explicit-url? true)) :ok)))
  (testing "error: each missing piece refuses with the message the throw carries"
    (is (= "r is not an OpenAI-compat provider (dispatch: anthropic)"
           (:message (spec-refusal (spec :dispatch :anthropic)))))
    (is (= "API URL required for custom provider" (:message (spec-refusal (spec :url nil)))))
    (is (= {:provider :r :secret-key :r-api-key :env "R_API_KEY"}
           (:data (spec-refusal (spec :api-key nil)))))
    (is (= :model-not-configured (get-in (spec-refusal (spec :model nil)) [:data :error])))))

(deftest openai-compat-backend-still-throws-the-same
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"API URL required for custom provider"
                        (openrouter/openai-compat-backend {:api-key "k" :model "m"}))))

;; =============================================================================
;; scheduler.housekeeping
;; =============================================================================

(deftest run-sweep-tasks-port
  (testing "happy: every task is called through the port, in order"
    (let [seen (atom [])
          call (fn [sym fallback] (swap! seen conj sym) {:sym sym :fallback fallback})]
      (is (= {:a {:sym 'x/a :fallback 1} :b {:sym 'x/b :fallback 2}}
             (run-sweep-tasks call [[:a 'x/a 1] [:b 'x/b 2]])))
      (is (= ['x/a 'x/b] @seen))))
  (testing "edge: no tasks, empty result"
    (is (= {} (run-sweep-tasks (fn [_ _] (throw (ex-info "never" {}))) []))))
  (testing "error: the port's failure map is kept as that task's result"
    (is (= {:a {:failed true :error "boom"}}
           (run-sweep-tasks (fn [_ _] {:failed true :error "boom"}) [[:a 'x/a {}]])))))

(deftest sweep-result-golden
  (is (= {:callbacks {:cleaned 0} :pending-reembed {:total 0} :gc-hint {:gc? false}
          :sweep-number 3 :duration-ms 7 :timestamp :t}
         (sweep-result {:callbacks {:cleaned 0}}
                       {:pending-reembed {:total 0} :gc-hint {:gc? false}
                        :sweep-number 3 :duration-ms 7 :timestamp :t}))))

(deftest sweep-log-events-golden
  (testing "happy: eviction and a large free are reported at info"
    (is (= [[:info "Housekeeping: GC sweep evicted" 4 "entries from" 2 "atoms"]
            [:info "Housekeeping: GC hint freed" 80 "MB" "(before:" 500 "MB)"]]
           (sweep-log-events {:gc-sweep {:total-evicted 4 :atom-count 2}
                              :gc-hint {:freed-mb 80 :before-mb 500}}))))
  (testing "edge: a quiet sweep logs nothing, even with no sub-results"
    (is (= [] (sweep-log-events {})))
    (is (= [] (sweep-log-events {:gc-sweep {:total-evicted 0} :gc-hint {:freed-mb 50}}))))
  (testing "error: committed heap that survives a GC warns"
    (is (= [:warn "Housekeeping: committed heap not returning to the OS —"
            "committed" 900 "MB vs used" 100 "MB."
            "Check -XX:-ExplicitGCInvokesConcurrent is set."]
           (first (sweep-log-events {:gc-hint {:gc? true :still-fragmented? true
                                               :committed-mb 900 :used-mb 100}}))))))

;; =============================================================================
;; channel.piggyback
;; =============================================================================

(def ^:private MsgIn
  [:map
   [:agent-id :string]
   [:event-type [:or :keyword :string]]
   [:message :string]
   [:timestamp :int]
   [:task {:optional true} [:maybe :string]]
   [:context-id {:optional true} [:maybe :string]]
   [:ref {:optional true} [:maybe :string]]
   [:deliberate? {:optional true} :boolean]])

(defn- row-mirrors-message?
  [m row]
  (and (= (:timestamp m) (:ts row))
       (= (:agent-id m) (:a row))
       (string? (:e row))
       (= (boolean (:deliberate? m)) (contains? row :deliberate?))
       (= (boolean (:task m)) (contains? row :t))))

(hst/deftrifecta-from-schema message->row-mirrors-the-message
  hive-mcp.hotspot-batch4-test/message->row
  {:in  MsgIn
   :out [:map [:a :string] [:e :string] [:m :string] [:ts :int]]
   :rel row-mirrors-message?
   :num-tests 100})

(defn- msg [pid ts & {:as kvs}] (merge {:project-id pid :timestamp ts} kvs))

(deftest cursor-window-golden
  (let [msgs [(msg "global" 5) (msg "p" 3) (msg "q" 9) (msg "p" 1)]]
    (testing "happy: fresh global and accepted-project shouts, sorted, with their maxima"
      (is (= {:project-key ["a" "p"] :global-key ["s" "global"]
              :in-context [(msg "p" 3) (msg "global" 5)]
              :max-global 5 :max-project 3}
             (cursor-window "a" {:project-id "p" :session-id "s"}
                            {["a" "p"] 1} msgs))))
    (testing "edge: additional projects are accepted; a context-id narrows the window"
      (is (= [3 9] (map :timestamp (:in-context (cursor-window "a" {:project-id "p"
                                                                     :additional-project-ids #{"q"}}
                                                                {["a" "p"] 1 ["a" "global"] 5}
                                                                msgs)))))
      (is (= {:in-context [(msg "p" 3 :context-id "c")] :max-global nil :max-project 3}
             (select-keys (cursor-window "a" {:project-id "p" :context-id "c"} {}
                                         [(msg "p" 3 :context-id "c") (msg "global" 4)])
                          [:in-context :max-global :max-project]))))
    (testing "error: with no project-id only global shouts are read, keyed by the reader"
      (is (= {:project-key ["a" "global"] :global-key ["a" "global"]
              :in-context [(msg "global" 5)] :max-global 5 :max-project nil}
             (cursor-window "a" {} {} msgs))))))

;; =============================================================================
;; tools.memory.crud.write
;; =============================================================================

(def ^:private AddResult
  [:or :nil :string
   [:map [:id [:maybe :string]]
    [:entry {:optional true} [:map [:id [:maybe :string]]]]]])

(defn- ok-iff-canonical?
  [in out]
  (let [id (if (map? in) (:id in) in)]
    (= (contains? out :ok)
       (and (string? id) (not (str/blank? id))
            (or (not (map? in)) (= id (get-in in [:entry :id])))))))

(hst/deftrifecta-from-schema index-receipt-accepts-only-canonical-ids
  hive-mcp.hotspot-batch4-test/index-receipt
  {:in  AddResult
   :out @#'write/IndexReceipt
   :rel ok-iff-canonical?
   :num-tests 200})

(deftest index-receipt-golden
  (testing "happy: a bare id, and a canonical receipt"
    (is (= {:ok {:entry-id "x" :receipt nil}} (index-receipt "x")))
    (is (= {:ok {:entry-id "a" :receipt {:id "a" :c 1}}}
           (index-receipt {:id "a" :entry {:id "a" :c 1}}))))
  (testing "edge: a map with no canonical entry is refused even with an id"
    (is (= :memory/no-entry-id (:error (index-receipt {:id "a"}))))
    (is (= :memory/no-entry-id (:error (index-receipt {:id "a" :entry {:id "b"}})))))
  (testing "error: nil and blank ids name a degraded backend"
    (is (str/starts-with? (:message (index-receipt nil))
                          "Memory store add-entry! did not return an id (got nil)."))
    (is (= :memory/no-entry-id (:error (index-receipt "  "))))))

(deftest dedup-and-kg-params
  (is (= ["a" "b" "c"] (dedup-merged-tags ["a" "b"] ["b" "c"])))
  (is (= [] (vec (dedup-merged-tags nil nil))))
  (is (= ["x"] (dedup-merged-tags ["x" "x"] nil)))
  (is (= {:kg_implements [1] :kg_supersedes [] :kg_depends_on [2] :kg_refines []}
         (kg-link-params {:kg-implements-vec [1] :kg-supersedes-vec []
                          :kg-depends-on-vec [2] :kg-refines-vec []})))
  (is (= {:kg_implements nil :kg_supersedes nil :kg_depends_on nil :kg_refines nil}
         (kg-link-params nil))))

(deftest canonical-receipt-golden
  (is (= {:id "e"} (canonical-receipt "e" [] {:id "e"})))
  (is (nil? (canonical-receipt "e" [{:relation :refines}] {:id "e"})))
  (is (nil? (canonical-receipt "e" [] {:id "other"})))
  (is (nil? (canonical-receipt "e" [] nil))))

(deftest finalize-receipt-golden
  (testing "happy: a retrieved entry carries every outcome the write had"
    (let [{:keys [ok]} (finalize-receipt "e" {:created {:id "e" :type "note" :tags []}
                                              :edge-ids ["k1"]
                                              :requested "axiom" :parked-as "axiom-candidate"
                                              :gate-reason "human review"})]
      (is (= "e" (:id ok)))
      (is (= ["k1"] (:kg_edges_created ok)))
      (is (= {:requested_type "axiom" :parked_as "axiom-candidate" :reason "human review"}
             (:queued_for_review ok)))))
  (testing "edge: a deferred entry names its embedding error"
    (let [{:keys [ok]} (finalize-receipt "e" {:created {:id "e"}
                                              :pending {:deferred/last-error {:error :no-embedder}}})]
      (is (= [true "e" ":no-embedder"]
             ((juxt :embedding_deferred :reembed_id :embedding_error) ok)))))
  (testing "error: a failed retrieval is an err that names the edge failures and the check"
    (is (= {:error :memory/retrieval-failed
            :message (str "Entry indexed as e but retrieval failed. Check memory store connectivity."
                          " KG edges also failed: [{:to \"x\"}] Check edges with: kg traverse")}
           (finalize-receipt "e" {:created nil :edge-errors [{:to "x"}]
                                  :kg-check {:how "kg traverse"}})))))

;; =============================================================================
;; tools.kg.synthetics
;; =============================================================================

(def ^:private UniverseIn
  [:map
   [:sources [:vector :string]]
   [:proj-count [:int {:min -1 :max 50}]]
   [:vis [:map [:visible? :boolean] [:reason {:optional true} [:maybe :string]]]]
   [:store? :boolean]
   [:registered [:vector :string]]])

(defn- liveness-blind-iff-storeless?
  [{:keys [store? registered]} out]
  (= (boolean (some #(= :liveness (:class %)) out))
     (and (not store?) (boolean (seq registered)))))

(hst/deftrifecta-from-schema cleanup-blind-spots-names-what-it-missed
  hive-mcp.hotspot-batch4-test/cleanup-blind-spots
  {:in  UniverseIn
   :out [:vector [:map [:class [:enum :universe :orphaned-record :liveness]]
                  [:reason [:maybe :string]]]]
   :rel liveness-blind-iff-storeless?
   :num-tests 100})

(deftest cleanup-blind-spots-golden
  (testing "happy: a fully visible universe with a store is blind to nothing"
    (is (= [] (cleanup-blind-spots {:sources ["synth-1"] :proj-count 3 :vis {:visible? true}
                                    :store? true :registered ["synth-1"]}))))
  (testing "edge: edges with no derived source, and a failed count"
    (is (= [{:class :universe
             :reason "4 :projects-to edges exist but no synth- source was derived"}]
           (cleanup-blind-spots {:sources [] :proj-count 4 :vis {:visible? true}
                                 :store? true :registered []})))
    (is (= [{:class :universe :reason "projection-edge count query failed"}]
           (cleanup-blind-spots {:sources [] :proj-count -1 :vis {:visible? true}
                                 :store? true :registered []}))))
  (testing "error: no store and an unreadable record set"
    (is (= [{:class :orphaned-record :reason "records unreadable"}
            {:class :liveness
             :reason "no memory store registered; 2 registered synthetics not classified"}]
           (cleanup-blind-spots {:sources ["a" "b"] :proj-count 2
                                 :vis {:visible? false :reason "records unreadable"}
                                 :store? false :registered ["a" "b"]})))))

(deftest cleanup-base-golden
  (is (= {:dry-run? false :action :delete :threshold 0.2 :min-live-members 2 :limit 50
          :universe {:sources 3 :orphans 1 :registered 2 :projection-edges 7}
          :blind []}
         (cleanup-base {:dry-run? nil :action :delete :threshold 0.2 :min-live-members 2
                        :limit 50}
                       {:sources ["a" "b" "c"] :orphans ["c"] :registered ["a" "b"]
                        :proj-count 7}
                       []))))
