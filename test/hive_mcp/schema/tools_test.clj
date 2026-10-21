(ns hive-mcp.schema.tools-test
  "Tests for MCP tool parameter schemas.

   TDD via nREPL - evaluate in CIDER to run tests."
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [hive-mcp.schema.tools :as tools]
            [malli.generator :as mg]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.util :as mu]))

;; =============================================================================
;; Agent Tool Schema Tests
;; =============================================================================

(deftest agent-spawn-params-test
  (testing "valid spawn params"
    (is (m/validate tools/AgentSpawnParams
                    {:name "worker-1"}))
    (is (m/validate tools/AgentSpawnParams
                    {:name "worker-1"
                     :cwd "/home/user/project"
                     :presets ["research" "clojure"]
                     :terminal "vterm"}))
    (is (m/validate tools/AgentSpawnParams
                    {:name "ling-1"
                     :kanban_task_id "task-123"})))

  (testing "missing name rejected"
    (is (not (m/validate tools/AgentSpawnParams {})))
    (is (not (m/validate tools/AgentSpawnParams
                         {:cwd "/tmp"}))))

  (testing "empty name rejected"
    (is (not (m/validate tools/AgentSpawnParams
                         {:name ""}))))

  (testing "resume continues at the tip or forks at seq/turn; chat_run_id is a string"
    (is (m/validate tools/AgentSpawnParams
                    {:name "w" :llm_retries 3 :chat_run_id "run-b"
                     :resume {:run_id "run-a"}}))
    (is (m/validate tools/AgentSpawnParams
                    {:name "w" :resume {:run_id "run-a" :at {:seq 4} :prompt "p"}}))
    (is (m/validate tools/AgentSpawnParams
                    {:name "w" :resume {:run_id "run-a" :at {:turn 2}}})))

  (testing "malformed resume rejected"
    (is (not (m/validate tools/AgentSpawnParams {:name "w" :resume {:at {:seq 1}}})))
    (is (not (m/validate tools/AgentSpawnParams {:name "w" :resume {:run_id "r" :at {:seq 1 :turn 2}}})))
    (is (not (m/validate tools/AgentSpawnParams {:name "w" :resume {:run_id "r" :at {:seq -1}}}))))

  (testing "resume is closed: a misspelled branch point is refused, never read as a resume at the tip"
    (is (not (m/validate tools/ResumeParam {:run_id "r" :fork_at {:seq 3}})))
    (is (not (m/validate tools/ResumeParam {:run_id "r" :at_seq 3})))
    (is (not (m/validate tools/ResumeParam {:run_id "r" :at {:sequence 3}}))))

  (testing "chat_run_id must be a non-empty string"
    (is (not (m/validate tools/AgentSpawnParams {:name "w" :chat_run_id 5})))
    (is (not (m/validate tools/AgentSpawnParams {:name "w" :chat_run_id ""})))))

(deftest non-blank-string-json-schema-test
  (testing "validates like a non-blank check"
    (is (m/validate tools/NonBlankString "run-1"))
    (is (m/validate tools/NonBlankString "  run-1  "))
    (is (not (m/validate tools/NonBlankString "   ")))
    (is (not (m/validate tools/NonBlankString "\t\n"))))
  (testing "advertises only JSON values, the pattern as an ECMA-262 string"
    (let [json-leaf? (fn json-leaf? [x]
                       (cond (map? x)  (every? (fn [[k v]] (and (or (keyword? k) (string? k)) (json-leaf? v))) x)
                             (coll? x) (every? json-leaf? x)
                             :else     (or (nil? x) (string? x) (number? x) (boolean? x) (keyword? x))))
          resume (tools/json-schema tools/ResumeParam)]
      (is (json-leaf? resume))
      (is (string? (get-in resume [:properties "run_id" :allOf 1 :pattern]))))))

(deftest resume-json-schema-derives-from-resume-param
  (let [js (tools/json-schema tools/ResumeParam)]
    (is (= "object" (:type js)))
    (is (= ["run_id"] (:required js)))
    (is (false? (:additionalProperties js)))
    (is (= #{"run_id" "at" "prompt"} (set (keys (:properties js)))))))

(deftest resume-run-id-rejects-whitespace
  (doseq [id ["" " \t " "\n"]]
    (is (not (m/validate tools/ResumeParam {:run_id id})) (pr-str id)))
  (is (m/validate tools/ResumeParam {:run_id "run-1"})))

(deftest param-coercer-names-failing-field
  (let [coerce (tools/param-coercer "spawn" tools/SpawnLoopParams)]
    (doseq [[bad field] [[{:chat_run_id 8} "chat_run_id"]
                         [{:llm_retries -1} "llm_retries"]
                         [{:resume {:run_id " "}} "resume"]]]
      (let [e (try (coerce bad) nil (catch clojure.lang.ExceptionInfo e e))]
        (is (= field (:param (ex-data e))))
        (is (re-find (re-pattern (str "^" field " is invalid")) (ex-message e)))))))

(deftest null-branch-key-coercion
  (let [coerce (tools/param-coercer "resume" tools/ResumeParam)]
    (is (= {:run_id "r" :at {:seq 1}}
           (coerce {:run_id "r" :at {:seq 1 :turn nil}})))
    (is (= {:run_id "r"}
           (coerce {:run_id "r" :at {:turn nil}})))))

(deftest agent-dispatch-params-test
  (testing "valid dispatch params"
    (is (m/validate tools/AgentDispatchParams
                    {:task "Implement feature X"}))
    (is (m/validate tools/AgentDispatchParams
                    {:task "Review PR"
                     :ling_id "ling-123"
                     :priority "high"
                     :context "PR #456 needs review"})))

  (testing "missing task rejected"
    (is (not (m/validate tools/AgentDispatchParams {}))))

  (testing "invalid priority rejected"
    (is (not (m/validate tools/AgentDispatchParams
                         {:task "test"
                          :priority "urgent"})))))

(deftest agent-kill-params-test
  (testing "valid kill params"
    (is (m/validate tools/AgentKillParams
                    {:slave_id "ling-123"}))
    (is (m/validate tools/AgentKillParams
                    {:slave_id "all"
                     :force true
                     :directory "/home/user/project"})))

  (testing "missing slave_id rejected"
    (is (not (m/validate tools/AgentKillParams {})))))

(deftest agent-status-params-test
  (testing "valid status params"
    (is (m/validate tools/AgentStatusParams {}))
    (is (m/validate tools/AgentStatusParams
                    {:directory "/tmp"
                     :include_details true}))))

;; =============================================================================
;; Memory Tool Schema Tests
;; =============================================================================

(deftest memory-add-params-test
  (testing "valid add params"
    (is (m/validate tools/MemoryAddParams
                    {:type "decision"
                     :content "Use Malli for validation"}))
    (is (m/validate tools/MemoryAddParams
                    {:type "snippet"
                     :content "(defn hello [] \"world\")"
                     :tags ["clojure" "example"]
                     :duration "long"
                     :directory "/home/user/project"
                     :agent_id "ling-1"
                     :abstraction_level 2}))
    (is (m/validate tools/MemoryAddParams
                    {:type "decision"
                     :content "Test KG relationships"
                     :kg_implements ["entry-1"]
                     :kg_supersedes ["entry-2"]
                     :kg_depends_on ["entry-3"]
                     :kg_refines ["entry-4"]})))

  (testing "missing required fields rejected"
    (is (not (m/validate tools/MemoryAddParams
                         {:type "decision"})))  ; missing content
    (is (not (m/validate tools/MemoryAddParams
                         {:content "missing type"})))))

(deftest memory-query-params-test
  (testing "valid query params"
    (is (m/validate tools/MemoryQueryParams {}))
    (is (m/validate tools/MemoryQueryParams
                    {:type "decision"}))
    (is (m/validate tools/MemoryQueryParams
                    {:type "snippet"
                     :tags ["clojure"]
                     :limit 10
                     :duration "long"
                     :scope "all"
                     :directory "/tmp"}))))

(deftest memory-get-full-params-test
  (testing "valid get-full params"
    (is (m/validate tools/MemoryGetFullParams
                    {:id "entry-123"})))

  (testing "missing id rejected"
    (is (not (m/validate tools/MemoryGetFullParams {}))))

  (testing "empty id rejected"
    (is (not (m/validate tools/MemoryGetFullParams
                         {:id ""})))))

;; =============================================================================
;; KG Tool Schema Tests
;; =============================================================================

(deftest kg-add-edge-params-test
  (testing "valid add-edge params"
    (is (m/validate tools/KGAddEdgeParams
                    {:from "entry-1"
                     :to "entry-2"
                     :relation "implements"}))
    (is (m/validate tools/KGAddEdgeParams
                    {:from "entry-1"
                     :to "entry-2"
                     :relation "depends-on"
                     :scope "hive-mcp"
                     :confidence 0.95
                     :created_by "agent:ling-1"
                     :source_type "manual"})))

  (testing "missing required fields rejected"
    (is (not (m/validate tools/KGAddEdgeParams
                         {:from "entry-1"
                          :to "entry-2"})))  ; missing relation
    (is (not (m/validate tools/KGAddEdgeParams
                         {:from "entry-1"
                          :relation "implements"})))))  ; missing to

(deftest kg-relation-type-test
  (testing "valid relation types"
    (is (m/validate (tools/KGRelationType) "implements"))
    (is (m/validate (tools/KGRelationType) "supersedes"))
    (is (m/validate (tools/KGRelationType) "refines"))
    (is (m/validate (tools/KGRelationType) "contradicts"))
    (is (m/validate (tools/KGRelationType) "depends-on"))
    (is (m/validate (tools/KGRelationType) "derived-from"))
    (is (m/validate (tools/KGRelationType) "applies-to")))

  (testing "invalid relation types rejected"
    (is (not (m/validate (tools/KGRelationType) "relates-to")))
    (is (not (m/validate (tools/KGRelationType) "")))))

(deftest kg-traverse-params-test
  (testing "valid traverse params"
    (is (m/validate tools/KGTraverseParams
                    {:start_node "entry-1"}))
    (is (m/validate tools/KGTraverseParams
                    {:start_node "entry-1"
                     :direction "outgoing"
                     :relations ["implements" "depends-on"]
                     :max_depth 5
                     :scope "hive-mcp"})))

  (testing "missing start_node rejected"
    (is (not (m/validate tools/KGTraverseParams {})))))

(deftest kg-impact-analysis-params-test
  (testing "valid impact-analysis params"
    (is (m/validate tools/KGImpactAnalysisParams
                    {:node_id "entry-1"}))
    (is (m/validate tools/KGImpactAnalysisParams
                    {:node_id "entry-1"
                     :max_depth 10
                     :scope "hive-mcp"})))

  (testing "missing node_id rejected"
    (is (not (m/validate tools/KGImpactAnalysisParams {})))))

(deftest kg-promote-params-test
  (testing "valid promote params"
    (is (m/validate tools/KGPromoteParams
                    {:edge_id "edge-123"
                     :to_scope "global"})))

  (testing "missing required fields rejected"
    (is (not (m/validate tools/KGPromoteParams
                         {:edge_id "edge-123"})))  ; missing to_scope
    (is (not (m/validate tools/KGPromoteParams
                         {:to_scope "global"})))))  ; missing edge_id

;; =============================================================================
;; Validator Function Tests
;; =============================================================================

(deftest validate-params-test
  (testing "valid params return {:valid true}"
    (let [result (tools/validate-params
                  tools/AgentSpawnParams
                  {:name "worker-1"})]
      (is (:valid result))
      (is (nil? (:errors result)))))

  (testing "invalid params return {:valid false :errors ...}"
    (let [result (tools/validate-params
                  tools/AgentSpawnParams
                  {})]
      (is (not (:valid result)))
      (is (some? (:errors result))))))

;; =============================================================================
;; Response Schema Tests
;; =============================================================================

(deftest mcp-response-test
  (testing "success response"
    (is (m/validate tools/MCPSuccessResponse
                    {:type "text"
                     :text "{\"success\": true}"}))
    (is (m/validate tools/MCPSuccessResponse
                    {:type "text"
                     :text "result"
                     :isError false})))

  (testing "error response"
    (is (m/validate tools/MCPErrorResponse
                    {:type "text"
                     :text "Something went wrong"
                     :isError true})))

  (testing "MCPResponse accepts both"
    (is (m/validate tools/MCPResponse
                    {:type "text" :text "ok"}))
    (is (m/validate tools/MCPResponse
                    {:type "text" :text "error" :isError true}))))

;; =============================================================================
;; Facets over the helpers' contracts
;; =============================================================================
;;
;; The schema argument of every helper comes from its OWN contract (the
;; :gen/elements on the m/=> input), so a schema added there is exercised here
;; too. Values are generated from that schema and then perturbed, so both
;; arms of the result are reached.

(defn- contract-input
  "The Nth input schema of hive-mcp.schema.tools/SYM's contract."
  [sym n]
  (nth (m/children (first (m/children (m/schema (get-in (m/function-schemas) ['hive-mcp.schema.tools sym :schema])))))
       n))

(def ^:private SchemaAndValue
  "validate-params' argument list: a schema from its contract and a value
   for it, generated from the schema (validates) or a foreign value (usually
   does not)."
  [:cat {:gen/fmap (fn [[s pick foreign]]
                     [s (if pick (mg/generate s {:seed (hash foreign)}) foreign)])
         :gen/schema [:tuple (contract-input 'validate-params 0) :boolean
                      [:enum nil 1 "x" {} {:a "no"} [:kw]]]}
   :any :any])

(deftrifecta-from-schema validate-params-is-malli-validate
  hive-mcp.schema.tools/validate-params
  {:in SchemaAndValue
   :out tools/ValidationResult
   :rel (fn [[s p] out]
          (and (= (:valid out) (m/validate s p))
               (= (some? (:errors out)) (not (m/validate s p)))))
   :classify (fn [_ out] (:valid out))
   :classify-domain #{true false}
   ;; the :errors arm holds malli schema objects, which an EDN golden cannot
   ;; print back; the mutation facet would snapshot them the same way
   :mutation false
   :num-tests 300})

(deftrifecta-from-schema json-schema-object-names-its-required-keys
  hive-mcp.schema.tools/json-schema
  {:in (contract-input 'json-schema 0)
   :out [:map-of :keyword :any]
   :rel (fn [s out]
          (let [ms (m/schema s)]
            (if (= :map (m/type ms))
              (and (= "object" (:type out))
                   (= (set (map (comp name first) (m/children ms))) (set (keys (:properties out))))
                   (= (set (keep (fn [[k p]] (when-not (:optional p) (name k))) (m/children ms)))
                      (set (:required out))))
              (not= "object" (:type out)))))
   :classify (fn [_ out] (= "object" (:type out)))
   :classify-domain #{true false}
   :num-tests 200
   :golden-path "test/golden/hive-mcp/schema/tools/json-schema.edn"})

(deftest-golden param-coercer-decodes-json-and-names-the-field-golden
  "test/golden/hive-mcp/schema/tools/param-coercer.edn"
  (let [schema [:map [:name [:string {:min 1}]] [:n {:optional true} :int]]
        c (tools/param-coercer "p" schema)
        thrown (fn [x] (try (c x) :no-throw
                            (catch clojure.lang.ExceptionInfo e [(ex-message e) (ex-data e)])))]
    {:decoded [(c {"name" "w" "n" "3"}) (c {:name "w"})]
     :one-bad-field (thrown {"name" ""})
     :two-bad-fields (thrown {"name" "" "n" "x"})
     :not-a-map (thrown 5)
     :coerce-and-validate [(tools/coerce-and-validate [:int] 1)
                           (:valid (tools/coerce-and-validate [:int] "1"))]}))

(comment
  ;; Run all tests in REPL
  (clojure.test/run-tests 'hive-mcp.schema.tools-test)

  ;; Run specific test
  (agent-spawn-params-test)
  (memory-add-params-test)
  (kg-add-edge-params-test))
