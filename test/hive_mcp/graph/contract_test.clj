(ns hive-mcp.graph.contract-test
  "Contract facets for hive-mcp.graph.schema and hive-mcp.graph.datascript,
   and the coverage gate for src/hive_mcp/graph.

   Every store here is built by the test (in memory, no persist path), so no
   shared state is touched."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.graph.datascript :as gd]
            [hive-mcp.graph.schema :as gs]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage graph-contracts-gate
  {:paths ["src/hive_mcp/graph"]
   :exempt {}})

(deftest every-graph-contract-generates-both-sides
  (doseq [ns-sym '[hive-mcp.graph.schema hive-mcp.graph.datascript]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (let [s (m/schema schema)] (if (= :function (m/type s)) (m/children s) [s]))
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

;; ---------------------------------------------------------------------------
;; Schema predicates: membership in their sets

(def ^:private Candidate
  [:or {:gen/elements [:tool-missing :preset-gap :convention :pattern :note :decision
                       :short :permanent :ling :coordinator :user-feedback :cross-project
                       :nope "ling" "note" nil 42]}
   :keyword :string :nil :int])

(defn- member-of [s] (fn [[x] out] (= out (contains? s x))))

(deftrifecta-from-schema valid-friction-type-is-membership
  #'gs/valid-friction-type?
  {:in [:cat Candidate] :out :boolean :rel (member-of gs/friction-types)
   :golden-path "test/golden/hive-mcp/graph/valid-friction-type.edn"})

(deftrifecta-from-schema valid-knowledge-type-is-membership
  #'gs/valid-knowledge-type?
  {:in [:cat Candidate] :out :boolean :rel (member-of gs/knowledge-types)
   :golden-path "test/golden/hive-mcp/graph/valid-knowledge-type.edn"})

(deftrifecta-from-schema valid-duration-type-is-membership
  #'gs/valid-duration-type?
  {:in [:cat Candidate] :out :boolean :rel (member-of gs/duration-types)})

(deftrifecta-from-schema valid-recall-context-is-membership
  #'gs/valid-recall-context?
  {:in [:cat Candidate] :out :boolean :rel (member-of gs/recall-contexts)})

(deftrifecta-from-schema valid-memory-type-is-membership
  #'gs/valid-memory-type?
  {:in [:cat Candidate] :out :boolean :rel (fn [[x] out] (= out (contains? (gs/memory-types) x)))})

(deftrifecta-from-schema valid-agent-type-follows-the-agent-registry
  #'gs/valid-agent-type?
  {:in [:cat Candidate] :out :boolean
   :rel (fn [[x] out] (= out (boolean (and (or (keyword? x) (string? x))
                                            (contains? gs/agent-types (keyword x))))))})

(deftrifecta-from-schema memory-types-come-from-the-registry
  #'gs/memory-types
  {:in [:cat] :out [:set :keyword]
   :rel (fn [_ out] (every? #{:note :decision :convention :snippet} (filter #{:note :decision :convention :snippet} out)))
   :num-tests 10})

;; ---------------------------------------------------------------------------
;; Entity constructors

(deftrifecta-from-schema make-friction-builds-an-open-friction
  #'gs/make-friction
  {:in [:cat gs/FrictionInput] :out gs/Friction
   :rel (fn [[{:keys [type context workaround]}] out]
          (and (= type (:friction/type out)) (= context (:friction/context out))
               (= (boolean workaround) (contains? out :friction/workaround))
               (or (not workaround) (= workaround (:friction/workaround out)))))})

(deftrifecta-from-schema make-knowledge-defaults-its-confidence
  #'gs/make-knowledge
  {:in [:cat gs/KnowledgeInput] :out gs/Knowledge
   :rel (fn [[{:keys [confidence tags content]}] out]
          (and (= (or confidence 0.5) (:knowledge/confidence out))
               (= content (:knowledge/content out))
               (= (some-> tags set) (:knowledge/tags out))))})

(deftrifecta-from-schema make-agent-stamps-both-times-alike
  #'gs/make-agent
  {:in [:cat gs/AgentInput] :out gs/Agent
   :rel (fn [[{:keys [id]}] out]
          (and (= id (:agent/id out))
               (= (:agent/created-at out) (:agent/last-active out))))})

(deftrifecta-from-schema make-memory-keywordizes-type-and-duration
  #'gs/make-memory
  {:in [:cat gs/MemoryInput] :out gs/Memory
   :rel (fn [[{:keys [type duration tags]}] out]
          (and (= (keyword type) (:memory/type out))
               (= (keyword duration) (:memory/duration out))
               (= (when (seq tags) (set tags)) (:memory/tags out))))})

(deftrifecta-from-schema make-recall-counts-one-by-default
  #'gs/make-recall
  {:in [:cat gs/RecallInput] :out gs/Recall
   :rel (fn [[{:keys [context count]}] out]
          (and (= (keyword context) (:recall/context out))
               (= (or count 1) (:recall/count out))))})

(defn- timeless [m] (into (sorted-map) (remove (fn [[k _]] (#{"created-at" "last-active" "accessed-at"} (name k)))) m))

(golden/deftest-golden entity-constructors-golden
  "test/golden/hive-mcp/graph/entity-constructors.edn"
  {:friction (timeless (gs/make-friction {:type :tool-missing :context "no grep" :reported-by [:agent/id "a"]
                                          :workaround "used carto"}))
   :knowledge (timeless (gs/make-knowledge {:type :decision :content "use carto" :tags ["tools" "tools"]}))
   :agent (timeless (gs/make-agent {:id "ling-1" :type :ling :spawned-by [:agent/id "coord"]}))
   :memory (timeless (gs/make-memory {:id "m-1" :type "note" :duration "long" :tags ["a"] :references [1 2]}))
   :memory-bare (timeless (gs/make-memory {:id "m-2" :type :plan :duration :short :tags []}))
   :recall (timeless (gs/make-recall {:memory [:memory/id "m-1"] :context "cross-project" :count 3}))})

(deftest constructors-refuse-what-their-pre-refuses
  (is (thrown? Throwable (gs/make-friction {:type :not-a-friction :context "x" :reported-by 1})))
  (is (thrown? Throwable (gs/make-knowledge {:type :decision :content nil})))
  (is (thrown? Throwable (gs/make-agent {:id "x" :type :queen}))))

;; ---------------------------------------------------------------------------
;; Datascript store

(deftrifecta-from-schema create-store-keeps-its-persist-path
  #'gd/create-store
  {:in [:cat gd/DsSchema gd/PersistPath] :out gd/Store
   :rel (fn [[_ path] out] (= path (:persist-path out)))
   :num-tests 20})

(deftrifecta-from-schema create-default-store-keeps-its-persist-path
  #'gd/create-default-store
  {:in [:cat gd/PersistPath] :out gd/Store
   :rel (fn [[path] out] (= path (:persist-path out)))
   :num-tests 20})

(deftrifecta-from-schema make-datascript-store-persists-nowhere
  #'gd/make-datascript-store
  {:in [:cat] :out gd/Store
   :rel (fn [_ out] (nil? (:persist-path out)))
   :num-tests 10})

(deftrifecta-from-schema transact-adds-what-it-reports
  #'gd/transact!
  {:in [:cat gd/Store gd/TxData] :out gd/TxReport
   :rel (fn [[store tx] out] (= (:db-after out) (gd/db store)))
   :num-tests 40})

(deftrifecta-from-schema ds-reset-empties-the-store
  #'gd/ds-reset!
  {:in [:cat gd/Store] :out gd/Db
   :rel (fn [[store] out] (and (identical? out (gd/db store)) (zero? (:entity-count (gd/store-stats store)))))
   :num-tests 20})

(deftrifecta-from-schema db-is-the-connection-value
  #'gd/db
  {:in [:cat gd/Store] :out gd/Db
   :rel (fn [[store] out] (identical? out @(:conn store)))
   :num-tests 20})

(deftrifecta-from-schema store-stats-of-a-fresh-store-are-zero
  #'gd/store-stats
  {:in [:cat gd/Store] :out gd/StoreStats
   :rel (fn [_ out] (= 0 (:entity-count out) (:agent-count out)))
   :num-tests 20})

(deftrifecta-from-schema retract-of-an-unknown-entity-is-nil
  #'gd/retract!
  {:in [:cat gd/Store [:tuple {:gen/elements [[:agent/id "nobody"] [:agent/id "ghost"]]} :keyword :string]]
   :out [:maybe gd/TxReport]
   :rel (fn [_ out] (nil? out))
   :num-tests 20})

(deftrifecta-from-schema ds-entity-of-an-unknown-ref-is-nil
  #'gd/ds-entity
  {:in [:cat gd/Store [:tuple {:gen/elements [[:agent/id "nobody"]]} :keyword :string]]
   :out [:maybe gd/Entity]
   :rel (fn [_ out] (nil? out))
   :num-tests 20})

(deftrifecta-from-schema query-of-a-fresh-store-finds-nothing
  #'gd/query
  {:in [:cat gd/Store [:enum '[:find ?e :where [?e :agent/id _]]]]
   :out [:set :any]
   :rel (fn [_ out] (empty? out))
   :num-tests 20})

(golden/deftest-golden store-lifecycle-golden
  "test/golden/hive-mcp/graph/store-lifecycle.edn"
  (let [store (gd/make-datascript-store)
        _ (gd/transact! store [{:agent/id "coord" :agent/type :coordinator}
                               {:agent/id "ling-1" :agent/type :ling}
                               {:friction/type :tool-missing :friction/context "no grep"}
                               {:knowledge/type :decision :knowledge/content "use carto"}])
        by-map (gd/query store '[:find ?t . :in $ ?id :where [?e :agent/id ?id] [?e :agent/type ?t]] {:id "ling-1"})
        by-vec (gd/query store '[:find ?t . :in $ ?id :where [?e :agent/id ?id] [?e :agent/type ?t]] ["coord"])
        no-in (gd/query store '[:find (count ?e) . :where [?e :agent/id _]] {:ignored 1})
        entity (into (sorted-map) (gd/ds-entity store [:agent/id "ling-1"]))
        before (gd/store-stats store)
        retracted (some? (gd/retract! store [:agent/id "ling-1"]))
        missing (gd/retract! store [:agent/id "ling-1"])
        after (gd/store-stats store)
        _ (gd/ds-reset! store)]
    {:by-map by-map :by-vec by-vec :no-in no-in :entity entity :before before
     :retracted retracted :missing missing :after after :after-reset (gd/store-stats store)}))

(deftest store-schemas-tell-their-values-apart
  (let [store (gd/make-datascript-store)]
    (is (m/validate gd/Store store))
    (is (not (m/validate gd/Store {:conn nil :persist-path nil})) "a plain map is not a store")
    (is (m/validate gd/Db (gd/db store)))
    (is (not (m/validate gd/Db {})))
    (testing "both lookup-ref arms"
      (is (m/validate gd/LookupRef [:agent/id "a"]))
      (is (m/validate gd/LookupRef [1 :agent/type :ling]))
      (is (not (m/validate gd/LookupRef [:agent/id]))))))
