(ns hive-mcp.graph.schema-test
  "The graph entity constructors and type predicates, driven by their own
   contracts (hive-mcp.graph.schema's *Input / entity schemas). The
   constructors stamp a fresh java.util.Date, so their trifectas carry no
   golden-path; the golden below snapshots them with the timestamps dropped."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.graph.schema :as schema]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema make-friction-builds-an-open-count-1-friction
  #'schema/make-friction
  {:in [:cat schema/FrictionInput]
   :out schema/Friction
   :rel (fn [[{:keys [type context reported-by workaround]}] out]
          (and (= type (:friction/type out))
               (= context (:friction/context out))
               (= reported-by (:friction/reported-by out))
               (= (boolean workaround) (contains? out :friction/workaround))))
   :num-tests 100})

(deftrifecta-from-schema make-knowledge-defaults-confidence-and-sets-tags
  #'schema/make-knowledge
  {:in [:cat schema/KnowledgeInput]
   :out schema/Knowledge
   :rel (fn [[{:keys [type content confidence tags derived-from]}] out]
          (and (= type (:knowledge/type out))
               (= content (:knowledge/content out))
               (= (or confidence 0.5) (:knowledge/confidence out))
               (= (when tags (set tags)) (:knowledge/tags out))
               (= (boolean derived-from) (contains? out :knowledge/derived-from))))
   :classify (fn [[{:keys [confidence]}] _] (if confidence :explicit :defaulted))
   :classify-domain #{:explicit :defaulted}
   :num-tests 150 :classify-floor 12})

(deftrifecta-from-schema make-agent-stamps-one-instant
  #'schema/make-agent
  {:in [:cat schema/AgentInput]
   :out schema/Agent
   ;; optional keys are kept on TRUTHINESS (cond->), so false is dropped too
   :rel (fn [[{:keys [id type spawned-by]}] out]
          (and (= id (:agent/id out))
               (= type (:agent/type out))
               (= (:agent/created-at out) (:agent/last-active out))
               (= (boolean spawned-by) (contains? out :agent/spawned-by))))
   :num-tests 100})

(deftrifecta-from-schema make-memory-keywordizes-type-and-duration
  #'schema/make-memory
  {:in [:cat schema/MemoryInput]
   :out schema/Memory
   :rel (fn [[{:keys [id type duration tags references]}] out]
          (and (= id (:memory/id out))
               (= (keyword type) (:memory/type out))
               (= (keyword duration) (:memory/duration out))
               (= (when (seq tags) (set tags)) (:memory/tags out))
               (= (when (seq references) references) (:memory/references out))))
   :num-tests 100})

(deftrifecta-from-schema make-recall-defaults-count-1
  #'schema/make-recall
  {:in [:cat schema/RecallInput]
   :out schema/Recall
   :rel (fn [[{:keys [memory context count]}] out]
          (and (= memory (:recall/memory out))
               (= (keyword context) (:recall/context out))
               (= (or count 1) (:recall/count out))))
   :num-tests 100})

(defn- candidate
  "Inputs for a membership predicate over MEMBERS: half the draws are
   members, half strangers (strings, other type sets, nil), so both arms are
   reached well above the classification floor."
  [members]
  [:any {:gen/schema [:tuple :boolean
                      [:enum {:gen/elements (vec (sort members))} :x]
                      [:any {:gen/elements [:note :ling "tool-missing" :nope nil 42 :decision :long]}]]
         :gen/fmap (fn [[member? m stranger]] (if member? m stranger))}])

(deftrifecta-from-schema valid-friction-type-is-set-membership
  #'schema/valid-friction-type?
  {:in [:cat (candidate schema/friction-types)]
   :out :boolean
   :rel (fn [[t] out] (= out (contains? schema/friction-types t)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 100})

(deftrifecta-from-schema valid-duration-type-is-set-membership
  #'schema/valid-duration-type?
  {:in [:cat (candidate schema/duration-types)]
   :out :boolean
   :rel (fn [[t] out] (= out (contains? schema/duration-types t)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 100})

(defn- untimed [m]
  (into (sorted-map) (remove (fn [[_ v]] (inst? v))) m))

(deftest-golden constructors-and-predicates-golden
  "test/golden/hive-mcp/graph/schema-constructors.edn"
  {:friction  (untimed (schema/make-friction {:type :tool-missing :context "no grep" :reported-by 7}))
   :friction+ (untimed (schema/make-friction {:type :preset-gap :context "gap" :reported-by [:agent/id "a"]
                                              :workaround "manual"}))
   :knowledge (untimed (schema/make-knowledge {:type :decision :content "use malli"}))
   :knowledge+ (untimed (schema/make-knowledge {:type :pattern :content "p" :confidence 0.9
                                                :tags [:a :a :b] :derived-from #{1 2}}))
   :agent     (untimed (schema/make-agent {:id "ling-1" :type :ling :spawned-by 3}))
   :memory    (untimed (schema/make-memory {:id "m1" :type "note" :duration "long"
                                            :tags ["x"] :references [1]}))
   :memory-0  (untimed (schema/make-memory {:id "m2" :type :decision :duration :permanent :tags []}))
   :recall    (untimed (schema/make-recall {:memory 5 :context "user-feedback"}))
   :recall+   (untimed (schema/make-recall {:memory 5 :context :cross-project :count 4}))
   :preds     (into (sorted-map)
                    (for [[k f] {:friction schema/valid-friction-type? :knowledge schema/valid-knowledge-type?
                                 :agent schema/valid-agent-type? :memory schema/valid-memory-type?
                                 :duration schema/valid-duration-type? :recall schema/valid-recall-context?}]
                      [k (mapv f [:tool-missing :decision :ling :note :long :user-feedback :nope])]))})

(deftest memory-types-come-from-the-registry
  (is (set? (schema/memory-types)))
  (is (contains? (schema/memory-types) :note)))
