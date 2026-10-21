(ns hive-mcp.memory.domain-test
  "ScopeFilter ADT: parse-scope as a schema-driven trifecta over the raw
   boundary strings; scope->search-project-ids as a trifecta with its scope
   hierarchy INJECTED (the :visible-scopes / :descendant-scopes ports it
   documents), so no ambient project tree is read; and a golden over the
   remaining projections for an id the hierarchy does not know."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.memory.domain :as domain]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private RawScope
  "Raw scope strings, one variant picked uniformly first so each of the four
   ScopeFilter arms gets about a quarter of the draws (a flat element list
   plus the :maybe nil left \"all\" under the classification floor)."
  [:maybe {:gen/schema [:tuple [:enum :all :global :project :auto]
                        [:enum "hive-mcp" "topic:x" "p1"]]
           :gen/fmap (fn [[arm pid]] (case arm :all "all" :global "global" :project pid :auto nil))}
   :string])

(def ^:private ProjectId
  [:maybe [:string {:gen/elements ["hive-mcp" "global" "p2"]}]])

(deftrifecta-from-schema parse-scope-picks-the-variant
  #'domain/parse-scope
  {:in [:cat RawScope ProjectId]
   :out [:map [:adt/type :keyword] [:adt/variant :keyword]]
   :rel (fn [[s pid] out]
          (case (:adt/variant out)
            :scope/all     (= "all" s)
            :scope/global  (= "global" s)
            :scope/project (and (some? s) (not (#{"all" "global"} s)) (= s (:project-id out)))
            :scope/auto    (and (nil? s) (= pid (:project-id out)))
            false))
   :classify (fn [_ out] (:adt/variant out))
   :classify-domain #{:scope/all :scope/global :scope/project :scope/auto}
   :golden-path "test/golden/hive-mcp/memory/domain-parse-scope.edn"
   :num-tests 200})

(defn- visible [pid] [pid "parent" "global"])
(defn- descendants-of [pid] [(str pid "/child")])

(def ^:private SearchIn
  "A parsed scope (built through parse-scope from boundary strings) and the
   search options with the hierarchy ports injected."
  [:cat
   [:map {:gen/schema [:tuple RawScope ProjectId]
          :gen/fmap (fn [[s pid]] (domain/parse-scope s pid))}
    [:adt/type :keyword] [:adt/variant :keyword]]
   [:map {:gen/schema [:tuple :boolean :boolean]
          :gen/fmap (fn [[d s]] {:include-descendants? d :strict? s
                                 :visible-scopes visible :descendant-scopes descendants-of})}]])

(deftrifecta-from-schema search-project-ids-honours-strictness
  #'domain/scope->search-project-ids
  {:in SearchIn
   :out [:maybe [:vector :string]]
   :rel (fn [[sf {:keys [include-descendants? strict?]}] out]
          (let [pid  (:project-id sf)
                kids (when include-descendants? (descendants-of pid))]
            (= out
               (case (:adt/variant sf)
                 :scope/all     nil
                 :scope/global  ["global"]
                 :scope/project (vec (distinct (concat (if strict? [pid] (visible pid)) kids)))
                 :scope/auto    (when (and pid (not= pid "global"))
                                  (vec (distinct (concat (visible pid) kids))))))))
   :classify (fn [[sf] _] (:adt/variant sf))
   :classify-domain #{:scope/all :scope/global :scope/project :scope/auto}
   :num-tests 200})

(deftest-golden projections-golden
  "test/golden/hive-mcp/memory/domain-projections.edn"
  (let [pid "f04-unknown-project"]
    (into (sorted-map)
          (for [[k sf] {:all (domain/parse-scope "all" pid) :global (domain/parse-scope "global" pid)
                        :project (domain/parse-scope pid nil) :auto (domain/parse-scope nil pid)
                        :auto-global (domain/parse-scope nil "global") :auto-nil (domain/parse-scope nil nil)}]
            [k {:effective (domain/scope->effective sf)
                :ids       (domain/scope->project-ids sf false)
                :ids+desc  (domain/scope->project-ids sf true)
                :tags      (some-> (domain/scope->tag-set sf false) sort vec)}]))))

(deftest project-scope-keeps-the-id
  (is (= "x" (:project-id (domain/parse-scope "x" "y")))))
