(ns hive-mcp.dsl.param-domain-test
  "Schema-driven trifectas (property + golden + mutation) for the pure
   param-domain predicates. The :rel oracles restate the documented law
   against the public key sets, independent of the function bodies."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.dsl.param-domain :as pd]
            [hive-schemas.coverage :as cov]
            [hive-schemas.test :as hst]
            [clojure.set]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private LabelText
  [:enum "$0" "$12" "$ref:x" "op-1" "" "0$" :kw nil 42])

(hst/deftrifecta-from-schema op-label-is-a-dollar-string
  hive-mcp.dsl.param-domain/op-label?
  {:in LabelText
   :out :boolean
   :rel (fn [in out]
          (= out (and (string? in) (= \$ (first in)))))
   :golden-path "test/golden/dsl/param_domain/op-label.edn"
   :num-tests 60
   :seed 0
   :n-cases 9})

(def ^:private ParamKey
  [:enum :content :title :prompt :reason :id :tool :wave :foo :from "content" nil])

(hst/deftrifecta-from-schema prose-param-matches-the-prose-key-set
  hive-mcp.dsl.param-domain/prose-param?
  {:in ParamKey
   :out :boolean
   :rel (fn [in out] (= out (contains? pd/prose-param-keys in)))
   :golden-path "test/golden/dsl/param_domain/prose-param.edn"
   :num-tests 60
   :seed 0
   :n-cases 8})

(def ^:private OpMap
  [:map-of [:enum :id :tool :command :depends_on :wave :content :title :from :to :limit]
   [:enum 1 "a" "$ref:op-1" nil]])

(hst/deftrifecta-from-schema ref-walkable-drops-meta-and-prose
  hive-mcp.dsl.param-domain/ref-walkable-entries
  {:in OpMap
   :out [:sequential [:tuple :any :any]]
   :rel (fn [in out]
          (let [walkable (remove (fn [[k _]]
                                   (or (contains? pd/meta-param-keys k)
                                       (contains? pd/prose-param-keys k)))
                                 in)]
            (= (set walkable) (set out))))
   :golden-path "test/golden/dsl/param_domain/ref-walkable-entries.edn"
   :num-tests 80
   :seed 0
   :n-cases 5})

(cov/deftest-contract-coverage every-param-domain-fn-carries-a-contract
  {:paths ["src/hive_mcp/dsl/param_domain.clj"]})

(deftest meta-and-prose-key-sets-are-disjoint
  (is (empty? (clojure.set/intersection pd/meta-param-keys pd/prose-param-keys))))
