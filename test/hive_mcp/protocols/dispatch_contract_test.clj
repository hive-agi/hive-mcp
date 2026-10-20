(ns hive-mcp.protocols.dispatch-contract-test
  "Contract facets for the dispatch-context factories and the string coercion."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.protocols.dispatch :as dp]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema text-context-wraps-the-prompt
  #'dp/->text-context
  {:in [:cat :string]
   :out dp/TextContextValue
   :rel (fn [[p] out] (and (= {:prompt p} (dp/resolve-context out)) (= :text (dp/context-type out))))
   :num-tests 100})

(deftrifecta-from-schema ensure-context-keeps-a-context-and-wraps-the-rest
  #'dp/ensure-context
  {:in [:cat [:or :string :int :nil :keyword dp/DispatchContext]]
   :out dp/DispatchContext
   :rel (fn [[x] out]
          (if (satisfies? dp/IDispatchContext x)
            (identical? x out)
            (= {:prompt (str x)} (dp/resolve-context out))))
   :classify (fn [[x] _] (if (satisfies? dp/IDispatchContext x) :context :wrapped))
   :classify-domain #{:context :wrapped}
   :num-tests 100})

;; The generated reconstruct-fn is nil, so ->ref-context installs its lazy
;; default; the relation reads the record's fields and never resolves it.
(deftrifecta-from-schema ref-context-defaults-its-refs-and-nodes
  #'dp/->ref-context
  {:in [:cat :string [:maybe [:map
                              [:ctx-refs {:optional true} [:maybe [:map-of :keyword :string]]]
                              [:kg-node-ids {:optional true} [:maybe [:or [:vector :string] [:set :string]]]]
                              [:scope {:optional true} [:maybe :string]]]]]
   :out dp/RefContextValue
   :rel (fn [[p opts] out]
          (and (= p (:prompt out))
               (= (or (:ctx-refs opts) {}) (:ctx-refs out))
               (= (vec (or (:kg-node-ids opts) [])) (:kg-node-ids out))
               (= (:scope opts) (:scope out))
               (ifn? (:reconstruct-fn out))
               (= :ref (dp/context-type out))))
   :num-tests 100})

(golden/deftest-golden resolve-context-golden
  "test/golden/hive-mcp/protocols/dispatch-resolve-context.edn"
  (let [recon (fn [refs nodes scope] (str "R:" (count refs) "/" (count nodes) "@" scope))
        boom (fn [_ _ _] (throw (ex-info "down" {})))]
    {:text (dp/resolve-context (dp/->text-context "fix auth"))
     :ref-bare (dp/resolve-context (dp/->ref-context "task" {:reconstruct-fn (fn [_ _ _] nil)}))
     :ref-full (dp/resolve-context (dp/->ref-context "task" {:ctx-refs {:axioms "ctx-1"}
                                                            :kg-node-ids #{"n1"}
                                                            :scope "hive-mcp"
                                                            :reconstruct-fn recon}))
     :ref-failing (dp/resolve-context (dp/->ref-context "task" {:reconstruct-fn boom}))
     :graph-fallback (dp/resolve-context (dp/->graph-context "node-42" nil))
     :coerced (dp/resolve-context (dp/ensure-context 42))}))

(deftest graph-context-falls-back-to-text
  (is (= :text (dp/context-type (dp/->graph-context "node-1" nil)))))
