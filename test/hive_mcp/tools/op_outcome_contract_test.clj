(ns hive-mcp.tools.op-outcome-contract-test
  "Contract facets for the batch-op failure classifier: a schema-derived
   trifecta over op-outcome whose oracle is the rule table of the ns doc,
   plus a golden over one input per rule."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.op-outcome :as oo]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private body-failure-text "{\"success\":false,\"error\":\"boom\"}")

(def ^:private HandlerResult
  "One handler result per rule of the ns doc, plus the not-failed shapes.
   Each map carries one rule key only, so the expected kind is unambiguous."
  [:or :nil :string [:vector :int]
   [:map [:success :boolean]]
   [:map [:text [:enum "{\"success\":false,\"error\":\"boom\"}" "[1,2]" "{\"success\":true}" "plain"]]]
   [:map [:isError :boolean]]
   [:map [:ok :boolean]]
   [:map [:error [:maybe [:enum "e1" "e2"]]]]])

(defn- expected-kind
  "The ns-doc rule table, written out independently of the subject."
  [r]
  (when (map? r)
    (cond
      (false? (:success r))               :success-false
      (= body-failure-text (:text r))     :body-failure
      (true? (:isError r))                :is-error
      (false? (:ok r))                    :ok-false
      (and (some? (:error r))
           (not (contains? r :success)))  :error-key)))

(deftrifecta-from-schema op-outcome-follows-the-rule-table
  #'oo/op-outcome
  {:in [:cat HandlerResult]
   :out oo/Outcome
   :rel (fn [[r] out]
          (let [k (expected-kind r)]
            (and (= (some? k) (:failed? out))
                 (= k (:kind out)))))
   :classify (fn [_ out] (if (:failed? out) :failed :ok))
   :classify-domain #{:failed :ok}
   :golden-path "test/golden/hive-mcp/tools/op-outcome-rules.edn"
   :num-tests 200})

(golden/deftest-golden op-outcome-golden
  "test/golden/hive-mcp/tools/op-outcome-kinds.edn"
  {:success-false (oo/op-outcome {:success false :errors ["first" "second"]})
   :body-failure  (oo/op-outcome {:type "text" :text body-failure-text})
   :is-error      (oo/op-outcome {:isError true :text "bad"})
   :ok-false      (oo/op-outcome {:ok false})
   :error-key     (oo/op-outcome {:error "e"})
   :success-wins  (oo/op-outcome {:success true :error "stray"})
   :null-id       (oo/op-outcome {:id nil} {:null-id-key :id})
   :not-armed     (oo/op-outcome {:id nil})
   :nil           (oo/op-outcome nil)
   :failed?       [(oo/op-failed? {:ok false}) (oo/op-failed? "x")]
   :body          [(oo/json-failure-body {:text body-failure-text})
                   (oo/json-failure-body {:text "{\"success\":true}"})]})

(deftest outcome-arms-are-disjoint
  (testing "a failed outcome validates its own arm and not the other"
    (let [failed [:map {:closed true} [:failed? [:= true]] [:kind oo/FailureKind] [:message :string]]
          ok     [:map {:closed true} [:failed? [:= false]]]]
      (is (m/validate failed (oo/op-outcome {:ok false})))
      (is (not (m/validate ok (oo/op-outcome {:ok false}))))
      (is (m/validate ok (oo/op-outcome {:ok true})))
      (is (not (m/validate failed (oo/op-outcome {:ok true})))))))
