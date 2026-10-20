(ns hive-mcp.tools.diagnostics-contract-test
  "Contract facets for the protocol dispatch-failure diagnostics. Inputs are
   drawn from fixed messages that reach every diagnosis: a stale class, a
   missing method key, a class never extended, an unresolvable protocol, and
   text that is not a dispatch failure at all."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.diagnostics :as diag]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private expected
  "Dispatch-failure message -> the diagnosis classify-dispatch-failure owes it.
   CollReduce is extended to Object, so String inherits an impl carrying
   :coll-reduce (stale-class) but no :nope (missing-method)."
  {"No implementation of method: :coll-reduce of protocol: #'clojure.core.protocols/CollReduce found for class: java.lang.String"
   :stale-class
   "No implementation of method: :nope of protocol: #'clojure.core.protocols/CollReduce found for class: java.lang.String"
   :missing-method
   "No implementation of method: :coll-reduce of protocol: #'clojure.core.protocols/CollReduce found for class: no.such.Klass"
   :not-extended
   "No implementation of method: :m of protocol: #'hive-mcp.no-such/Proto found for class: java.lang.String"
   :unknown})

(def ^:private Message
  (into [:enum] (concat (keys expected) ["boom" "" "No implementation of method"])))

(deftrifecta-from-schema detect-names-every-diagnosis
  #'diag/detect-protocol-poisoning
  {:in [:cat Message]
   :out [:maybe diag/PoisoningReport]
   :rel (fn [[x] out]
          (and (= (get expected x) (:diagnosis out))
               (= (:poisoned? out) (when out (= :stale-class (:diagnosis out))))))
   :classify (fn [_ out] (or (:diagnosis out) :none))
   :classify-domain #{:none :stale-class :missing-method :not-extended :unknown}
   :golden-path "test/golden/hive-mcp/tools/diagnostics-detect.edn"
   :num-tests 200})

(deftrifecta-from-schema recovery-hint-advises-restart-only-when-stale
  #'diag/recovery-hint
  {:in [:cat Message]
   :out [:maybe :string]
   :rel (fn [[x] out]
          (if-let [d (get expected x)]
            (and (string? out)
                 (str/includes? out "protocol:")
                 (= (= :stale-class d) (str/includes? out "restart the JVM")))
            (nil? out)))
   :num-tests 200})

(golden/deftest-golden classify-dispatch-failure-golden
  "test/golden/hive-mcp/tools/diagnostics-classify.edn"
  {:stale     (diag/classify-dispatch-failure "clojure.core.protocols/CollReduce" ":coll-reduce" "java.lang.String")
   :missing   (diag/classify-dispatch-failure "clojure.core.protocols/CollReduce" "-nope" "java.lang.String")
   :not-ext   (diag/classify-dispatch-failure "clojure.core.protocols/CollReduce" ":coll-reduce" "no.such.Klass")
   :unknown   (diag/classify-dispatch-failure "hive-mcp.no-such/Proto" ":m" "java.lang.String")})

(deftest throwable-input-reads-its-message
  (let [[msg d] (first expected)]
    (is (= d (:diagnosis (diag/detect-protocol-poisoning (IllegalArgumentException. ^String msg)))))
    (is (m/validate diag/DispatchVerdict {:diagnosis :missing-method :impl-keys [:a]}))
    (is (not (m/validate diag/DispatchVerdict {:diagnosis :bogus})))))
