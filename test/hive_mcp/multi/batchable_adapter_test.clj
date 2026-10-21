(ns hive-mcp.multi.batchable-adapter-test
  "Name-matched facets for `hive-mcp.multi.batchable-adapter`.

     make-default-adapter  trifecta over both arities: the record carries
                           exactly the tool name, resolver and emit-fx it
                           was built from, and satisfies Batchable
     batch-execute         golden over an adapter whose two ports are
                           injected: a resolver answering one probe tool
                           (echo / throw) and a recording emit-fx. The
                           snapshot pins the waves, per-op results, summary,
                           the fx event sequence, and the advertised schema."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.batch.protocol :as bproto]
            [hive-mcp.multi.batchable-adapter :as ba]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Port
  "A port fn the adapter stores verbatim (keywords are callable, and keep the
   record EDN-comparable)."
  [:enum :resolver-a :resolver-b])

(deftrifecta-from-schema make-default-adapter-binds-its-ports
  #'ba/make-default-adapter
  ;; [:? ...] drives both arities from one arglist schema
  {:in [:cat [:enum "memory" "kg" "probe"] Port [:? [:maybe [:enum :fx-a :fx-b]]]]
   :out [:map [:tool-name :string] [:resolve-handler :keyword] [:emit-fx [:maybe :keyword]]]
   :rel (fn [[tool resolver fx] out]
          (and (satisfies? bproto/Batchable out)
               (= {:tool-name tool :resolve-handler resolver :emit-fx fx} (into {} out))))
   :classify (fn [args _] (if (= 3 (count args)) :three-arity :two-arity))
   :classify-domain #{:three-arity :two-arity}
   :num-tests 200})

(defn- probe-resolver
  "Resolver port: the probe tool echoes :msg, or throws on command boom."
  [tool]
  (when (= tool "probe")
    (fn [p]
      (case (:command p)
        "echo" {:type "text" :text (str (:msg p))}
        "boom" (throw (ex-info "boom" {}))
        {:type "text" :text "?"}))))

(defn- run-batch [ops]
  (let [fx (atom [])
        a  (ba/make-default-adapter "probe" probe-resolver (fn [event & _] (swap! fx conj event)))
        r  (log/with-min-level :info (bproto/batch-execute a ops {}))]
    {:result r :fx @fx}))

(deftest-golden batch-execute-through-injected-ports-golden
  "test/golden/hive-mcp/multi/batchable-adapter-execute.edn"
  {:mixed    (run-batch [{:id "a" :tool "probe" :command "echo" :msg "hi"}
                         {:id "b" :tool "probe" :command "boom" :depends_on ["a"]}
                         {:id "c" :tool "nope" :command "x"}])
   :all-ok   (run-batch [{:id "a" :tool "probe" :command "echo" :msg "one"}
                         {:id "b" :tool "probe" :command "echo" :msg "two" :depends_on ["a"]}])
   :invalid  (run-batch [{:id "a" :command "echo"}])
   :schema   (bproto/batch-schema (ba/make-default-adapter "probe" probe-resolver))})

(deftest a-dependent-of-a-failed-op-is-skipped
  (let [{:keys [result]} (run-batch [{:id "a" :tool "nope" :command "x"}
                                     {:id "b" :tool "probe" :command "echo" :depends_on ["a"]}])]
    (is (false? (:success result)))
    (is (= "Skipped: dependencies failed — a"
           (get-in result [:waves 2 :results 0 :error])))))
