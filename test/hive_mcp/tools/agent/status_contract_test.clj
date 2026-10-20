(ns hive-mcp.tools.agent.status-contract-test
  "Contract facets for agent status: a trifecta over annotate-liveness with
   the status function injected (only a non-negative number is copied, order
   kept, a throwing or unknown backend leaves the row untouched), a golden
   over backend-status-fn with a stub backend registry, and the handlers'
   error envelopes in a unit JVM without hive-datascript."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.agent.status :as st]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.addon.headless :as headless]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private ages
  "The age the injected status function reports, by row id."
  {"a" 0 "b" 1500 "c" -5 "d" nil "e" :throw})

(defn- status-of
  "Status port: answers each row from `ages`; \"e\" throws."
  [row]
  (let [age (get ages (:slave/id row))]
    (if (= :throw age) (throw (ex-info "boom" {})) {:liveness-age-ms age})))

;; No :golden-path: the input holds the status function, which has no EDN
;; form; status-golden pins literal annotations.
(deftrifecta-from-schema annotate-liveness-copies-only-valid-ages
  #'st/annotate-liveness
  {:in [:cat [:fn {:gen/elements [status-of]} ifn?]
        [:vector {:max 5} [:map [:slave/id [:enum "a" "b" "c" "d" "e" "z"]]]]]
   :out [:vector st/AnnotatedRow]
   :rel (fn [[_ rows] out]
          (and (= (map :slave/id rows) (map :slave/id out))
               (every? (fn [[in o]]
                         (let [a (get ages (:slave/id in))]
                           (if (and (number? a) (not (neg? a)))
                             (= a (:slave/liveness-age-ms o))
                             (= in o))))
                       (map vector rows out))))
   :num-tests 100})

(defn- stub-backend [age]
  (reify headless/IHeadlessBackend
    (headless-id [_] :stub)
    (headless-spawn! [_ _ _] nil)
    (headless-dispatch! [_ _ _] true)
    (headless-status [_ ctx _] {:slave/id (:id ctx) :liveness-age-ms age})
    (headless-kill! [_ _] {:killed? true})
    (headless-interrupt! [_ _] {:success? false})))

(golden/deftest-golden status-golden
  "test/golden/hive-mcp/tools/agent/status-backend.edn"
  (let [f (st/backend-status-fn {:stub (stub-backend 42)})]
    {:stub        (f {:slave/id "x" :ling/spawn-mode :stub})
     :unknown     (f {:slave/id "x" :ling/spawn-mode :claude})
     :no-mode     (f {:slave/id "x"})
     :annotated   (st/annotate-liveness status-of (mapv #(hash-map :slave/id %) ["a" "b" "c" "d" "e" "z"]))
     :not-found   (st/handle-status {:agent_id "nope"})
     :digest-keys (sort (keys (st/handle-digest {})))}))

(deftest default-backend-status-fn-is-a-port
  (is (ifn? (st/backend-status-fn)))
  (is (nil? ((st/backend-status-fn) {:slave/id "x"}))))
