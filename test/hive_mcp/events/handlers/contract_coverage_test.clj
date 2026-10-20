(ns hive-mcp.events.handlers.contract-coverage-test
  "Contract coverage gate for the event handler tree: every public defn under
   src/hive_mcp/events/handlers and in the handlers.clj facade carries an m/=>
   contract, and every schema those contracts name generates."
  (:require [clojure.test :refer [deftest is]]
            [malli.core :as m]
            [malli.generator :as mg]
            [hive-mcp.events.handlers]
            [hive-mcp.events.handlers.schema :as hs]
            [hive-schemas.coverage :as coverage]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(coverage/deftest-contract-coverage event-handler-contracts
  {:paths ["src/hive_mcp/events/handlers"
           "src/hive_mcp/events/handlers.clj"]})

(def ^:private handler-namespaces
  "The namespaces the gate above scans; each one's contracts must generate."
  '[hive-mcp.events.handlers hive-mcp.events.handlers.agora hive-mcp.events.handlers.claim
    hive-mcp.events.handlers.conversation hive-mcp.events.handlers.crystal
    hive-mcp.events.handlers.hot-reload hive-mcp.events.handlers.kanban
    hive-mcp.events.handlers.kg hive-mcp.events.handlers.lifecycle
    hive-mcp.events.handlers.ling hive-mcp.events.handlers.memory-read
    hive-mcp.events.handlers.resilience hive-mcp.events.handlers.saa
    hive-mcp.events.handlers.saa-config hive-mcp.events.handlers.saa-fx
    hive-mcp.events.handlers.session hive-mcp.events.handlers.system
    hive-mcp.events.handlers.task])

(deftest shared-handler-schemas-generate
  (doseq [s [hs/HandlerArgs hs/LogEffect hs/HandlerRegistry]]
    (is (= 5 (count (mg/sample s {:size 5 :seed 11}))) (pr-str (m/form s)))))

(deftest every-handler-contract-generates-both-sides
  (doseq [ns-sym handler-namespaces
          :let [_ (require ns-sym)]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))
