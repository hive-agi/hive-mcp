(ns hive-mcp.transport.a2a.schema-test
  "Facets for hive-mcp.transport.a2a.schema (cov-facets f07): the A2A
   factories and JSON-RPC wrappers are pure; each is driven from its own
   contract's value schemas."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.transport.a2a.schema :as s]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:or [:string {:min 1 :max 6}] [:int {:min 0 :max 99}]])
(def ^:private Text [:maybe [:string {:max 8}]])

(deftrifecta-from-schema make-message-contract s/make-message
  {:in  [:cat [:maybe [:enum "user" "agent"]]
         [:maybe [:sequential {:gen/max 3} [:map [:type [:= "text"]] [:text Text]]]]]
   :out s/Message
   :rel (fn [[role parts] out]
          (and (= role (:role out)) (vector? (:parts out)) (= (seq parts) (seq (:parts out)))))
   :golden-path "test/golden/hive-mcp/f07-a2a-make-message.edn"})

(deftrifecta-from-schema make-status-update-event-contract s/make-status-update-event
  {:in  [:cat Id [:maybe s/TaskState] Text]
   :out s/StatusUpdateEvent
   :rel (fn [[id state msg] out]
          (and (= id (:id out))
               (= state (get-in out [:status :state]))
               (= msg (get-in out [:status :message :parts 0 :text]))))
   :golden-path "test/golden/hive-mcp/f07-a2a-status-update.edn"})

(deftrifecta-from-schema make-artifact-update-event-contract s/make-artifact-update-event
  {:in  [:cat Id [:map [:name [:string {:max 4}]]]]
   :out s/ArtifactUpdateEvent
   :rel (fn [[id artifact] out] (= {:id id :artifact artifact :kind "artifact-update"} out))
   :golden-path "test/golden/hive-mcp/f07-a2a-artifact-update.edn"})

(deftrifecta-from-schema jsonrpc-success-contract s/jsonrpc-success
  {:in  [:cat s/JsonRpcId [:or :int [:map [:ok :boolean]]]]
   :out s/RpcSuccess
   :rel (fn [[id result] out] (= {:jsonrpc "2.0" :id id :result result} out))
   :golden-path "test/golden/hive-mcp/f07-a2a-jsonrpc-success.edn"})

(deftrifecta-from-schema make-task-contract s/make-task
  {:in  [:cat Id Id [:maybe s/TaskState] Text]
   :out s/Task
   :rel (fn [[id ctx state msg] out]
          (and (= [id ctx state msg]
                  [(:id out) (:contextId out) (get-in out [:status :state])
                   (get-in out [:status :message :parts 0 :text])])
               (not-any? #(contains? out %) [:messages :artifacts :metadata])))
   :golden-path "test/golden/hive-mcp/f07-a2a-make-task.edn"})

(deftest-golden a2a-varargs-golden
  "test/golden/hive-mcp/f07-a2a-varargs.edn"
  {:task-with-opts (s/make-task "t1" "c1" "working" "busy"
                                :metadata {:agent "ling-1"}
                                :messages [(s/make-message "user" [{:type "text" :text "hi"}])])
   :task-map-opts  (s/make-task "t2" "c1" "completed" "done" {:artifacts [{:name "out"}]})
   :error-plain    (s/jsonrpc-error 7 (:task-not-found s/error-codes) "no such task")
   :error-data     (s/jsonrpc-error "r" (:invalid-params s/error-codes) "bad" :field "id")
   :error-nil-code (s/jsonrpc-error nil (:no-such-kind s/error-codes) nil)
   :state-map      (into (sorted-map) s/agent-status->task-state)})

(deftest every-mapped-state-is-a-task-state
  (is (every? #(contains? #{"submitted" "working" "input-required" "completed" "failed" "canceled"} %)
              (vals s/agent-status->task-state))))
