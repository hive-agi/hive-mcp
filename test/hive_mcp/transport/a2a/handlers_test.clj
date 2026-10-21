(ns hive-mcp.transport.a2a.handlers-test
  "Facets for hive-mcp.transport.a2a.handlers (cov-facets f07).

   Pinned: every validation refusal (no agent id, no parts, no task id, no
   agent metadata) and GetTask, all of which answer before any agent lookup.
   Each answer is checked against HandlerResult / StreamingResult.

   Not covered: any call that reaches resolve-agent. It reads the swarm
   DataScript through hive-datascript, which is absent from the unit classpath,
   and the handler's own contract says that absence propagates as a throw;
   with the library present it builds the agent with ling/->ling, a
   concretion. There is no IAgent port to inject a stub through without
   with-redefs (missing seam, recorded in the report). The throw is asserted
   below so a change to that arm is noticed."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.transport.a2a.handlers :as h]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private text-parts [{:type "text" :text "hello"}])

(defn- refusal-cases []
  {:no-agent-id    (h/handle-send-message {:message {:role "user" :parts text-parts}})
   :blank-agent    (h/handle-send-message {:message {:role "user" :parts text-parts}
                                           :configuration {:agentId ""}})
   :no-parts       (h/handle-send-message {:message {:role "user" :parts []}
                                           :configuration {:agentId "ling-1"}})
   :nil-params     (h/handle-send-message nil)
   :stream-refused (h/handle-send-streaming-message {:configuration {:agentId "ling-1"}})
   :get-no-id      (h/handle-get-task {})
   :cancel-no-id   (h/handle-cancel-task {:id ""})
   :cancel-no-meta (h/handle-cancel-task {:id "a2a-1"})})

(deftest-golden a2a-handlers-refusals-golden
  "test/golden/hive-mcp/f07-a2a-handlers.edn"
  (refusal-cases))

(deftest every-refusal-conforms
  (doseq [[k r] (refusal-cases)]
    (is (m/validate (if (= :stream-refused k) h/StreamingResult h/HandlerResult) r)
        (str k))))

(deftest get-task-answers-a-task
  ;; No journal entry for this id, so the fall-back working task, or an
  ;; internal error when the journal namespace cannot load: both conform.
  (is (m/validate h/HandlerResult (h/handle-get-task {:id "a2a-f07-none"}))))

(deftest a-lookup-without-the-swarm-library-throws
  (is (thrown? Exception
               (h/handle-send-message {:message {:role "user" :parts text-parts}
                                       :configuration {:agentId "f07-ghost"}}))))
