(ns hive-mcp.events.handlers.session-contract-test
  "Contract facets for the session lifecycle handlers. The coeffects carry the
   agent context, so no generated case falls back to the environment or the
   clock."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.session :as session]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:string {:min 1 :max 10}])
(def ^:private InfoLog [:map [:level [:= :info]] [:message :string]])

(deftrifecta-from-schema session-end-queues-the-wrap
  #'session/handle-session-end
  {:in [:cat [:map [:agent-context [:map [:agent-id Id] [:session-id Id]]]]
        [:tuple [:= :session/end] [:map [:slave-id {:optional true} Id] [:reason {:optional true} Id]]]]
   :out [:map
         [:log InfoLog]
         [:wrap-notify [:map [:agent-id Id] [:session-id Id]
                        [:stats [:map [:triggered-by [:= "auto-wrap"]] [:reason :string]]]]]]
   :rel (fn [[cofx [_ {:keys [slave-id reason]}]] out]
          (= {:agent-id (or slave-id (get-in cofx [:agent-context :agent-id]))
              :session-id (get-in cofx [:agent-context :session-id])
              :stats {:triggered-by "auto-wrap" :reason (or reason "session-end")}}
             (:wrap-notify out)))
   :golden-path "test/golden/hive-mcp/events/handlers/session-end.edn"
   :num-tests 100})

(deftrifecta-from-schema session-wrap-crystallizes-the-session
  #'session/handle-session-wrap
  {:in [:cat [:map [:agent-context [:map [:agent-id Id]]]]
        [:tuple [:= :session/wrap]
         [:map [:session-id Id] [:slave-id {:optional true} Id] [:project {:optional true} Id]
          [:triggered-by {:optional true} [:enum "auto-wrap" "manual"]]
          [:reason {:optional true} Id]]]]
   :out [:map
         [:log InfoLog]
         [:wrap-crystallize [:map [:agent-id Id] [:session-id Id] [:project [:maybe Id]]]]]
   :rel (fn [[cofx [_ {:keys [session-id slave-id project triggered-by reason]}]] out]
          (and (= {:agent-id (or slave-id (get-in cofx [:agent-context :agent-id]))
                   :session-id session-id :project project}
                  (:wrap-crystallize out))
               (= (str "Auto-wrap crystallizing session: " session-id
                       " (triggered-by: " (or triggered-by "unknown")
                       ", reason: " (or reason "session-wrap") ")")
                  (get-in out [:log :message]))))
   :golden-path "test/golden/hive-mcp/events/handlers/session-wrap.edn"
   :num-tests 100})

(deftest register-handlers-installs-the-session-events
  (ev/with-clean-registry
    (let [reg (session/register-handlers!)]
      (is (every? #(contains? reg %) [:session/end :session/wrap]))
      (is (every? ev/handler-registered? [:session/end :session/wrap])))))
