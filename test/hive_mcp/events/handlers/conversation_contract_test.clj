(ns hive-mcp.events.handlers.conversation-contract-test
  "Contract facets for the conversation handlers. The envelopes are built by
   hive-agent: with it on the classpath the handlers' effects are checked
   against their contracts, without it they refuse loudly."
  (:require [clojure.test :refer [deftest is]]
            [malli.core :as m]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.conversation :as conversation]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema conversation-registration-flag-resets
  #'conversation/reset-registration!
  {:in [:cat]
   :out [:= false]
   :rel (fn [_ out] (false? out))
   :num-tests 20})

(defn- contract-out [sym]
  (-> (m/function-schemas)
      (get-in ['hive-mcp.events.handlers.conversation sym :schema])
      m/children
      second))

(defn- answer [f event]
  (try {:effects (f {} event)}
       (catch clojure.lang.ExceptionInfo e {:refused (ex-message e)})))

(deftest handlers-answer-their-contract-or-refuse
  (doseq [[sym f event] [['handle-conversation-tell conversation/handle-conversation-tell
                          [:conversation/tell {:from "a" :to "b" :message "hi"}]]
                         ['handle-conversation-ask conversation/handle-conversation-ask
                          [:conversation/ask {:from "a" :to "b" :question "q?"}]]
                         ['handle-conversation-respond conversation/handle-conversation-respond
                          [:conversation/respond {:from "b" :to "a" :ask-id "ask-1" :answer "yes"}]]]
          :let [{:keys [effects refused]} (answer f event)]]
    (if refused
      (is (string? refused) (str sym " refused without a reason"))
      (is (m/validate (contract-out sym) effects) (str sym " broke its contract")))))

(deftest register-handlers-installs-the-conversation-events
  (ev/with-clean-registry
    (is (true? (conversation/register-handlers!)))
    (is (every? ev/handler-registered?
                [:conversation/tell :conversation/ask :conversation/respond]))))
