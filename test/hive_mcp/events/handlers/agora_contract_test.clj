(ns hive-mcp.events.handlers.agora-contract-test
  "Contract facets for the Agora dialogue handlers. The timeout handler is
   pure and gets the full trifecta. The dialogue-reading handlers depend on a
   dialogue store hive-agent provides; their contract is checked on what they
   answer in this build."
  (:require [clojure.test :refer [deftest is]]
            [malli.core :as m]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.agora :as agora]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:string {:min 1 :max 10}])

(deftrifecta-from-schema agora-timeout-names-the-reason
  #'agora/handle-agora-timeout
  {:in [:cat [:map {:closed true}]
        [:tuple [:= :agora/timeout]
         [:map [:dialogue-id Id]
          [:reason {:optional true} [:enum :max-turns :inactivity]]
          [:turn-count {:optional true} [:int {:min 0 :max 60}]]]]]
   :out [:map {:closed true} [:log [:map [:level [:= :warn]] [:message :string]]]]
   :rel (fn [[_ [_ {:keys [dialogue-id reason turn-count]}]] out]
          (= (str "Agora dialogue " dialogue-id " timed out"
                  " (reason: " (name (or reason :unknown)) ")"
                  (when turn-count (str ", turns: " turn-count)))
             (get-in out [:log :message])))
   :golden-path "test/golden/hive-mcp/events/handlers/agora-timeout.edn"
   :num-tests 100})

(defn- contract-out [sym]
  (-> (m/function-schemas) (get-in ['hive-mcp.events.handlers.agora sym :schema]) m/children second))

(deftest dispatch-next-answers-a-log-whatever-the-store-says
  (let [out (agora/handle-dispatch-next {} [:agora/dispatch-next {:dialogue-id "m11-absent-dialogue"}])]
    (is (m/validate (contract-out 'handle-dispatch-next) out))
    (is (nil? (:swarm-send-prompt out)) "an unknown dialogue has no next participant")))

(deftest register-handlers-installs-the-agora-events
  (ev/with-clean-registry
    (let [events [:agora/turn-dispatched :agora/timeout :agora/turn-completed
                  :agora/turn-response :agora/dispatch-next :agora/consensus]
          reg (agora/register-handlers!)]
      (is (every? #(contains? reg %) events))
      (is (every? ev/handler-registered? events)))))
