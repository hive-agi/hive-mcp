(ns hive-mcp.events.handlers.ling-contract-test
  "Contract facets for the ling lifecycle handlers. The completed and
   ready-for-wrap arms are both generated: an event with neither :slave-id nor
   :agent-id takes the dropped arm."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.ling :as ling]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:string {:min 1 :max 10}])
(def ^:private NoCoeffects [:map {:closed true}])
(def ^:private WarnLog [:map [:level [:= :warn]] [:message :string]])

;; No golden: :slave/created-at reads the clock.
(deftrifecta-from-schema ling-started-registers-the-slave
  #'ling/handle-ling-started
  {:in [:cat NoCoeffects
        [:tuple [:= :ling/started]
         [:map [:slave-id {:optional true} Id] [:name {:optional true} Id]
          [:presets {:optional true} [:vector [:enum "tdd" "clarity"]]]
          [:cwd {:optional true} Id] [:depth {:optional true} [:int {:min 1 :max 4}]]]]]
   :out [:map
         [:log [:map [:level [:= :info]] [:message :string]]]
         [:ds-transact [:tuple [:map [:slave/id :string] [:slave/name :string]
                                [:slave/status [:= :starting]] [:slave/depth [:int {:min 1}]]
                                [:slave/tasks-completed [:= 0]] [:slave/created-at inst?]
                                [:slave/presets [:vector :string]] [:slave/cwd [:maybe :string]]]]]]
   :rel (fn [[_ [_ {:keys [slave-id name presets cwd depth]}]] out]
          (let [row (first (:ds-transact out))
                id (or slave-id name "unknown-ling")]
            (= {:slave/id id :slave/name (or name id) :slave/depth (or depth 1)
                :slave/presets (vec (or presets [])) :slave/cwd cwd}
               (select-keys row [:slave/id :slave/name :slave/depth :slave/presets :slave/cwd]))))
   :num-tests 100})

(def ^:private LingIds
  [:map [:slave-id {:optional true} Id] [:agent-id {:optional true} Id]
   [:result {:optional true} Id] [:reason {:optional true} Id]
   [:session-id {:optional true} Id]])

(deftrifecta-from-schema ling-completed-chains-session-end
  #'ling/handle-ling-completed
  {:in [:cat NoCoeffects [:tuple [:= :ling/completed] LingIds]]
   :out [:or
         [:map [:log [:map [:level [:= :info]] [:message :string]]]
          [:dispatch [:tuple [:= :session/end] [:map [:slave-id Id] [:reason :string]]]]]
         [:map {:closed true} [:log WarnLog]]]
   :rel (fn [[_ [_ {:keys [slave-id agent-id reason]}]] out]
          (if-let [id (or slave-id agent-id)]
            (= [:session/end {:slave-id id :reason (or reason "ling-completed")}] (:dispatch out))
            (and (nil? (:dispatch out)) (= :warn (get-in out [:log :level])))))
   :golden-path "test/golden/hive-mcp/events/handlers/ling-completed.edn"
   :num-tests 100})

(deftrifecta-from-schema ling-ready-for-wrap-chains-session-wrap
  #'ling/handle-ling-ready-for-wrap
  {:in [:cat NoCoeffects [:tuple [:= :ling/ready-for-wrap] LingIds]]
   :out [:or
         [:map [:log [:map [:level [:= :info]] [:message :string]]]
          [:dispatch [:tuple [:= :session/wrap]
                      [:map [:session-id [:maybe Id]] [:slave-id Id]
                       [:triggered-by [:= "auto-wrap"]] [:reason :string]]]]]
         [:map {:closed true} [:log WarnLog]]]
   :rel (fn [[_ [_ {:keys [slave-id agent-id reason session-id]}]] out]
          (if-let [id (or slave-id agent-id)]
            (= [:session/wrap {:session-id session-id :slave-id id :triggered-by "auto-wrap"
                               :reason (or reason "task-completed")}]
               (:dispatch out))
            (and (nil? (:dispatch out)) (= :warn (get-in out [:log :level])))))
   :golden-path "test/golden/hive-mcp/events/handlers/ling-ready-for-wrap.edn"
   :num-tests 100})

(deftest register-handlers-installs-the-ling-events
  (ev/with-clean-registry
    (let [events [:ling/started :ling/completed :ling/ready-for-wrap]
          reg (ling/register-handlers!)]
      (is (every? #(contains? reg %) events))
      (is (every? ev/handler-registered? events)))))
