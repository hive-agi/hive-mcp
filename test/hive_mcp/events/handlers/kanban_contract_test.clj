(ns hive-mcp.events.handlers.kanban-contract-test
  "Contract facets for the kanban event handlers."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.kanban :as kanban]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:string {:min 1 :max 10}])
(def ^:private NoCoeffects [:map {:closed true}])

;; No golden: the note content and :kanban/completed-at read the clock.
(deftrifecta-from-schema kanban-done-records-the-completion
  #'kanban/handle-kanban-done
  {:in [:cat NoCoeffects
        [:tuple [:= :kanban/done]
         [:map [:task-id Id] [:task-title {:optional true} Id] [:agent-id Id]
          [:result {:optional true} [:enum "success" "failure"]]]]]
   :out [:map
         [:log [:map [:level [:= :info]] [:message :string]]]
         [:memory-write [:map [:type [:= "note"]] [:content :string]
                         [:tags [:= ["kanban-progress" "ephemeral"]]]
                         [:duration [:= "ephemeral"]] [:agent-id Id]]]
         [:ds-transact [:tuple [:map [:kanban/task-id Id] [:kanban/status [:= :done]]
                                [:kanban/completed-by Id] [:kanban/completed-at inst?]]]]]
   :rel (fn [[_ [_ {:keys [task-id task-title agent-id result]}]] out]
          (let [content (get-in out [:memory-write :content])
                row (first (:ds-transact out))]
            (and (= task-id (:kanban/task-id row))
                 (= agent-id (:kanban/completed-by row) (get-in out [:memory-write :agent-id]))
                 (str/includes? content (str "- **Title:** " (or task-title "Untitled")))
                 (str/includes? content (str "- **Result:** " (or result "completed")))
                 (= (str "Kanban task done: " (or task-title task-id)) (get-in out [:log :message])))))
   :num-tests 100})

(deftrifecta-from-schema kanban-sync-prefers-the-event-project
  #'kanban/handle-kanban-sync
  {:in [:cat [:map [:agent-context {:optional true} [:map [:project Id]]]]
        [:tuple [:= :kanban/sync] [:map [:project {:optional true} Id]]]]
   :out [:map
         [:log [:map [:level [:= :info]] [:message :string]]]
         [:kanban-sync [:map [:project :string] [:direction [:= :bidirectional]]]]]
   :rel (fn [[cofx [_ data]] out]
          (= (or (:project data) (get-in cofx [:agent-context :project]) "unknown-project")
             (get-in out [:kanban-sync :project])))
   :golden-path "test/golden/hive-mcp/events/handlers/kanban-sync.edn"
   :num-tests 100})

(deftest register-handlers-installs-the-kanban-events
  (ev/with-clean-registry
    (let [reg (kanban/register-handlers!)]
      (is (every? #(contains? reg %) [:kanban/done :kanban/sync]))
      (is (every? ev/handler-registered? [:kanban/done :kanban/sync])))))
