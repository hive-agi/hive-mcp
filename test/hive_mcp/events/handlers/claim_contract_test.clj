(ns hive-mcp.events.handlers.claim-contract-test
  "Contract facets for the file-claim handlers. The notify handler is pure.
   The release handler is driven only on its no-file path, which never reads
   the swarm store."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.claim :as claim]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:string {:min 1 :max 10}])
(def ^:private Path [:enum "src/core.clj" "src/a.clj" "test/b_test.clj"])

(deftrifecta-from-schema notify-waiting-wakes-and-unparks-the-ling
  #'claim/handle-claim-notify-waiting
  {:in [:cat [:map {:closed true}]
        [:tuple [:= :claim/notify-waiting]
         [:map [:target-agent-id Id] [:file Path]
          [:task-id {:optional true} [:maybe Id]]
          [:released-by {:optional true} Id]
          [:parked? {:optional true} :boolean]]]]
   :out [:map
         [:targeted-shout [:map [:target-agent-id Id] [:sender-id [:maybe Id]]
                           [:event-type [:= :file-available]]
                           [:data [:map [:file Path] [:task-id [:maybe Id]] [:message :string]]]]]
         [:ds-transact {:optional true}
          [:tuple [:tuple [:= :db/retractEntity] [:tuple [:= :wait-queue/id] :string]]]]]
   :rel (fn [[_ [_ {:keys [target-agent-id file task-id released-by parked?]}]] out]
          (and (= {:target-agent-id target-agent-id :sender-id released-by :event-type :file-available
                   :data {:file file :task-id task-id :message (str "File " file " is now available")}}
                  (:targeted-shout out))
               (= (when parked?
                    [[:db/retractEntity [:wait-queue/id (str "wait:" target-agent-id ":" file)]]])
                  (:ds-transact out))))
   :golden-path "test/golden/hive-mcp/events/handlers/claim-notify-waiting.edn"
   :num-tests 100})

(deftrifecta-from-schema file-released-without-a-file-notifies-nobody
  #'claim/handle-claim-file-released
  {:in [:cat [:map [:db-snapshot [:= :m11-snapshot]]]
        [:tuple [:= :claim/file-released] [:map [:released-by Id]]]]
   :out [:map {:closed true} [:log [:map [:level [:= :debug]] [:message :string]]]]
   :rel (fn [[_ [_ {:keys [released-by]}]] out]
          (= (str "File  released by " released-by " - no waiting lings")
             (get-in out [:log :message])))
   :golden-path "test/golden/hive-mcp/events/handlers/claim-file-released.edn"
   :num-tests 50})

(deftest register-handlers-installs-the-claim-events
  (ev/with-clean-registry
    (is (nil? (claim/register-handlers!)))
    (is (every? ev/handler-registered? [:claim/file-released :claim/notify-waiting]))))
