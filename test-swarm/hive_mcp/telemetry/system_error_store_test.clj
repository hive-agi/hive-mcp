(ns hive-mcp.telemetry.system-error-store-test
  "The :emit-system-error effect leaves a post-mortem record in the swarm store.

   The effect writes through the hive-mcp.swarm.datascript facade, so the
   subject needs a swarm store on the classpath: this lives under test-swarm/
   and runs against a fresh, thread-bound test connection, never a redefined
   datascript var."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.effects :as effects]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.datascript.connection :as conn]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- with-fresh-swarm-store [f]
  (effects/reset-registration!)
  (conn/with-test-conn (conn/create-conn) f))

(use-fixtures :each with-fresh-swarm-store)

(defn- stored-errors []
  (ds/q '[:find ?error-type ?source ?message
          :where
          [?e :error/type :system-error]
          [?e :error/error-type ?error-type]
          [?e :error/source ?source]
          [?e :error/message ?message]]))

(deftest emit-system-error-stores-in-the-swarm-store
  (testing "the store starts without error records"
    (is (empty? (stored-errors))))
  (testing ":emit-system-error stores one record for post-mortem analysis"
    (effects/register-effects!)
    ((ev/get-fx-handler :emit-system-error)
     {:error-type :restart-collision
      :source "server/start"
      :message "Port 7910 already in use"
      :context {:port 7910 :existing-pid 12345}})
    (is (= #{[:restart-collision "server/start" "Port 7910 already in use"]}
           (set (stored-errors))))))
