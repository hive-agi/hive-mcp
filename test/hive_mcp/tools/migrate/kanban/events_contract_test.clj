(ns hive-mcp.tools.migrate.kanban.events-contract-test
  "Contract facets for the migrator's event surface: emit refuses every id
   outside the closed event-ids set, and registration is repeatable."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.migrate.kanban.events :as ev]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn emit-verdict
  "Subject: :refused when emit throws for EVENT-ID, else :dispatched."
  [event-id]
  (try (ev/emit event-id {:probe true}) :dispatched
       (catch clojure.lang.ExceptionInfo _ :refused)))

(deftrifecta-from-schema emit-admits-only-known-event-ids
  #'emit-verdict
  {:in [:or ev/EventId [:enum :kanban-mig/typo :run-done :other]]
   :out [:enum :dispatched :refused]
   :rel (fn [id out] (= out (if (contains? ev/event-ids id) :dispatched :refused)))
   :classify (fn [_ out] out)
   :classify-domain #{:dispatched :refused}
   :num-tests 100})

(golden/deftest-golden events-surface-golden
  "test/golden/hive-mcp/tools/migrate/kanban-events.edn"
  {:ids   (into (sorted-set) ev/event-ids)
   :init  (ev/init!)
   :again (ev/init!)
   :typo  (emit-verdict :kanban-mig/typo)})

(deftest registration-installs-the-log-fx
  (is (contains? (ev/register-default-fx!) :kanban-mig/log))
  (is (contains? (ev/register-default-handlers!) :kanban-mig/run-done)))
