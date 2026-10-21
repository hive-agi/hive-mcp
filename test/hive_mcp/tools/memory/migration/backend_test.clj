(ns hive-mcp.tools.memory.migration.backend-test
  "migrate-backend! reads a write through the hive-spi memory write contract:
   a target that refuses add-entry! (a map with :success? false or an :error)
   is counted :failed, one that only queues for replay is counted :queued, and
   neither is counted :migrated. A write that returns without throwing is not
   thereby a success (card 20261003164228-761b00c4)."
  (:require [clojure.test :refer [is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.test.stub.memory-store :as ms]
            [hive-mcp.tools.memory.migration.backend :as backend]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- target-answer
  "The add-entry! answer a target store gives for OUTCOME."
  [outcome entry]
  (case outcome
    :landed  (:id entry)
    :refused {:success? false :error "store refused"}
    :errored {:error "backend down"}
    :queued  {:queued? true}))

(defn- migrate-outcome
  "Migrate one note through a target whose add-entry! answers OUTCOME; answers
   the write-related counts."
  [{:keys [outcome]}]
  (let [source (ms/->stub [{:id "e-1" :type "note" :content "one"}])
        target (ms/->stub nil {:add-entry-fn #(target-answer outcome %)})]
    (select-keys (backend/migrate-backend! source target)
                 [:migrated :failed :queued :errors :skipped])))

(deftrifecta migrate-backend-counts-write-outcome
  hive-mcp.tools.memory.migration.backend-test/migrate-outcome
  {:golden-path "test/golden/memory/migrate-backend-write-outcome.edn"
   :cases {:landed  {:outcome :landed}
           :refused {:outcome :refused}
           :errored {:outcome :errored}
           :queued  {:outcome :queued}}
   :gen (gen/hash-map :outcome (gen/elements [:landed :refused :errored :queued]))
   :pred #(and (map? %)
               (every? nat-int? (vals %))
               (= 1 (+ (:migrated %) (:failed %) (:queued %) (:errors %) (:skipped %))))
   :num-tests 20
   :mutations [["refusal-as-migrated" (fn [_] {:migrated 1 :failed 0 :queued 0 :errors 0 :skipped 0})]
               ["refusal-dropped" (fn [_] {:migrated 0 :failed 0 :queued 0 :errors 0 :skipped 0})]]
   :assert (fn []
             (is (= {:migrated 1 :failed 0 :queued 0 :errors 0 :skipped 0}
                    (migrate-outcome {:outcome :landed})))
             (is (= {:migrated 0 :failed 1 :queued 0 :errors 0 :skipped 0}
                    (migrate-outcome {:outcome :refused}))
                 "a refusal map is a failure, not a migration")
             (is (= {:migrated 0 :failed 1 :queued 0 :errors 0 :skipped 0}
                    (migrate-outcome {:outcome :errored})))
             (is (= {:migrated 0 :failed 0 :queued 1 :errors 0 :skipped 0}
                    (migrate-outcome {:outcome :queued}))
                 "a queued write is not durable yet"))})
