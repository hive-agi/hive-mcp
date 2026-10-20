(ns hive-mcp.tools.migrate.batch-test
  "DefaultBatchMigrator/sync! reads each target write through the hive-spi
   memory write contract. A contract-compliant store answers an id string on
   success (not a map with :success?), a refusal map on failure and a
   {:queued? true} ack when it only accepted the write for replay. Before this
   the loader counted every id-string answer as a failure and every
   non-throwing refusal map without :success? as ok
   (card 20261003164228-761b00c4)."
  (:require [clojure.test :refer [is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.test.stub.memory-store :as ms]
            [hive-mcp.tools.migrate.batch :as batch]))
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

(defn- sync-outcome
  "Sync one entry into a target whose add-entry! answers OUTCOME; answers the
   load counts."
  [{:keys [outcome]}]
  (let [target (ms/->stub nil {:add-entry-fn #(target-answer outcome %)})]
    (select-keys (batch/sync! (batch/current-migrator)
                              {:target target
                               :source-fn (constantly [{:id "e-1" :type "note" :content "one"}])})
                 [:loaded-ok :loaded-fail :loaded-queued])))

(deftrifecta batch-sync-counts-write-outcome
  hive-mcp.tools.migrate.batch-test/sync-outcome
  {:golden-path "test/golden/migrate/batch-sync-write-outcome.edn"
   :cases {:landed  {:outcome :landed}
           :refused {:outcome :refused}
           :errored {:outcome :errored}
           :queued  {:outcome :queued}}
   :gen (gen/hash-map :outcome (gen/elements [:landed :refused :errored :queued]))
   :pred #(and (map? %)
               (every? nat-int? (vals %))
               (= 1 (apply + (vals %))))
   :num-tests 20
   :mutations [["refusal-as-ok" (fn [_] {:loaded-ok 1 :loaded-fail 0 :loaded-queued 0})]
               ["id-as-fail" (fn [_] {:loaded-ok 0 :loaded-fail 1 :loaded-queued 0})]]
   :assert (fn []
             (is (= {:loaded-ok 1 :loaded-fail 0 :loaded-queued 0}
                    (sync-outcome {:outcome :landed}))
                 "an id string is the contract's success answer")
             (is (= {:loaded-ok 0 :loaded-fail 1 :loaded-queued 0}
                    (sync-outcome {:outcome :refused})))
             (is (= {:loaded-ok 0 :loaded-fail 1 :loaded-queued 0}
                    (sync-outcome {:outcome :errored})))
             (is (= {:loaded-ok 0 :loaded-fail 0 :loaded-queued 1}
                    (sync-outcome {:outcome :queued}))
                 "a queued write is not durable yet"))})
