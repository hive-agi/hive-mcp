(ns hive-mcp.tools.swarm.batch-runner-contract-test
  "Contract facets for the swarm batch runner: an op naming no swarm tool
   fails alone, and the summary counts every op."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.batch.protocol :as proto]
            [hive-mcp.tools.swarm.batch :as sb]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema unknown-tools-fail-and-are-counted
  #'sb/run-batch
  {:in [:cat [:vector {:max 3} [:map [:id [:enum "a" "b" "c"]]
                                [:tool [:enum "no-such" "zz"]]
                                [:command [:= "x"]]]]]
   :out sb/BatchResult
   :rel (fn [[ops] out]
          (and (= (count ops) (get-in out [:summary :total] 0))
               (zero? (get-in out [:summary :success] 0))
               (= (empty? ops) (:success out))
               (or (empty? ops) (apply distinct? (map :id ops))
                   ;; a batch with a repeated id is refused before any wave runs
                   (zero? (get-in out [:summary :waves] 0)))))
   :classify (fn [[ops] _] (cond (empty? ops) :empty
                                 (apply distinct? (map :id ops)) :distinct
                                 :else :duplicate-ids))
   :classify-domain #{:empty :distinct :duplicate-ids}
   :classify-floor 3
   :num-tests 60})

(golden/deftest-golden swarm-batch-golden
  "test/golden/hive-mcp/tools/swarm/batch-runner.edn"
  {:empty   (sb/run-batch [])
   :unknown (sb/run-batch [{:id "a" :tool "no-such" :command "x"}])})

(deftest the-runner-is-batchable
  (is (satisfies? proto/Batchable (sb/make-swarm-runner))))
