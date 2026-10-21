(ns hive-mcp.tools.swarm.state-contract-test
  "Contract facets for unified swarm state: merging hivemind status into an
   empty or id-less slaves list never reaches the DataScript store, and the
   WorkingStatus enum is closed."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.swarm.state :as sst]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema id-less-slaves-pass-through
  #'sst/merge-hivemind-into-slaves
  {:in [:cat [:vector {:max 3} [:map [:name [:enum "a" "b"]] [:status [:enum :idle :working]]]]]
   :out [:maybe [:vector :map]]
   :rel (fn [[slaves] out] (if (seq slaves) (= out slaves) (nil? out)))
   :classify (fn [[slaves] _] (if (seq slaves) :some :none))
   :classify-domain #{:some :none}
   :num-tests 100})

(golden/deftest-golden state-merge-golden
  "test/golden/hive-mcp/tools/swarm/state-merge.edn"
  {:empty   (sst/merge-hivemind-into-slaves [])
   :nil     (sst/merge-hivemind-into-slaves nil)
   :id-less (sst/merge-hivemind-into-slaves [{:name "a" :status :idle}])})

(deftest working-status-is-closed
  (is (m/validate sst/WorkingStatus "working"))
  (is (not (m/validate sst/WorkingStatus "error"))))
