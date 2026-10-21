(ns hive-mcp.saa.registry.dispatch-modes-test
  "Facets for the mode -> Act dispatch fn child registry: register!
   keeps the owner index consistent with its outcome (property over generated
   owners and ids, every outcome classified), deregister-by-owner! removes
   exactly that owner's ids, and a golden over the whole lifecycle.
   support/with-fresh-registry snapshots every SAA child registry and
   restores it afterwards."
  (:require [clojure.test :refer [use-fixtures]]
            [hive-mcp.saa.registry.child-support :as child]
            [hive-mcp.saa.registry.dispatch-modes :as reg]
            [hive-mcp.saa.support :as support]
            [hive-mcp.saa.types :as types]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each support/with-fresh-registry)

(deftrifecta-from-schema register-keeps-owner-index
  #'reg/register!
  {:in child/RegisterArgs
   :out types/RegisterOutcome
   :rel (child/index-consistency-rel reg/snapshot)
   :classify (fn [_ out] out)
   :classify-domain types/RegisterOutcome
   :num-tests 200})

(deftrifecta-from-schema deregister-removes-the-owners-ids
  #'reg/deregister-by-owner!
  {:in [:cat child/Owner]
   :out [:set :any]
   :rel (child/deregister-rel reg/snapshot)})

(golden/deftest-golden dispatch-modes-lifecycle-golden
  "test/golden/hive-mcp/saa/registry/dispatch-modes-lifecycle.edn"
  (child/lifecycle {:register! reg/register! :lookup reg/lookup :dereg reg/deregister-by-owner!
                    :ids reg/all-ids :snapshot reg/snapshot :restore! reg/restore!
                    :reset! reg/reset-for-test!}))
