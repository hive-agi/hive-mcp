(ns hive-mcp.tools.olympus-contract-test
  "Contract facets for the Olympus grid handlers. Only the arms that read no
   swarm state are driven: restore, and the refusal when nothing names a
   ling. The status, arrange and tab arms read and write the swarm store and
   are covered by their contracts under instrumentation of callers."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.olympus :as olympus]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private FocusParams
  "Restore, or nothing that names a ling: neither arm touches the store."
  [:or [:map [:restore [:= true]] [:position {:optional true} [:enum 1 2]]]
   [:map [:restore {:optional true} [:enum false nil]]]])

(deftrifecta-from-schema focus-restores-or-refuses
  #'olympus/handle-olympus-focus
  {:in [:cat FocusParams]
   :out olympus/FocusResult
   :rel (fn [[p] out]
          (if (:restore p)
            (= {:success true :focused-ling nil} out)
            (= {:success false :error "Must specify ling-id, position, or restore"} out)))
   :classify (fn [_ out] (if (:success out) :restored :refused))
   :classify-domain #{:restored :refused}
   :num-tests 100})

(golden/deftest-golden focus-golden
  "test/golden/hive-mcp/tools/olympus-focus.edn"
  {:restore (olympus/handle-olympus-focus {:restore true})
   :nothing (olympus/handle-olympus-focus {})})

(deftest focus-arms-are-disjoint
  (is (m/validate olympus/FocusResult {:success false :error "x"}))
  (is (not (m/validate olympus/FocusResult {:success false :focused-ling "x"}))))
