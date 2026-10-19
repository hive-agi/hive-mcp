(ns hive-mcp.events.handlers.saa-fx-contract-test
  "Contract facets for the SAA effect wiring: the default dispatch fn and the
   default FSM resources a run in a directory gets."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.saa-fx :as saa-fx]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Dir [:enum "/p/one" "/p/two" "/tmp/x"])

(def ^:private absent-mode
  "A dispatch mode no owner ever registers."
  ::never-registered)

(deftrifecta-from-schema default-dispatch-fn-answers-an-unknown-mode
  #'saa-fx/default-dispatch-fn
  {:in [:cat [:maybe Dir]]
   :out ifn?
   :rel (fn [_ dispatch]
          (= {:wave-id nil :result {:status :no-dispatch-for-mode :mode absent-mode}}
             (dispatch {:steps []} absent-mode "ling-1" {})))
   :num-tests 50})

(deftrifecta-from-schema default-resources-persist-plans-only-on-request
  #'saa-fx/default-resources
  {:in [:cat [:maybe Dir] [:map [:store-plan? {:optional true} [:maybe :boolean]]]]
   :out [:map [:scope-fn ifn?] [:shout-fn ifn?] [:dispatch-fn ifn?] [:clock-fn ifn?]
         [:store-plan-fn {:optional true} :some]]
   :rel (fn [[_ {:keys [store-plan?]}] out]
          (and (or (true? store-plan?) (not (contains? out :store-plan-fn)))
               (inst? ((:clock-fn out)))))
   :num-tests 50})

(deftest register-saa-fx-installs-the-four-effects
  (ev/with-clean-registry
    (is (nil? (saa-fx/register-saa-fx!)))
    (is (every? #(ifn? (ev/get-fx-handler %))
                [:saa/run-workflow :saa/tool-gate :saa/context-inject :saa/shout]))))
