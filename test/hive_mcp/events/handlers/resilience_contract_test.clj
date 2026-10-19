(ns hive-mcp.events.handlers.resilience-contract-test
  "Contract facets for the dim-mismatch advisory handler."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.resilience :as resilience]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Mismatch
  [:map [:message [:string {:min 1 :max 20}]]
   [:details {:optional true} [:map [:expected [:int {:min 1 :max 4096}]]
                               [:actual [:int {:min 1 :max 4096}]]]]
   [:ex-class {:optional true} [:maybe [:enum "clojure.lang.ExceptionInfo"]]]])

(deftrifecta-from-schema dim-mismatch-logs-the-structured-fields
  #'resilience/handle-dim-mismatch
  {:in [:cat [:map {:closed true}] [:tuple [:= :resilience/dim-mismatch] Mismatch]]
   :out [:map {:closed true}
         [:log [:map {:closed true}
                [:level [:= :warn]] [:event [:= :resilience/dim-mismatch]]
                [:message :string] [:ex-class [:maybe :string]] [:details [:maybe :map]]
                [:data Mismatch]]]]
   :rel (fn [[_ [_ {:keys [message details ex-class] :as data}]] out]
          (= {:level :warn :event :resilience/dim-mismatch :message message
              :ex-class ex-class :details details :data data}
             (:log out)))
   :golden-path "test/golden/hive-mcp/events/handlers/dim-mismatch.edn"
   :num-tests 100})

(deftrifecta-from-schema resilience-registration-flag-resets
  #'resilience/reset-registration!
  {:in [:cat]
   :out [:= false]
   :rel (fn [_ out] (false? out))
   :num-tests 20})

(deftest register-handlers-installs-the-dim-mismatch-event
  (ev/with-clean-registry
    (is (true? (resilience/register-handlers!)))
    (is (ev/handler-registered? :resilience/dim-mismatch))))
