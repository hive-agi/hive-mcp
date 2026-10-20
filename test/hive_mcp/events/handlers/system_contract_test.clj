(ns hive-mcp.events.handlers.system-contract-test
  "Contract facets for the system-error handler registration."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.system :as system]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema system-registration-flag-resets
  #'system/reset-registration!
  {:in [:cat]
   :out [:= false]
   :rel (fn [_ out] (false? out))
   :num-tests 20})

(deftest register-handlers-routes-system-errors-to-telemetry
  (ev/with-clean-registry
    (let [seen (atom [])
          error {:error-type :m11-probe :source "test" :message "boom"}]
      (is (true? (system/register-handlers!)))
      (ev/reg-fx :emit-system-error (fn [v] (swap! seen conj v)))
      (ev/dispatch-sync [:system/error error])
      (is (= [error] @seen)))))
