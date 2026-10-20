(ns hive-mcp.events.handlers-contract-test
  "Contract facets for the event handler facade: what it expects, what it
   verifies and what it registers."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers :as handlers]
            [hive-mcp.events.handlers.claim :as claim]
            [hive-mcp.events.handlers.system :as system]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema expected-events-cover-the-kernel
  #'handlers/expected-events
  {:in [:cat]
   :out [:set {:min 8} :keyword]
   :rel (fn [_ out] (set/subset? handlers/kernel-events out))
   :num-tests 20})

(deftrifecta-from-schema facade-registration-flag-resets
  #'handlers/reset-registration!
  {:in [:cat]
   :out [:maybe [:= false]]
   :rel (fn [_ out] (not (true? out)))
   :num-tests 20})

(deftest verify-handlers-partitions-the-registry
  (ev/with-clean-registry
    (claim/register-handlers!)
    (system/register-handlers!)
    (ev/reg-event :m11/undeclared [] (fn [_ _] {}))
    (let [{:keys [registered missing extra]} (handlers/verify-handlers!)
          expected (handlers/expected-events)]
      (is (contains? registered :claim/file-released))
      (is (= missing (set/difference expected registered)))
      (is (= extra (set/difference registered expected)))
      (is (contains? extra :m11/undeclared))
      (is (not (contains? missing :system/error))))))

(deftest register-handlers-leaves-nothing-expected-missing
  (ev/with-clean-registry
    (is (true? (handlers/register-handlers!)))
    (is (empty? (:missing (handlers/verify-handlers!))))))
