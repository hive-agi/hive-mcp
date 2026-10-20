(ns hive-mcp.events.handlers.lifecycle-contract-test
  "Contract facets for the GC sweep handler and its dispatch helper."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.lifecycle :as lifecycle]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema lifecycle-sweep-names-the-atoms
  #'lifecycle/handle-lifecycle-sweep
  {:in [:cat [:map {:closed true}]
        [:tuple [:= :lifecycle/sweep]
         [:maybe [:map [:atom-ids {:optional true} [:vector [:enum :a :b :c]]]]]]]
   :out [:map
         [:lifecycle/sweep-fx [:map [:atom-ids [:maybe [:vector :keyword]]]]]
         [:log [:map [:level [:= :info]] [:message [:re #"^Lifecycle sweep triggered.*"]]]]]
   :rel (fn [[_ [_ data]] out]
          (let [ids (:atom-ids data)]
            (and (= {:atom-ids ids} (:lifecycle/sweep-fx out))
                 (= (str "Lifecycle sweep triggered"
                         (when (seq ids) (str " for atoms: " (pr-str ids))))
                    (get-in out [:log :message])))))
   :golden-path "test/golden/hive-mcp/events/handlers/lifecycle-sweep.edn"
   :num-tests 100})

(deftrifecta-from-schema lifecycle-registration-flag-resets
  #'lifecycle/reset-registration!
  {:in [:cat]
   :out [:= false]
   :rel (fn [_ out] (false? out))
   :num-tests 20})

(deftest trigger-sweep-dispatches-to-the-sweep-effect
  (ev/with-clean-registry
    (let [seen (atom [])]
      (is (true? (lifecycle/register-handlers!)))
      (ev/reg-fx :lifecycle/sweep-fx (fn [v] (swap! seen conj v)))
      (ev/reg-fx :log (fn [_] nil))
      (is (map? (lifecycle/trigger-sweep! {:atom-ids [:m11-probe]})))
      (is (map? (lifecycle/trigger-sweep!)))
      (is (= [{:atom-ids [:m11-probe]} {:atom-ids nil}] @seen)))))
