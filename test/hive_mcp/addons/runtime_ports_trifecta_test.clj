(ns hive-mcp.addons.runtime-ports-trifecta-test
  "Trifecta for the pure projections in `hive-mcp.addons.runtime-ports`."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.addons.runtime-ports :as ports]
            [hive-schemas.test :as hst]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(hst/deftrifecta-from-schema slave->ling-contract
  hive-mcp.addons.runtime-ports/slave->ling
  {:in        [:maybe [:map
                       [:slave/id {:optional true} :string]
                       [:slave/status {:optional true} [:enum :idle :working]]
                       [:slave/project-id {:optional true} :string]]]
   :out       [:maybe :map]
   :rel       (fn [in out] (if (nil? in) (nil? out) (= (:slave/id in) (:ling/id out))))
   :num-tests 100
   :golden-path "test/golden/addons/runtime-ports-slave-ling.edn"})

(hst/deftrifecta-from-schema ling-updates->slave-contract
  hive-mcp.addons.runtime-ports/ling-updates->slave
  {:in        [:map
               [:ling/status {:optional true} [:enum :idle :working]]
               [:ling/project-id {:optional true} :string]]
   :out       :map
   :rel       (fn [in out]
                (and (= (contains? in :ling/status) (contains? out :slave/status))
                     (= (contains? in :ling/project-id) (contains? out :slave/project-id))))
   :num-tests 100
   :golden-path "test/golden/addons/runtime-ports-ling-updates.edn"})

(deftest runtime-ports-shape-test
  (testing "the port map is keyword -> callable, with the documented keys"
    (let [p (ports/runtime-ports)]
      (is (m/validate ports/Ports p))
      (is (every? #(contains? p %) [:tools/invoke :memory/store :host/shared-jvm? :extension/get]))))
  (testing "shared-jvm? is true exactly when the injected system var holds a system"
    (is (true? (ports/shared-jvm? (constantly (atom {:up true})))))
    (is (false? (ports/shared-jvm? (constantly nil)))))
  (testing "invoke-registered-tool! refuses an unknown tool"
    (is (thrown? clojure.lang.ExceptionInfo
                 (ports/invoke-registered-tool! (constantly []) "nope" {})))
    (is (= {:a 1} (ports/invoke-registered-tool!
                   (constantly [{:name "t" :handler identity}]) "t" {:a 1})))))
