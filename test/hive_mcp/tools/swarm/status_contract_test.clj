(ns hive-mcp.tools.swarm.status-contract-test
  "Contract facets for swarm status: the lings formatter over both input
   shapes, and the host-less refusals of broadcast / presets / status."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.tools.swarm.status :as sts]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema registry-lings-gain-an-age
  #'sts/format-lings-for-response
  {:in [:cat [:map-of [:enum "a" "b"] [:map [:name [:enum "x" "y"]]
                                       [:spawned-at [:int {:min 0 :max 1000}]]]]]
   :out [:map-of :string [:map [:age-minutes :int]]]
   :rel (fn [[lings] out]
          (and (= (set (keys lings)) (set (keys out)))
               (every? (fn [[k v]] (= (:name v) (get-in lings [k :name]))) out)))
   :num-tests 100})

(golden/deftest-golden status-hostless-golden
  "test/golden/hive-mcp/tools/swarm/status-hostless.edn"
  {:elisp-list (sts/format-lings-for-response [{:slave_id "s1" :name "n" :status "idle"}])
   :other      (sts/format-lings-for-response "neither")
   :broadcast  (sts/handle-swarm-broadcast {:prompt "x"})
   :presets    (sts/handle-swarm-list-presets {})
   :status     (sts/handle-swarm-status {})})

(deftest no-host-means-no-elisp-lings
  (when-not (core/swarm-addon-available?)
    (is (nil? (sts/query-elisp-lings)))))
