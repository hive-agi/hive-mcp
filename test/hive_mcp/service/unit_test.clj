(ns hive-mcp.service.unit-test
  "Trifecta for the pure systemd unit renderer (`hive-mcp.service.unit`), plus
   a pin that the committed unit file IS the default render."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.service.unit :as unit]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema render-unit-trifecta
  hive-mcp.service.unit/render-unit
  {:in  unit/UnitOpts
   :out :string
   :rel (fn [{:keys [restart-sec start-limit-burst exec-start]} text]
          (let [lines (set (str/split-lines text))]
            (and (contains? lines "Restart=on-failure")
                 (contains? lines (str "RestartSec=" restart-sec))
                 (contains? lines (str "StartLimitBurst=" start-limit-burst))
                 (contains? lines (str "ExecStart=" exec-start)))))
   :golden-path "test/golden/service/unit.edn"
   :num-tests 100})

(deftest committed-unit-is-default-render-test
  (testing "resources/systemd/hive-mcp.service is exactly the default render"
    (is (= (unit/render-unit unit/default-opts)
           (slurp (clojure.java.io/resource "systemd/hive-mcp.service"))))))
