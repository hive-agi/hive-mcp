(ns hive-mcp.system.layer5-contract-test
  "Contract facet for hive-mcp.system.layer5: register-stdio-surface! with the
   surface registry injected as a recording fn, so no real surface is touched."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.system.layer5 :as layer5]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- run [deps]
  (let [seen (atom [])
        reg! (fn [id s] (swap! seen conj [id (:surface/kind s)]) id)]
    {:ret (layer5/register-stdio-surface! reg! deps)
     :registered @seen}))

(golden/deftest-golden stdio-surface-registration-golden
  "test/golden/hive-mcp/system/layer5-stdio-surface.edn"
  {:with-tools-atom (run {:tools (atom {})})
   :without-tools   (run {})
   :nil-deps        (run nil)
   :nil-tools       (run {:tools nil})})

(deftest stdio-surface-registers-only-with-a-tools-atom
  (let [seen (atom [])
        reg! (fn [id s] (swap! seen conj [id (:surface/kind s)]) id)]
    (is (= :mcp-stdio (layer5/register-stdio-surface! reg! {:tools (atom {})})))
    (is (nil? (layer5/register-stdio-surface! reg! {})))
    (is (= [[:mcp-stdio :tools-atom]] @seen))))
