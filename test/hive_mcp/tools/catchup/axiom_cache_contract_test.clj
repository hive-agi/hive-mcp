(ns hive-mcp.tools.catchup.axiom-cache-contract-test
  "catchup.axiom-cache over the IMemoryStore PORT (seeded StubMemoryStore)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.tools.catchup.axiom-cache :as axc]
            [hive-mcp.tools.catchup.contract-fixtures :as fx]
            [hive-mcp.tools.catchup.scope :as scope]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each fx/with-seeded-store)

(defn- ids [es] (mapv :id es))

(deftest-golden axioms-formal-and-legacy
  "test/golden/catchup/axiom-cache-query.edn"
  (fx/with-seeded-store #(mapv (fn [e] (select-keys e [:id :type :tags])) (axc/query-axioms "p"))))

(deftest axiom-cache-swr
  (testing "formal and legacy rows are unioned, newest first"
    (is (= ["e2" "e1"] (ids (axc/query-axioms "p")))))
  (testing "the scope facade forwards to the same cache"
    (is (= (axc/query-axioms "p") (scope/query-axioms "p"))))
  (testing "invalidation by project and in full"
    (is (= {} (axc/invalidate-axioms-cache! "p")))
    (axc/query-axioms "p")
    (is (= 0 (axc/evict-stale!)) "fresh entries are not evicted")
    (is (= {} (axc/invalidate-axioms-cache!)))))
