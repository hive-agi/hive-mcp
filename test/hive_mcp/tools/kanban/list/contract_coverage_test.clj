(ns hive-mcp.tools.kanban.list.contract-coverage-test
  "Contract coverage gate for tools/kanban/list (cov-mcp m28): every public
   defn of list/plan and list/source carries an m/=> contract; there are no
   exemptions. (The tools/kanban gate also covers this directory; this one
   pins it on its own so a change here fails a gate named for it.)"
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.kanban.list.source :as source]
            [hive-schemas.coverage :as coverage]

            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private list-dir "src/hive_mcp/tools/kanban/list")

(coverage/deftest-contract-coverage tools-kanban-list-contracts
  {:paths [list-dir]})

(deftest tools-kanban-list-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths [list-dir]})]
    (is (= 12 (count (:universe report))) "list/plan 11 + list/source 1")
    (is (= 12 (count (:covered report))))))

(deftest seq-source-contract-is-registered
  (is (some? (get-in (m/function-schemas) ['hive-mcp.tools.kanban.list.source '->seq-source])))
  (is (some? (source/->seq-source []))))
