(ns hive-mcp.tools.consolidated.multi-ref-docs-test
  "The cross-op ref rule must be documented on the surface it applies to.

   Positional $0..$n-1 exists only because the DSL compiler names its ops that
   way. The `operations` surface matches node-id params against the ids the
   caller DECLARED, and a $-prefixed value naming none of them fails the batch.
   The rule used to be described only under `dsl`, so callers of `operations`
   wrote $0 and had the whole batch refused."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.consolidated.multi :as multi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- description [param]
  (get-in multi/tool-def [:inputSchema :properties param :description]))

(deftest operations-documents-declared-id-refs
  (let [d (description "operations")]
    (testing "it says node-id params resolve against declared ids"
      (is (str/includes? d "declared id"))
      (is (str/includes? d "$ref:<id>.data.id"))
      (is (str/includes? d "depends_on")))
    (testing "it says positional $N is DSL-only and dangling refs fail the batch"
      (is (str/includes? d "NO positional $N"))
      (is (str/includes? d "DSL-only"))
      (is (str/includes? d "fails the whole batch")))))

(deftest dsl-scopes-positional-ids-to-itself
  (let [d (description "dsl")]
    (is (str/includes? d "DSL only: the compiler names its ops $0..$n-1"))
    (is (not (str/includes? d "Ops are numbered from $0"))
        "the old surface-agnostic wording must be gone")))
