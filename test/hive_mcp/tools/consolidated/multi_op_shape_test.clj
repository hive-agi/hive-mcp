(ns hive-mcp.tools.consolidated.multi-op-shape-test
  "Trifecta pinning for the multi batch shape guard (kanban
   20260915163543-5aac729f): a string entry in `operations` used to leak
   'nth not supported on this type: Character'. The guard names every
   non-map entry by index instead.

   Mutants are self-contained; none references the subject var."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.tools.consolidated.multi :as multi]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private gen-op
  (gen/one-of [(gen/return {:id "a" :tool "memory" :command "search"})
               gen/string-alphanumeric
               (gen/vector gen/small-integer 0 3)
               gen/small-integer
               (gen/return nil)]))

(deftrifecta malformed-operations-contract
  hive-mcp.tools.consolidated.multi/malformed-operations
  {:golden-path "test/golden/multi/malformed-operations.edn"
   :cases       {:all-maps     [{:id "a" :tool "memory" :command "search"}]
                 :one-string   ["memory search q"
                                {:id "a" :tool "memory" :command "search"}]
                 :dsl-tuple    [["m/" {:q "x"}]]
                 :mixed        [{:id "a"} "x" nil 3]}
   :gen         (gen/vector gen-op 0 6)
   :pred        vector?
   :num-tests   200
   :mutations   [["accepts-everything" (constantly [])]
                 ["flags-everything"
                  (fn [ops] (vec (map-indexed (fn [i v] {:index i :value v}) ops)))]
                 ["loses-index"
                  (fn [ops] (vec (keep #(when-not (map? %) {:index 0 :value %}) ops)))]]
   :assert      (fn []
                  (is (= [] (multi/malformed-operations
                             [{:id "a" :tool "memory" :command "search"}])))
                  (is (= [{:index 0 :value "memory search q"}]
                         (multi/malformed-operations
                          ["memory search q" {:id "a"}])))
                  (is (= [1 3] (mapv :index (multi/malformed-operations
                                             [{} "x" {} ["m/" {}]])))))})

(deftest message-names-the-offending-entry
  (let [msg (multi/malformed-operations-message
             (multi/malformed-operations ["memory search q" {:id "a"}]))]
    (is (str/includes? msg "index 0"))
    (is (str/includes? msg "\"memory search q\""))
    (is (str/includes? msg "dsl"))))
