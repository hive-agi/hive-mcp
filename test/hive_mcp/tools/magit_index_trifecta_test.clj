(ns hive-mcp.tools.magit-index-trifecta-test
  "Golden, property and mutation coverage for the explicit-files index guard."
  (:require [clojure.test.check.generators :as gen]
            [hive-mcp.tools.magit :as magit]
            [hive-test.trifecta :refer [deftrifecta]]))

(def ^:private gen-scope
  (gen/let [own (gen/elements ["a.clj" "src/a.clj" "src/"])
            outsider (gen/elements ["other.clj" "elsewhere/b.clj"])]
    [[own] [outsider]]))

(deftrifecta explicit-files-index-scope
  hive-mcp.tools.magit/staged-outside-files
  {:golden-path "test/golden/hive-mcp/tools/magit-index-scope.edn"
   :apply? true
   :cases {:foreign [["own.clj"] ["own.clj" "foreign.clj"]]
           :only-own [["own.clj"] ["own.clj"]]
           :nested-dir [["src/"] ["src/a.clj" "other/a.clj"]]
           :rename-sides [["old.clj"] ["old.clj" "new.clj"]]
           :dot-prefix [["./own.clj"] ["own.clj"]]}
   :gen gen-scope
   :pred (fn [result] (every? string? result))
   :property-type :pred-fn
   :num-tests 100
   :mutations [["trust-entire-index" (fn [_files _staged] [])]
               ["reject-entire-index" (fn [_files staged] staged)]]})
