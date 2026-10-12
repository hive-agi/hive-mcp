(ns hive-mcp.tools.consolidated.git-batch-commit-test
  "batch-commit refuses :parallel (20260906164520-50e229df).

   Every operation stages its own files and then commits through ONE git index,
   so a pmap over operations can commit another operation's paths. The pure
   guard `parallel-refusal` decides; the handler only reports it."
  (:require [clojure.string :as str]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.tools.consolidated.git :as git-tool]))

(deftrifecta parallel-refusal-contract
  hive-mcp.tools.consolidated.git/parallel-refusal
  {:golden-path "test/golden/git/batch-commit-parallel-refusal.edn"
   :cases       {:absent         {:operations [{:message "m"}]}
                 :false          {:operations [{:message "m"}] :parallel false}
                 :nil            {:operations [{:message "m"}] :parallel nil}
                 :true           {:operations [{:message "m"}] :parallel true}
                 :truthy-string  {:operations [{:message "m"}] :parallel "true"}}
   :gen         (gen/let [p (gen/elements [nil false true "true" 1])]
                  {:operations [{:message "m"}] :parallel p})
   :pred        (fn [r]
                  (or (nil? r)
                      (and (string? r)
                           (str/starts-with? r "batch-commit/parallel-unsupported"))))
   :num-tests   100
   :mutations   [["never-refuses" (fn [_] nil)]
                 ["always-refuses"
                  (fn [_] "batch-commit/parallel-unsupported: x")]
                 ["untyped-message" (fn [_] "parallel not allowed")]]})
