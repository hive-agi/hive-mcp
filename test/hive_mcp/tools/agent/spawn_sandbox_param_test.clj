(ns hive-mcp.tools.agent.spawn-sandbox-param-test
  "The `sandbox` spawn param fails CLOSED: a value that is not a boolean (or
   \"true\"/\"false\") is refused, never read as false (no sandbox)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.tools.agent.spawn :as spawn]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- outcome
  "nil/true/false as returned, or :refused when the value is rejected."
  [v]
  (try (spawn/normalize-sandbox v)
       (catch clojure.lang.ExceptionInfo _ :refused)))

(deftest darkmatter-never-means-no-sandbox
  (testing "a backend name is refused, not coerced to false"
    (is (= :refused (outcome "darkmatter")))
    (is (= :refused (outcome "bwrap")))
    (is (= :refused (outcome {:sandbox/backend :darkmatter})))
    (is (= :refused (outcome :darkmatter)))
    (is (= :refused (outcome "")))
    (is (= :refused (outcome "TRUE")))))

(deftest accepted-values
  (is (nil? (outcome nil)))
  (is (true? (outcome true)))
  (is (true? (outcome "true")))
  (is (false? (outcome false)))
  (is (false? (outcome "false"))))

(def ^:private gen-sandbox
  (gen/one-of [(gen/elements [nil true false "true" "false" "darkmatter" "bwrap"])
               gen/string-alphanumeric
               gen/keyword
               (gen/map gen/keyword gen/keyword)]))

(deftrifecta sandbox-param-fails-closed
  #'hive-mcp.tools.agent.spawn/sandbox-decision
  {:golden-path "test/golden/spawn_sandbox_param.edn"
   :cases       {:nil        nil
                 :true       true
                 :false      false
                 :str-true   "true"
                 :str-false  "false"
                 :darkmatter "darkmatter"
                 :map        {:sandbox/backend :darkmatter}}
   :gen         gen-sandbox
   :pred        #(contains? #{nil true false :refused} %)
   :num-tests   200
   :mutations   [["string-not-true-is-false"
                  (fn [v] (if (string? v) (= "true" v) (boolean v)))]
                 ["anything-else-is-true"
                  (fn [v] (if (contains? #{false "false"} v) false (some? v)))]
                 ["unknown-is-nil"
                  (fn [v] (case v (true "true") true (false "false") false nil))]]})
