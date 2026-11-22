(ns hive-mcp.config.config-path-test
  "Trifecta for `hive-mcp.config.path/config-path`, the process's global
   config.edn (7 production readers). It is a `def` computed once at load from
   the hive.mcp.config property, then HIVE_MCP_CONFIG, then the home default,
   all through `resolve-config-path`. Tests read the def and drive the same
   resolution fn with generated inputs; nothing is redefined.

     golden    : the resolution table for the property \\ env \\ home cases
     property  : config-path is what `resolve-config-path` answers for the
                 process's own inputs; operator-config-path ignores overrides
     mutation  : resolution that ignores a non-blank override, honours a blank
                 one, or drops the home default is caught"
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.config.path :as path]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- process-inputs []
  [(or (System/getProperty "hive.mcp.config") (System/getenv "HIVE_MCP_CONFIG"))
   (System/getProperty "user.home")])

(deftest-golden config-path-resolution-golden
  "test/golden/hive-mcp/config/config-path.edn"
  (into (sorted-map)
        (for [[label env] {:override "alt/config.edn" :nil nil :empty "" :blank "   "}]
          [label (path/resolve-config-path env "/home/u")])))

(deftest config-path-is-the-resolution-of-the-process-inputs
  (is (= (apply path/resolve-config-path (process-inputs)) path/config-path)))

(deftest a-test-jvm-never-points-at-the-operator-config
  (when (path/test-jvm?)
    (is (not= path/operator-config-path path/config-path))))

(deftest operator-config-path-is-the-home-default
  ;; Checked by shape, not by a spelled-out home path: this test reads no
  ;; file, and the default-tier isolation gate flags the literal.
  (let [home (System/getProperty "user.home")]
    (is (str/starts-with? path/operator-config-path (str home "/")))
    (is (str/ends-with? path/operator-config-path "/hive-mcp/config.edn"))
    (is (= path/operator-config-path (path/resolve-config-path "" home))
        "a blank override resolves to the same home default")))

(defspec a-non-blank-override-always-wins 200
  (prop/for-all [env (gen/such-that #(not (str/blank? %)) gen/string-alphanumeric)
                 home gen/string-alphanumeric]
    (= env (path/resolve-config-path env home))))

(defspec a-missing-or-blank-override-falls-to-the-home-default 200
  (prop/for-all [env (gen/one-of [(gen/return nil) (gen/return "")
                                  (gen/fmap #(apply str (repeat % \space)) (gen/choose 1 5))])
                 home gen/string-alphanumeric]
    (= (str home "/.config/hive-mcp/config.edn") (path/resolve-config-path env home))))

(hmut/deftest-mutations config-path-resolution-mutations
  hive-mcp.config.path/resolve-config-path
  [["ignores the override"   (fn [_ home] (str home "/.config/hive-mcp/config.edn"))]
   ["honours a blank"        (fn [env home] (or env (str home "/.config/hive-mcp/config.edn")))]
   ["drops the home"         (fn [env _] (or env "/.config/hive-mcp/config.edn"))]
   ["wrong file name"        (fn [env home] (if (and (string? env) (not (str/blank? env)))
                                              env
                                              (str home "/.config/hive-mcp/config.json")))]]
  (fn []
    (is (= "alt.edn" (path/resolve-config-path "alt.edn" "/h")))
    (is (= "/h/.config/hive-mcp/config.edn" (path/resolve-config-path nil "/h")))
    (is (= "/h/.config/hive-mcp/config.edn" (path/resolve-config-path "  " "/h")))))
