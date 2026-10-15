(ns hive-mcp.config.io-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.config.io :as config-io]
            [hive-dsl.result :as r]
            [hive-test.golden :as golden]))

(deftest read-write-roundtrip-facet
  (let [file (doto (java.io.File/createTempFile "hive-config-io" ".edn") .deleteOnExit)
        config {:memory {:default-store :chroma} :project-roots ["/tmp"]}
        path (.getPath file)]
    (is (r/ok? (config-io/write-config! config path)))
    (let [read-back (config-io/read-config-file path)]
      (is (r/ok? read-back))
      (is (= config (:ok read-back)))
      (golden/assert-golden
       (golden/anchor 'hive-mcp.config.io-test "test/golden/config/io-roundtrip.edn")
       (:ok read-back)))))
