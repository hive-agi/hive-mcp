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

(defn- result-arms
  "Every arm of read-config-file and write-config!, against a temp dir:
   a map, a missing file and a non-map read as (ok nil), a parse failure
   and an unwritable path read as errors. Error messages are host text,
   so the projection keeps the error keyword and class."
  []
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "hive-config-io-arms" (make-array java.nio.file.attribute.FileAttribute 0)))
        at  #(str (.getPath dir) "/" %)
        project (fn [res] (if (r/ok? res) {:ok (:ok res)} (select-keys res [:error :class])))]
    (try
      (spit (at "map.edn") "{:a 1 :b {:c \"x\"}}")
      (spit (at "vec.edn") "[1 2]")
      (spit (at "bad.edn") "{:a")
      {:map        (project (config-io/read-config-file (at "map.edn")))
       :missing    (project (config-io/read-config-file (at "missing.edn")))
       :non-map    (project (config-io/read-config-file (at "vec.edn")))
       :parse-fail (project (config-io/read-config-file (at "bad.edn")))
       :write-ok   (project (config-io/write-config! {:k [1 2]} (at "nested/dir/out.edn")))
       :written    (project (config-io/read-config-file (at "nested/dir/out.edn")))
       :write-fail (project (config-io/write-config! {:k 1} (str (at "vec.edn") "/under-a-file.edn")))}
      (finally
        (doseq [f (reverse (file-seq dir))] (.delete ^java.io.File f))))))

(golden/deftest-golden-fn read-config-file-arms-golden
  "test/golden/config/io-result-arms.edn"
  result-arms)
