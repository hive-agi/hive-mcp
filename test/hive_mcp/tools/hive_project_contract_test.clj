(ns hive-mcp.tools.hive-project-contract-test
  "Contract facets for native project-type detection and headless
   .hive-project.edn generation. Every case builds its own temporary project
   directory and deletes it afterwards; generation is driven only on the
   skip arms (existing file, no .git), so config parent rules are never read."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.hive-project :as hp]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- with-project
  "Call f with the path of a fresh temp dir holding `files` (a .git entry
   counts as a dir); delete the tree afterwards."
  [files f]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "m24-hp" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (doseq [n files]
        (if (= ".git" n) (.mkdirs (io/file dir n)) (spit (io/file dir n) "")))
      (f (.getAbsolutePath dir))
      (finally
        (doseq [x (reverse (file-seq dir))] (.delete ^java.io.File x))))))

(defn- detect [files] (with-project files hp/detect-project-type-native))

(golden/deftest-golden detect-project-type-golden
  "test/golden/hive-mcp/tools/hive-project-detect.edn"
  {:no-git        (detect ["deps.edn"])
   :generic       (detect [".git"])
   :clojure-cli   (detect [".git" "deps.edn" "package.json"])
   :npm           (detect [".git" "package.json" "Makefile"])
   :make          (detect [".git" "Makefile"])
   :gradle-kts    (detect [".git" "build.gradle.kts"])
   :nil-dir       (hp/detect-project-type-native nil)
   :missing-dir   (hp/detect-project-type-native "/m24/no/such/dir")})

(deftest headless-generation-skips-without-writing
  (with-project [".git" ".hive-project.edn"]
    (fn [d]
      (let [r (hp/generate-hive-project-headless! d)]
        (is (m/validate hp/HeadlessGeneration r))
        (is (= "existing" (:reason r)))
        (is (= "" (slurp (io/file d ".hive-project.edn")))))))
  (with-project ["deps.edn"]
    (fn [d]
      (let [r (hp/generate-hive-project-headless! d)]
        (is (= "no-git" (:reason r)))
        (is (not (.exists (io/file d ".hive-project.edn"))))))))

(deftest detected-types-stay-inside-the-enum
  (doseq [fs [[".git"] [".git" "go.mod"] [".git" "mix.exs" "Makefile"]]]
    (is (m/validate hp/ProjectType (detect fs)))))
