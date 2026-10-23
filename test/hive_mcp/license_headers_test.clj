(ns hive-mcp.license-headers-test
  "Every SPDX header in the repository names the licence version.edn declares.

   hive-build's `verify-license` only reads src/ and only fails under
   :strict, which no workflow ran, so MIT headers sat under an AGPL
   version.edn for months. This test is the unit-suite gate: it reads
   version.edn :license :name and fails on any header, in any tree that ships
   or is shipped alongside the jar, that names a different licence.

   `drift` is pure (declared licence + [path text] pairs -> offenders); the
   file walk is the only IO and lives in `repo-sources`."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private header-re
  "A comment line carrying an SPDX id. Anchored on the comment prefix so a
   string literal that merely mentions the tag is not read as a header."
  #"(?m)^\s*(?:;+|#+|//)\s*SPDX-License-Identifier:\s*(\S+)")

(def ^:private roots
  ["src" "resources" "test" "test-swarm" "dev" "bin" "Dockerfile"])

(def ^:private source-exts
  #{"clj" "cljc" "cljs" "edn" "bb" "sh" "el"})

(def ^:private known-exceptions
  "Repo-relative paths whose header deliberately differs from version.edn,
   keyed to the licence they carry. Adding an entry here is a licensing
   decision by the copyright holder, never a way to quiet the test."
  {"test/hive_mcp/server/routes/guard_middleware_test.clj" "LicenseRef-Proprietary"})

(defn spdx-ids
  "Every SPDX id declared in a comment line of `text`."
  [text]
  (mapv second (re-seq header-re text)))

(defn drift
  "Offending headers among `sources` ([path text] pairs): every SPDX id that
   is neither `declared` nor the licence `exceptions` records for that path.
   Returns [{:path :spdx}], sorted by path."
  [declared exceptions sources]
  (->> sources
       (mapcat (fn [[path text]]
                 (for [id (spdx-ids text)
                       :when (not= id declared)
                       :when (not= id (get exceptions path))]
                   {:path path :spdx id})))
       (sort-by :path)
       vec))

(defn- source-file? [^java.io.File f]
  (and (.isFile f)
       (or (= "Dockerfile" (.getName f))
           (contains? source-exts (last (str/split (.getName f) #"\."))))))

(defn- repo-sources
  "[repo-relative-path text] for every source file under `roots`."
  []
  (for [root roots
        :let [r (io/file root)]
        :when (.exists r)
        ^java.io.File f (file-seq r)
        :when (source-file? f)]
    [(str/replace (.getPath f) java.io.File/separatorChar \/) (slurp f)]))

(defn- declared-licence []
  (get-in (edn/read-string (slurp "version.edn")) [:license :name]))

(deftest spdx-ids-reads-comment-headers-only
  (is (= ["MIT"] (spdx-ids (str ";; Copyright\n;;\n;; SPDX-License-Identifier: MIT\n"))))
  (is (= ["AGPL-3.0-or-later"] (spdx-ids "# SPDX-License-Identifier: AGPL-3.0-or-later\n")))
  (is (= [] (spdx-ids "(def s \"SPDX-License-Identifier: MIT\")\n"))))

(deftest drift-flags-a-foreign-licence
  (let [agpl ";; SPDX-License-Identifier: AGPL-3.0-or-later\n"
        mit  ";; SPDX-License-Identifier: MIT\n"]
    (testing "an MIT header under an AGPL declaration is drift"
      (is (= [{:path "src/a.clj" :spdx "MIT"}]
             (drift "AGPL-3.0-or-later" {} [["src/a.clj" mit] ["src/b.clj" agpl]]))))
    (testing "an exception covers only its own path and its own licence"
      (is (= [] (drift "AGPL-3.0-or-later" {"src/a.clj" "MIT"} [["src/a.clj" mit]])))
      (is (= [{:path "src/c.clj" :spdx "MIT"}]
             (drift "AGPL-3.0-or-later" {"src/a.clj" "MIT"} [["src/c.clj" mit]]))))))

(deftest repository-headers-match-version-edn
  (let [declared (declared-licence)
        sources  (repo-sources)]
    (is (= "AGPL-3.0-or-later" declared))
    (is (< 1000 (count sources)) "the walk must actually see the repository")
    (is (= [] (drift declared known-exceptions sources))
        "SPDX headers that disagree with version.edn :license")))
