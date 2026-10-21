(ns hive-mcp.engine.hprof.fs-test
  "Golden over the hprof filesystem boundary, driven against a fresh temp
   directory. The snapshot projects away host-specific values (absolute
   paths, mtimes) and keeps names, sizes and success flags."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [hive-mcp.engine.hprof.fs :as fs]
            [hive-test.golden :refer [deftest-golden-fn]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "hprof-fs-test" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- rm-rf [^java.io.File f]
  (when (.isDirectory f) (run! rm-rf (.listFiles f)))
  (.delete f))

(defn- names [hs] (vec (sort (map #(.getName (io/file (:path %))) hs))))

(defn- project [h] (when h {:name (.getName (io/file (:path h))) :bytes (:bytes h)}))

(defn- scenario []
  (let [root (temp-dir)
        src  (io/file root "src")
        dst  (io/file root "dst")]
    (try
      (.mkdirs src)
      (spit (io/file src "java_pid1.hprof") "aaaaaaaaaa")
      (spit (io/file src "java_pid2.hprof") "bbbbb")
      (spit (io/file src "old.hprof.gz") "zz")
      (spit (io/file src "notes.txt") "not a dump")
      (.mkdirs (io/file src "sub.hprof"))
      (let [listed   (fs/list-hprofs src)
            ensured  (fs/ensure-dir! dst)
            p1       (first (filter #(.endsWith ^String (:path %) "java_pid1.hprof") listed))
            moved    (fs/move-to-dir! p1 dst)
            gz       (fs/gzip! moved)
            p2       (first (filter #(.endsWith ^String (:path %) "java_pid2.hprof") (fs/list-hprofs src)))
            deleted  (fs/delete! p2)]
        {:listed          (names listed)
         :listed-bytes    (into (sorted-map) (map (juxt #(.getName (io/file (:path %))) :bytes) listed))
         :missing-dir     (fs/list-hprofs (io/file root "absent"))
         :ensured-is-dir? (.isDirectory ^java.io.File ensured)
         :moved           (project moved)
         :move-missing    (fs/move-to-dir! {:path (str root "/ghost.hprof")} dst)
         :gzip-suffix     (when gz (.getName (io/file gz)))
         :gzip-missing    (fs/gzip! {:path (str root "/ghost.hprof")})
         :dst-after       (names (fs/list-hprofs dst))
         :deleted?        deleted
         :delete-again?   (fs/delete! p2)
         :src-after       (names (fs/list-hprofs src))})
      (finally (rm-rf root)))))

(deftest-golden-fn fs-boundary-golden
  "test/golden/engine/hprof-fs-boundary.edn"
  scenario)

(deftest gzip-is-idempotent-over-an-existing-gz
  (let [root (temp-dir)]
    (try
      (let [raw (io/file root "x.hprof")]
        (spit raw "payload")
        (spit (io/file root "x.hprof.gz") "prior")
        (let [out (fs/gzip! {:path (.getPath raw)})]
          (is (.endsWith ^String out "x.hprof.gz"))
          (is (not (.exists raw)) "the raw is dropped")
          (is (= "prior" (slurp (io/file root "x.hprof.gz"))) "the prior .gz is kept")))
      (finally (rm-rf root)))))
