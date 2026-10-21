(ns hive-mcp.crystal.harvest.sources-test
  "Golden over the default IHarvestSource set. Every source is total: with
   its backend absent (no chroma, no swarm addon on the unit classpath) the
   harvest answers a :harvest/error outcome instead of throwing. git-source
   is driven against temp directories: a non-repository and a repository
   with no commits (git log fails in both), and a repository with one fresh
   commit (harvest-ok). Error text from git is host-dependent, so the golden
   keeps the variant, source, error type and commit count."
  (:require [clojure.java.shell :refer [sh]]
            [clojure.test :refer [deftest is]]
            [hive-mcp.crystal.harvest.protocol :as hp]
            [hive-mcp.crystal.harvest.sources :as sources]
            [hive-test.golden :refer [deftest-golden-fn]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "harvest-sources" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- rm-rf [^java.io.File f]
  (when (.isDirectory f) (run! rm-rf (.listFiles f)))
  (.delete f))

(defn- git! [dir & args]
  (apply sh "git" "-c" "user.name=f08" "-c" "user.email=f08@example.invalid"
         "-c" "commit.gpgsign=false" (concat args [:dir (.getPath ^java.io.File dir)])))

(defn- outcome [o]
  (cond-> (select-keys o [:adt/variant :source])
    (:error o) (assoc :error-type (get-in o [:error :type]))
    (:data o)  (assoc :count (get-in o [:data :count]))))

(defn- scenario []
  (let [non-repo  (temp-dir)
        empty-rep (temp-dir)
        repo      (temp-dir)]
    (try
      (git! empty-rep "init" "-q")
      (git! repo "init" "-q")
      (git! repo "commit" "-q" "--allow-empty" "-m" "one")
      (let [srcs (sources/default-sources)
            git  (sources/git-source)]
        {:ids            (mapv hp/source-id srcs)
         :git-available? (hp/available? git)
         :git-non-repo   (outcome (hp/harvest git {:directory (.getPath non-repo)}))
         :git-no-commits (outcome (hp/harvest git {:directory (.getPath empty-rep)}))
         :git-one-commit (outcome (hp/harvest git {:directory (.getPath repo)}))
         :kanban-absent  (outcome (hp/harvest (sources/kanban-source) {:project-id "f08-none"}))
         :memory-absent  (outcome (hp/harvest (sources/memory-source) {:project-id "f08-none"}))})
      (finally (run! rm-rf [non-repo empty-rep repo])))))

(deftest-golden-fn default-sources-golden
  "test/golden/crystal/harvest-sources.edn"
  scenario)

(deftest constructors-are-fresh-instances
  (is (not (identical? (sources/git-source) (sources/git-source))))
  (is (= [:memory :hivemind :kanban :git]
         (mapv hp/source-id (sources/default-sources)))))
