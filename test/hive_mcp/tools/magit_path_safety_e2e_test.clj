(ns hive-mcp.tools.magit-path-safety-e2e-test
  "E2E twin of magit-path-safety-test: the same handler chain against a REAL
   git binary in a temp repository this test creates and deletes. It checks
   what the in-memory world cannot: that real git with --literal-pathspecs,
   real symlinks and the real index file behave as the fake claims.

   Skipped by default, twice over: ^:integration (excluded by :test-unit and
   :test-focus) and the HIVE_GIT_E2E=1 gate, so a plain run-tests in a REPL
   skips it too. Sandbox: a temp dir under java.io.tmpdir, git config local to
   it, the editor vessel stubbed (never the live Emacs), deleted afterwards.
     HIVE_GIT_E2E=1 clojure -M:test-integration   (or set it in the REPL JVM)"
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.consolidated.git :as git-tool]
            [hive-mcp.tools.magit :as magit]
            [hive-mcp.test.stub.swarm-host :as swarm]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- enabled? []
  (or (= "1" (System/getenv "HIVE_GIT_E2E")) (= "1" (System/getProperty "hive.git.e2e"))))

(defn- git [dir & args]
  (let [r (apply sh/sh "git" (concat args [:dir dir]))]
    (when-not (zero? (:exit r)) (throw (ex-info (str "git " (str/join " " args) " failed") r)))
    (:out r)))

(defn- delete-tree!
  "Delete F and everything under it. A symlink is deleted as a link, never
   followed, even when its target is missing."
  [^java.io.File f]
  (let [p (.toPath f)
        link? (java.nio.file.Files/isSymbolicLink p)]
    (when (or link? (.exists f))
      (when (and (not link?) (.isDirectory f))
        (doseq [c (.listFiles f)] (delete-tree! c)))
      (java.nio.file.Files/deleteIfExists p))))

(defn- with-temp-repo
  "Run F with a fresh repo holding pages/[id].tsx, pages/i.tsx, pages/d.tsx,
   a.txt, b.txt and a symlink `link` -> a.txt; delete it afterwards."
  [f]
  (let [dir (str (java.nio.file.Files/createTempDirectory
                  "hive-git-e2e" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (git dir "init" "-q")
      (doseq [[k v] [["user.name" "e2e"] ["user.email" "e2e@example.invalid"]
                     ["commit.gpgsign" "false"] ["core.hooksPath" "/dev/null"]]]
        (git dir "config" k v))
      (.mkdirs (java.io.File. dir "pages"))
      (doseq [p ["pages/[id].tsx" "pages/i.tsx" "pages/d.tsx" "a.txt" "b.txt"]]
        (spit (str dir "/" p) (str p "\n")))
      (java.nio.file.Files/createSymbolicLink
       (.toPath (java.io.File. dir "link")) (.toPath (java.io.File. "a.txt"))
       (make-array java.nio.file.attribute.FileAttribute 0))
      (git dir "add" "-A")
      (git dir "commit" "-q" "-m" "base")
      (f dir)
      (finally (delete-tree! (java.io.File. dir))))))

(def ^:private bridge-down
  (constantly {:success false :timed-out false :error "Emacs bridge entrypoints failed to load."}))

(defn- handle [dir params]
  (binding [magit/*bridge-down-ms* 0 magit/*handshake-retry-delay-ms* 0]
    (swarm/with-swarm-host [_ bridge-down]
      (git-tool/handle-git (merge {:directory dir} params)))))

(defn- last-commit [dir]
  (set (remove str/blank? (str/split-lines (git dir "show" "--name-only" "--format=" "HEAD")))))

(deftest ^:integration e2e-bracket-path-commits-only-itself
  (if-not (enabled?)
    (is true "skipped: set HIVE_GIT_E2E=1 to run against a temp git repo")
    (with-temp-repo
      (fn [dir]
        (.delete (java.io.File. dir "pages/[id].tsx"))
        (let [r (handle dir {:command "commit" :message "drop id" :files ["pages/[id].tsx"]
                             :include_staged true})]
          (is (not (:isError r)) (:text r)))
        (is (= #{"pages/[id].tsx"} (last-commit dir)))
        (is (str/blank? (git dir "status" "--porcelain")))))))

(deftest ^:integration e2e-absolute-path-commits-the-edit
  (if-not (enabled?)
    (is true "skipped: set HIVE_GIT_E2E=1")
    (with-temp-repo
      (fn [dir]
        (spit (str dir "/a.txt") "edited\n")
        (let [r (handle dir {:command "commit" :message "edit a" :files [(str dir "/a.txt")]
                             :include_staged true})]
          (is (not (:isError r)) (:text r)))
        (is (= "M\ta.txt" (str/trim (git dir "show" "--name-status" "--format=" "HEAD"))))))))

(deftest ^:integration e2e-dangling-symlink-commits-the-retarget
  (if-not (enabled?)
    (is true "skipped: set HIVE_GIT_E2E=1")
    (with-temp-repo
      (fn [dir]
        (let [link (.toPath (java.io.File. dir "link"))]
          (java.nio.file.Files/delete link)
          (java.nio.file.Files/createSymbolicLink link (.toPath (java.io.File. "missing"))
                                                  (make-array java.nio.file.attribute.FileAttribute 0)))
        (let [r (handle dir {:command "commit" :message "retarget" :files ["link"]})]
          (is (not (:isError r)) (:text r)))
        (is (= "M\tlink" (str/trim (git dir "show" "--name-status" "--format=" "HEAD"))))
        (is (str/blank? (git dir "status" "--porcelain")))))))

(deftest ^:integration e2e-a-refusal-leaves-the-index-as-found
  (if-not (enabled?)
    (is true "skipped: set HIVE_GIT_E2E=1")
    (with-temp-repo
      (fn [dir]
        (spit (str dir "/b.txt") "b2\n")
        (git dir "add" "b.txt")
        (.delete (java.io.File. dir "a.txt"))
        (let [before (git dir "status" "--porcelain")
              r (handle dir {:command "commit" :message "x" :files ["a.txt"]})]
          (is (:isError r))
          (is (str/includes? (:text r) "unrelated-staged") (:text r))
          (is (= before (git dir "status" "--porcelain"))))))))

(deftest ^:integration e2e-deleted-directory-and-glob
  (if-not (enabled?)
    (is true "skipped: set HIVE_GIT_E2E=1")
    (with-temp-repo
      (fn [dir]
        (let [before (git dir "status" "--porcelain")
              r (handle dir {:command "stage" :files ["*.txt"]})]
          (is (:isError r) "a glob names no file")
          (is (= before (git dir "status" "--porcelain"))))
        (delete-tree! (java.io.File. dir "pages"))
        (let [r (handle dir {:command "commit" :message "drop pages" :files ["pages"]})]
          (is (not (:isError r)) (:text r)))
        (is (= #{"pages/[id].tsx" "pages/i.tsx" "pages/d.tsx"} (last-commit dir)))))))
