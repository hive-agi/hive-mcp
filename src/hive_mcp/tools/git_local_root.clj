(ns hive-mcp.tools.git-local-root
  "The `git commit` / `batch-commit` choke point for a committed deps.edn or
   bb.edn whose `:local/root` leaves the repository [NO-LOCAL-ROOT-LAND].

   The check itself lives in hive-git (`hive-git.local-root`, hive-git #48):
   `refuse-commit` reads the INDEX through hive-git's git port, and
   `violations` is the pure EDN check over one file's content. hive-mcp does
   not depend on hive-git, so both are resolved softly: with the hive-git
   addon absent the commit is not checked (nil checker), never refused.

   A plain commit records the index, so the index is checked. A commit with
   `all` also records every modified tracked file, which is not staged yet,
   so the working-tree content of the changed deps files is checked too.

   Every IO edge is a port map, so tests drive it with stubs:
     checker {:refuse-commit (fn [ctx staged-paths] result)
              :violations    (fn [file content] [violation ...])
              :git           <hive-git port impl, passed through in ctx>}
     repo    {:staged-paths   (fn [dir] [path ...])
              :modified-paths (fn [dir] [path ...])
              :read-file      (fn [dir path] content-or-nil)}"
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [hive-mcp.tools.core :refer [mcp-error]]
            [malli.core :as m]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- soft [sym]
  (try (requiring-resolve sym) (catch Throwable _absent nil)))

(defn hive-git-checker
  "The checker port backed by the hive-git addon, or nil when hive-git is not
   on the classpath."
  []
  (let [refuse (soft 'hive-git.local-root/refuse-commit)
        viols  (soft 'hive-git.local-root/violations)
        ->cli  (soft 'hive-git.adapter.cli/->cli)]
    (when (and refuse viols ->cli)
      {:refuse-commit refuse :violations viols :git (->cli)})))

(defn- git-lines [dir & args]
  (let [{:keys [exit out]} (apply sh/sh "git" "-C" (str dir) args)]
    (if (zero? exit) (vec (remove str/blank? (str/split-lines out))) [])))

(def cli-repo
  "The repo port over the git CLI and the filesystem."
  {:staged-paths   (fn [dir] (git-lines dir "diff" "--cached" "--name-only"))
   :modified-paths (fn [dir] (git-lines dir "diff" "--name-only"))
   :read-file      (fn [dir path]
                     (let [f (io/file (str dir) (str path))]
                       (when (.isFile f) (slurp f))))})

(defn- deps-file? [path]
  (contains? #{"deps.edn" "bb.edn"} (.getName (io/file (str path)))))

(defn- describe [vs]
  (str/join "; " (for [{:keys [file line coord path reason]} vs]
                   (if (= :unparseable reason)
                     (str file " does not parse as EDN")
                     (str file ":" line " " coord " {:local/root " (pr-str path) "}")))))

(defn- worktree-violations
  "Violations in the working-tree content of the root deps files and of every
   modified deps file: what `commit --all` would record."
  [{:keys [violations]} {:keys [modified-paths read-file]} dir]
  (let [paths (distinct (concat ["deps.edn" "bb.edn"] (filter deps-file? (modified-paths dir))))]
    (into [] (mapcat #(violations % (read-file dir %))) paths)))

(defn refusal
  "nil when the commit in `dir` may proceed, else an MCP error naming each
   file and coordinate whose :local/root leaves the repository. `all?` is the
   commit's stage-all flag. A nil checker (no hive-git) checks nothing."
  ([dir all?] (refusal (hive-git-checker) cli-repo dir all?))
  ([checker repo dir all?]
   (when checker
     (let [{:keys [refuse-commit git]} checker
           staged (refuse-commit {:git git :repo (str dir)} ((:staged-paths repo) dir))
           staged-vs (when-not (contains? staged :ok) (:violations staged))
           tree-vs (when all? (worktree-violations checker repo dir))
           vs (vec (distinct (concat staged-vs tree-vs)))]
       (cond
         (seq vs)
         (do (log/warn "commit refused: escaping :local/root" {:directory dir :violations vs})
             (mcp-error (str "git/local-root: commit refused: a committed deps file points "
                             ":local/root outside the repository (" (describe vs) "). "
                             "Pin a released version; keep checkouts in local.deps.edn.")))

         (not (contains? staged :ok))
         (mcp-error (str "git/local-root: commit refused: "
                         (or (:message staged) (pr-str (:error staged))))))))))

(def ^:private Port [:map-of :keyword :any])

(m/=> refusal [:function
               [:=> [:cat :any :any] [:maybe map?]]
               [:=> [:cat [:maybe Port] Port :any :any] [:maybe map?]]])
(m/=> hive-git-checker [:=> [:cat] [:maybe Port]])
