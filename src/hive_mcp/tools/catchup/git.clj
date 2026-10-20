(ns hive-mcp.tools.catchup.git
  "Git context gathering for catchup workflow.
   Uses direct shell calls instead of Emacs to avoid blocking."
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:dynamic *sh*
  "The shell port git commands run through, or nil for
   `clojure.java.shell/sh`. A bound fn takes sh's arguments and answers
   {:exit :out :err}; rebind it (with `binding`) to answer git without a
   repository or a process."
  nil)

(defn- git-cmd
  "Run a git command in the given directory. Returns trimmed stdout or fallback."
  [directory fallback & args]
  (try
    (let [opts (if directory
                 (into (vec args) [:dir directory])
                 (vec args))
          {:keys [exit out]} (apply (or *sh* sh) "git" opts)]
      (if (zero? exit)
        (str/trim out)
        fallback))
    (catch Exception _
      fallback)))

(defn gather-git-info
  "Gather git information via direct shell calls (no Emacs dependency)."
  [directory]
  (try
    {:branch      (git-cmd directory "none"
                           "rev-parse" "--abbrev-ref" "HEAD")
     :uncommitted (not (str/blank?
                        (git-cmd directory ""
                                 "status" "--porcelain")))
     :last-commit (git-cmd directory "none"
                           "log" "-1" "--format=%h - %s")}
    (catch Exception _
      {:branch "unknown" :uncommitted false :last-commit "unknown"})))

;; =============================================================================
;; Contracts
;; =============================================================================

(def GitInfo
  "What `gather-git-info` answers. Failure values are the strings \"none\" or
   \"unknown\", never nil."
  [:map
   [:branch :string]
   [:uncommitted :boolean]
   [:last-commit :string]])

(m/=> gather-git-info
      [:=> [:cat [:maybe [:string {:gen/schema [:enum "/nonexistent-m26/repo"]}]]] GitInfo])
