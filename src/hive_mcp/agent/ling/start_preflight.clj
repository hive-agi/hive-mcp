(ns hive-mcp.agent.ling.start-preflight
  "Spawn-time preflight for lings that run Claude Code interactively.

   Refuses a spawn whose cwd Claude Code has not trusted, instead of letting
   the ling start on the folder-trust dialog and exit when its task arrives."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Port

(defprotocol IFolderTrust
  (trusted-roots [this]
    "Set of absolute directory paths Claude Code has trusted.")
  (checkout-root [this dir]
    "Absolute path of the main checkout DIR belongs to, or nil outside git."))

;;; Promote (pure)

(def interactive-modes
  "Spawn modes that launch the Claude Code TUI in a terminal."
  #{:claude :vterm})

(defn- strip-trailing-slash [path]
  (if (and (> (count path) 1) (str/ends-with? path "/"))
    (subs path 0 (dec (count path)))
    path))

(defn- parent-dir [path]
  (some-> (io/file path) .getParent))

(defn trust-candidates
  "Directories whose trust covers DIR: DIR and its ancestors, stopping at
   ROOT (inclusive) when ROOT is given, else at the filesystem root."
  [dir root]
  (let [dir  (strip-trailing-slash dir)
        root (some-> root strip-trailing-slash)]
    (loop [path dir acc []]
      (let [acc (conj acc path)]
        (if (or (= path root) (nil? (parent-dir path)))
          acc
          (recur (parent-dir path) acc))))))

(defn trusted?
  "True when any of CANDIDATES is in TRUSTED."
  [candidates trusted]
  (boolean (some (set trusted) candidates)))

(defn refusal-message
  "Error text for a spawn refused because ROOT is not trusted."
  [cwd root]
  (str "Claude Code has not trusted " root " (spawn cwd " cwd "). "
       "A claude ling started there opens on the folder-trust dialog and exits "
       "when its task is typed. Fix: run `claude` once in " root
       " and choose \"Yes, I trust this folder\", or set projects[\"" root
       "\"].hasTrustDialogAccepted to true in ~/.claude.json. "
       "Or spawn with a trusted cwd."))

(defn untrusted-refusal
  "Nil when MODE does not run Claude Code interactively or CWD is trusted
   by SOURCE; otherwise {:cwd :root :message}."
  [source mode cwd]
  (when (and cwd (contains? interactive-modes mode))
    (let [root (checkout-root source cwd)]
      (when-not (trusted? (trust-candidates cwd root) (trusted-roots source))
        (let [shown (or root (strip-trailing-slash cwd))]
          {:cwd cwd :root shown :message (refusal-message cwd shown)})))))

;;; Boundary adapter: ~/.claude.json + .git on disk

(defn- read-claude-config [path]
  (let [f (io/file path)]
    (if (.exists f)
      (json/read-str (slurp f))
      {})))

(defn- trusted-project-paths [config]
  (->> (get config "projects")
       (keep (fn [[path entry]]
               (when (true? (get entry "hasTrustDialogAccepted"))
                 (strip-trailing-slash path))))
       set))

(defn- worktree-main-root
  "Main checkout root named by a worktree .git FILE (gitdir: <root>/.git/worktrees/<n>)."
  [git-file]
  (when-let [gitdir (some->> (slurp git-file)
                             (re-find #"(?m)^gitdir:\s*(.+)$")
                             second
                             str/trim)]
    (let [f (io/file gitdir)
          f (if (.isAbsolute f) f (io/file (.getParentFile git-file) gitdir))]
      (some-> f .getCanonicalFile .getParentFile .getParentFile .getParent))))

(defn- main-checkout-root [dir]
  (loop [d (some-> dir io/file .getCanonicalFile)]
    (when d
      (let [git (io/file d ".git")]
        (cond
          (.isDirectory git) (.getPath d)
          (.isFile git)      (or (worktree-main-root git) (.getPath d))
          :else              (recur (.getParentFile d)))))))

(defrecord ClaudeCodeTrust [config-path]
  IFolderTrust
  (trusted-roots [_] (trusted-project-paths (read-claude-config config-path)))
  (checkout-root [_ dir] (main-checkout-root dir)))

(defn claude-code-trust
  "Trust source backed by Claude Code's ~/.claude.json."
  []
  (->ClaudeCodeTrust (str (System/getProperty "user.home") "/.claude.json")))

(def ^:dynamic *folder-trust*
  "IFolderTrust consulted by `ensure-startable!`; nil means the default adapter."
  nil)

;;; Boundary: the spawn-path entry point

(defn ensure-startable!
  "Throw ex-info when a MODE ling cannot start in CWD. A trust source that
   cannot be read admits the spawn and logs a warning."
  [mode cwd]
  (let [source (or *folder-trust* (claude-code-trust))
        refusal (try
                  (untrusted-refusal source mode cwd)
                  (catch Exception e
                    (log/warn "Spawn preflight could not read folder trust; admitting"
                              {:mode mode :cwd cwd :error (ex-message e)})
                    nil))]
    (when refusal
      (throw (ex-info (:message refusal)
                      (assoc refusal :reason :spawn/untrusted-folder :spawn-mode mode))))))
