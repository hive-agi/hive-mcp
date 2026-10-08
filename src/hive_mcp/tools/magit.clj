(ns hive-mcp.tools.magit
  "Magit integration handlers for MCP.

   Provides comprehensive git operations via magit addon:
   - Status, branches, log, diff
   - Stage, commit, push, pull, fetch
   - Feature branch listing for /ship and /ship-pr skills

   Result DSL: Internal logic returns Result maps ({:ok val} or {:error category}).
   Single try-result boundary at each handler level. Zero nested try-catch."
  (:require [hive-mcp.dns.result :as result]
            [hive-mcp.tools.core :refer [mcp-success mcp-error emacs-timeout-ms]]
            [hive-mcp.context.request :as ctx]
            [taoensso.timbre :as log]
            [clojure.string :as str]
            [hive-spi.editor.services :as svc]
            [clojure.data.json :as json]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Working Directory Resolution
;; ============================================================
;;
;; When Claude CLI spawns in a project directory, magit tools should
;; operate on that project by default - not on whatever buffer is
;; active in Emacs.
;;
;; Fallback chain:
;;   1. Explicit directory parameter from caller
;;   2. ctx/current-directory - from request context (CTX migration)
;;   3. System/getProperty "user.dir" - MCP server's working directory

(defn resolve-directory
  "Resolve the directory to use for git operations.
   Uses provided directory, request context directory, or falls back to
   MCP server's working directory. A blank candidate counts as absent: the
   closed :magit/* ops refuse a blank :directory, so a blank one must fall
   through the chain instead of failing the call.

   CTX Migration: Now uses hive-mcp.context.request for directory resolution."
  [directory]
  (some #(when-not (str/blank? %) %)
        [(some-> directory str)
         (some-> (ctx/current-directory) str)
         (System/getProperty "user.dir")]))

;;; =============================================================================
;;; Result DSL Helpers (boundary pattern — same as tools/cider.clj)
;;; =============================================================================

(defn- dispatch->result
  "Dispatch a closed magit operation and convert the eval-shaped envelope to Result."
  [op timeout-ms]
  (let [{:keys [success result error]} (svc/invoke :vessel :dispatch op timeout-ms)]
    (if success
      (result/ok result)
      (result/err :magit/dispatch-failed {:message (str error)}))))

(defn- try-result
  "Execute thunk f returning Result; catch unexpected exceptions as error Result.
   Unlike try-effect, expects f to return a Result map directly."
  [category f]
  (try
    (f)
    (catch Exception e
      (log/error e (str (name category) " failed"))
      (result/err category {:message (.getMessage e)}))))

(defn- result->mcp
  "Convert Result to MCP response.
   {:ok data} -> (mcp-success data), {:error ...} -> (mcp-error message).
   Uses if-let+find for CC-free unwrapping (scc does not count if-let)."
  [r]
  (if-let [entry (find r :ok)]
    (mcp-success (val entry))
    (mcp-error (str "Error: " (get r :message (get r :error "unknown"))))))

(defn- handle-op
  "Run a closed magit op through the Result boundary."
  [category op timeout-ms]
  (result->mcp (try-result category #(dispatch->result op timeout-ms))))

(defn- with-default
  "Provide default value when Result ok value is nil.
   Uses if-let (CC-free) for both Result detection and nil check."
  [default-val r]
  (if-let [entry (find r :ok)]
    (if-let [_v (val entry)] r (result/ok default-val))
    r))

;; ============================================================
;; Magit Integration Tools (requires hive-mcp-magit addon)
;; ============================================================

;;; =============================================================================
;;; Magit Handlers (thin wrappers over handle-op: one closed op per command)
;;; =============================================================================


(defn normalize-files
  "Normalize the `files` tool parameter to :all or a vector of path strings.

   Accepts a collection of paths, a single path, several paths in one
   whitespace-separated string, or \"all\". Returns :all, a NON-EMPTY vector of
   paths, or nil when nothing usable was given."
  [files]
  (cond
    (nil? files)     nil
    (keyword? files) (when (= :all files) :all)
    (symbol? files)  (when (= 'all files) :all)
    (string? files)  (let [ps (vec (remove str/blank? (str/split (str/trim files) #"\s+")))]
                       (cond
                         (empty? ps)    nil
                         (= ["all"] ps) :all
                         :else          ps))
    (coll? files)    (let [ps (->> files (map str) (map str/trim) (remove str/blank?) vec)]
                       (cond
                         (empty? ps)    nil
                         (= ["all"] ps) :all
                         :else          ps))
    :else            nil))

(defn- stage-verify
  "Stage PATHS in DIR and confirm the index carries at least one of them.

   The :magit/stage-verify op answers a JSON verdict: {status ok}, {status
   missing, path P} or {status empty}.

   Result: (ok PATHS), or (err :magit/path-not-found | :magit/nothing-staged |
   :magit/dispatch-failed | :magit/stage-failed). Any answer that is not the
   explicit ok verdict is an error: a commit must never proceed on an index this
   operation did not verify."
  [paths dir timeout-ms]
  (let [r (try-result :magit/stage-failed
                      #(dispatch->result {:op :magit/stage-verify :paths paths :directory dir}
                                         timeout-ms))
        listed (str/join " " paths)]
    (if-let [entry (find r :ok)]
      (let [raw (str (val entry))
            out (try (json/read-str raw :key-fn keyword)
                     ;; An unparseable verdict falls through to :magit/stage-failed below.
                     (catch Exception _unparseable-verdict nil))
            status (when (map? out) (:status out))]
        (case status
          "missing" (result/err :magit/path-not-found
                                {:message (str ":magit/path-not-found - no such path under "
                                               dir ": " (or (:path out) "(unnamed)"))})
          "empty"   (result/err :magit/nothing-staged
                                {:message (str ":magit/nothing-staged - staging left the index empty for "
                                               listed
                                               "; refusing to commit what was already staged")})
          "ok"      (result/ok paths)
          (result/err :magit/stage-failed
                      {:message (str ":magit/stage-failed - staging " listed
                                     " returned no verdict: " raw)})))
      r)))

(defn handle-magit-status
  "Get comprehensive git repository status via magit addon."
  [{:keys [directory] :as params}]
  (let [dir (resolve-directory directory)]
    (log/info "magit-status" {:directory dir})
    (handle-op :magit/status-failed {:op :magit/status :directory dir}
               (emacs-timeout-ms params))))

(defn handle-magit-branches
  "Get branch information including current, upstream, local and remote branches."
  [{:keys [directory] :as params}]
  (let [dir (resolve-directory directory)]
    (log/info "magit-branches" {:directory dir})
    (handle-op :magit/branches-failed {:op :magit/branches :directory dir}
               (emacs-timeout-ms params))))

(defn handle-magit-log
  "Get recent commit log (default 10 entries)."
  [{:keys [count directory] :as params}]
  (let [dir (resolve-directory directory)
        n (or count 10)]
    (log/info "magit-log" {:count n :directory dir})
    (handle-op :magit/log-failed {:op :magit/log :count n :directory dir}
               (emacs-timeout-ms params))))

(defn handle-magit-diff
  "Get diff for staged, unstaged, or all changes. Unknown targets mean staged."
  [{:keys [target directory] :as params}]
  (let [dir (resolve-directory directory)
        target (if (contains? #{"staged" "unstaged" "all"} target) target "staged")]
    (log/info "magit-diff" {:target target :directory dir})
    (handle-op :magit/diff-failed {:op :magit/diff :target target :directory dir}
               (emacs-timeout-ms params))))

(defn handle-magit-stage
  "Stage files for commit.

   `files` takes a list of paths, a single path, several paths in one
   whitespace-separated string, or 'all' for every modified file. An absent or
   blank `files` is an error, never a silent no-op."
  [{:keys [files directory] :as params}]
  (let [dir (resolve-directory directory)
        norm (normalize-files files)
        timeout-ms (emacs-timeout-ms params)]
    (log/info "magit-stage" {:files norm :directory dir})
    (if (nil? norm)
      (mcp-error (str ":magit/no-files - files is required: a path, a list of paths, "
                      "a whitespace-separated path string, or 'all'"))
      (result->mcp
       (try-result :magit/stage-failed
                   #(with-default "Staged files"
                      (dispatch->result {:op :magit/stage :files norm :directory dir}
                                        timeout-ms)))))))

(defn handle-magit-commit
  "Create a commit with the given message.

   `files` restricts the commit to explicit paths: a list, a single path,
   several paths in one whitespace-separated string, or 'all'. Named paths are
   staged and VERIFIED before the commit runs; a path that does not exist, or a
   stage that leaves the index empty for those paths, fails the operation
   instead of committing whatever the index already held."
  [{:keys [message all directory files] :as params}]
  (let [dir (resolve-directory directory)
        timeout-ms (emacs-timeout-ms params)
        norm (normalize-files files)
        stage-all (boolean (or all (= :all norm)))
        staged (when (vector? norm) (stage-verify norm dir timeout-ms))]
    (log/info "magit-commit" {:message-len (count message) :all stage-all
                              :files norm :directory dir})
    (if (and (some? staged) (nil? (find staged :ok)))
      (result->mcp staged)
      (handle-op :magit/commit-failed
                 {:op :magit/commit :message message :all stage-all :directory dir}
                 timeout-ms))))

(defn handle-magit-push
  "Push to remote. Optionally set upstream tracking.

   `remote` selects the remote explicitly; omitted or blank, git's own default
   remote is used. A push is the magit command most likely to outrun the
   client's default timeout — it waits on a remote. Pass `timeout_ms` to give
   it a longer budget."
  [{:keys [set_upstream remote directory] :as params}]
  (let [dir (resolve-directory directory)
        remote (some-> remote str str/trim not-empty)]
    (log/info "magit-push" {:set_upstream set_upstream :remote remote :directory dir})
    (handle-op :magit/push-failed
               {:op :magit/push :set-upstream (boolean set_upstream)
                :remote remote :directory dir}
               (emacs-timeout-ms params))))

(defn handle-magit-pull
  "Pull from upstream."
  [{:keys [directory] :as params}]
  (let [dir (resolve-directory directory)]
    (log/info "magit-pull" {:directory dir})
    (handle-op :magit/pull-failed {:op :magit/pull :directory dir}
               (emacs-timeout-ms params))))

(defn handle-magit-fetch
  "Fetch from remote(s). A blank or absent `remote` fetches git's default."
  [{:keys [remote directory] :as params}]
  (let [dir (resolve-directory directory)
        remote (some-> remote str str/trim not-empty)]
    (log/info "magit-fetch" {:remote remote :directory dir})
    (handle-op :magit/fetch-failed
               {:op :magit/fetch :remote remote :directory dir}
               (emacs-timeout-ms params))))

(defn handle-magit-feature-branches
  "Get list of feature/fix/feat branches (for /ship and /ship-pr skills)."
  [{:keys [directory] :as params}]
  (let [dir (resolve-directory directory)]
    (log/info "magit-feature-branches" {:directory dir})
    (handle-op :magit/feature-branches-failed
               {:op :magit/feature-branches :directory dir}
               (emacs-timeout-ms params))))

;; Tool definitions for magit handlers

;; IMPORTANT: When using a shared hive-mcp server across multiple projects,
;; Claude should ALWAYS pass its current working directory to these tools.
;; The directory can be found in Claude's prompt (e.g., ~/PP/funeraria/sisf-web)
;; or by running `pwd` in bash.

(def ^:private dir-desc
  "IMPORTANT: Pass your current working directory here to ensure git operations target YOUR project, not the MCP server's directory. Get it from your prompt path or run `pwd`.")

(def tools
  "REMOVED: Flat magit tools no longer exposed. Use consolidated `magit` tool."
  [])
