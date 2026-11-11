(ns hive-mcp.tools.magit
  "Magit integration handlers for MCP.
   Provides comprehensive git operations via magit addon:
   - Status, branches, log, diff; stage, commit, push, pull, fetch
   - Feature branch listing (to ship a branch: `git ship land|status|reconcile`)

   Result DSL: Internal logic returns Result maps ({:ok val} or {:error category}).
   Single try-result boundary at each handler level. Zero nested try-catch."
  (:require [hive-mcp.dns.result :as result]
            [hive-mcp.tools.core :as core :refer [mcp-success mcp-error emacs-timeout-ms]]
            [hive-mcp.tools.git-local-root :as local-root]
            [hive-mcp.context.request :as ctx]
            [taoensso.timbre :as log]
            [clojure.string :as str]
            [hive-spi.editor.services :as svc]
            [clojure.data.json :as json]
            [malli.core :as m]))
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

(def ^:private local-timeout-ms
  "Budget for a local git run (index, log, status) when the caller set none."
  60000)

(def ^:private remote-timeout-ms
  "Budget for a git run that waits on a network remote when the caller set none."
  120000)

(def ^:dynamic *repo-port*
  "The git and the filesystem the git verbs act on. nil means the real git CLI
   and the real disk; a test binds a fake world (hive-mcp.test.stub.git-world)
   so the default suite reaches no repository. Keys, each optional:
     :run             (fn [dir args in timeout-ms] {:exit :out})  one git run
     :kind            (fn [abs-path] {:type :file|:dir|:symlink|:absent
                                      :target-exists? bool}), never follows a link
     :snapshot-index  (fn [root] snapshot)
     :restore-index!  (fn [root snapshot])"
  nil)

(defn- real-kind [^String abs]
  (let [p (java.nio.file.Paths/get abs (make-array String 0))
        nofollow (into-array java.nio.file.LinkOption [java.nio.file.LinkOption/NOFOLLOW_LINKS])]
    (cond
      (java.nio.file.Files/isSymbolicLink p)
      {:type :symlink
       :target-exists? (java.nio.file.Files/exists p (make-array java.nio.file.LinkOption 0))}
      (java.nio.file.Files/isDirectory p nofollow) {:type :dir}
      (java.nio.file.Files/exists p nofollow) {:type :file}
      :else {:type :absent})))

(defn- path-kind
  "What sits at ABS on disk. A symlink is reported as a symlink, never as
   what it points at, so a dangling link is present, not deleted."
  [^String abs]
  (if-let [k (:kind *repo-port*)] (k abs) (real-kind abs)))

(defn- exists-follow?
  "java.io.File#exists over `path-kind`: a file, a directory, or a link whose
   target exists."
  [^String abs]
  (let [{:keys [type target-exists?]} (path-kind abs)]
    (boolean (or (#{:file :dir} type) (and (= :symlink type) target-exists?)))))

(declare real-git!)

(defn- git!
  "Run git ARGS in DIR, optionally feeding IN on stdin, for at most TIMEOUT-MS
   (nil = `local-timeout-ms`). {:exit :out}; a run that outlives its budget is
   destroyed and answers exit -1. Git never prompts: the server has no terminal
   to answer a credential or merge-message prompt, so such a run fails instead
   of hanging.

   Every run carries --literal-pathspecs: a path a caller names is that path
   and nothing else. Without it `pages/[id].tsx` is a character class matching
   pages/i.tsx and pages/d.tsx, and `*.txt` every text file. A bound
   `*repo-port*` answers instead of the git CLI."
  ([dir args] (git! dir args nil nil))
  ([dir args in] (git! dir args in nil))
  ([dir args in timeout-ms]
   (let [args (into ["--literal-pathspecs"] args)]
     (if-let [run (:run *repo-port*)]
       (run dir args in timeout-ms)
       (if-not (.isDirectory (java.io.File. ^String dir))
         {:exit -1 :out (str "not a directory: " dir)}
         (real-git! dir args in timeout-ms))))))

(defn- real-git!
  "One git run on the CLI; see `git!`."
  [dir args in timeout-ms]
  (let [budget (long (or timeout-ms local-timeout-ms))
        pb (-> (ProcessBuilder. ^java.util.List (into ["git"] args))
               (.directory (java.io.File. ^String dir))
               (.redirectErrorStream true))
        env (.environment pb)]
    (.put env "GIT_TERMINAL_PROMPT" "0")
    (.put env "GIT_MERGE_AUTOEDIT" "no")
    (let [process (.start pb)
          out (future (String. (.readAllBytes (.getInputStream process))
                               java.nio.charset.StandardCharsets/UTF_8))]
      (with-open [w (.getOutputStream process)]
        (when in (.write w (.getBytes ^String in java.nio.charset.StandardCharsets/UTF_8))))
      (if (.waitFor process budget java.util.concurrent.TimeUnit/MILLISECONDS)
        {:exit (.exitValue process) :out @out}
        (do (.destroyForcibly process)
            {:exit -1 :out (str "git " (str/join " " args) " timed out after " budget "ms")})))))

(defn- git-ok [{:keys [exit out]} category]
  (if (zero? exit)
    (result/ok out)
    (result/err category {:message (str category " - " (str/trim out))})))

(defn- git-out
  "Trimmed output of a successful git run in DIR, else nil."
  [dir args]
  (let [{:keys [exit out]} (git! dir args)]
    (when (zero? exit) (str/trim out))))

(defn- git-lines
  "Non-blank output lines of a successful git run in DIR, [] otherwise."
  [dir args]
  (if-let [out (git-out dir args)]
    (vec (remove str/blank? (str/split-lines out)))
    []))

(defn- recent-commits
  "The last N commits in DIR, shaped as the editor's api-log answers them."
  [dir n]
  (mapv (fn [line]
          (let [[h sh an ae s d] (str/split line (re-pattern (str (char 0))) -1)]
            {:hash (or h "") :short-hash (or sh "") :author (or an "") :email (or ae "")
             :subject (or s "") :date (or d "")}))
        (git-lines dir ["log" (str "-n" n) "--format=%H%x00%h%x00%an%x00%ae%x00%s%x00%ai"])))

(defn- cli-status
  "Repository status in DIR as the JSON the editor's api-status encodes, plus
   \"backend\": \"git-cli\" so a reader can tell which side answered."
  [dir]
  (if-let [root (git-out dir ["rev-parse" "--show-toplevel"])]
    (let [staged (git-lines dir ["diff" "--cached" "--name-only"])
          unstaged (git-lines dir ["diff" "--name-only"])
          untracked (git-lines dir ["ls-files" "--others" "--exclude-standard"])
          [behind ahead] (some-> (git-out dir ["rev-list" "--left-right" "--count" "@{upstream}...HEAD"])
                                 (str/split #"\s+"))]
      (result/ok
       (json/write-str
        {:repository root
         :branch (git-out dir ["rev-parse" "--abbrev-ref" "HEAD"])
         :upstream (git-out dir ["rev-parse" "--abbrev-ref" "@{upstream}"])
         :ahead (or (some-> ahead parse-long) 0)
         :behind (or (some-> behind parse-long) 0)
         :staged staged :staged-count (count staged)
         :unstaged unstaged :unstaged-count (count unstaged)
         :untracked untracked :untracked-count (count untracked)
         :stashes (git-lines dir ["stash" "list" "--oneline"])
         :recent-commits (recent-commits dir 5)
         :clean (every? empty? [staged unstaged untracked])
         :magit-available false
         :backend "git-cli"})))
    (result/err :magit/status-failed
                {:message (str ":magit/status-failed - not a git repository: " dir)})))

(defn- cli-branches [dir]
  {:current (git-out dir ["rev-parse" "--abbrev-ref" "HEAD"])
   :upstream (git-out dir ["rev-parse" "--abbrev-ref" "@{upstream}"])
   :local (git-lines dir ["branch" "--format=%(refname:short)"])
   :remote (git-lines dir ["branch" "-r" "--format=%(refname:short)"])})

(defn- cli-diff [dir target]
  (let [staged #(:out (git! dir ["diff" "--cached" "-U3"]))
        unstaged #(:out (git! dir ["diff" "-U3"]))]
    (result/ok (str/trim (case target
                           "unstaged" (unstaged)
                           "all" (str (staged) "\n---\n" (unstaged))
                           (staged))))))

(defn- push-argv
  "The git push argv for BRANCH to REMOTE. The refspec is always the explicit
   HEAD:refs/heads/<branch>, so the push lands on <remote>/<branch> whatever the
   branch tracks: a worktree cut from origin/staging tracks origin/staging, and
   a bare `git push` then refuses (push.default=simple, 'the upstream branch of
   your current branch does not match the name of your current branch') or, on
   push.default=upstream, would move staging. With SET-UPSTREAM, -u re-points
   tracking at <remote>/<branch>."
  [{:keys [remote branch set-upstream]}]
  (cond-> ["push"]
    set-upstream (conj "-u")
    true (conj remote (str "HEAD:refs/heads/" branch))))

(defn- push-remote
  "The remote a push goes to: the caller's, else the branch's own remote, else origin."
  [dir branch remote]
  (or remote
      (let [r (git-out dir ["config" "--get" (str "branch." branch ".remote")])]
        (when-not (or (str/blank? r) (= "." r)) r))
      "origin"))

(defn- cli-push
  "Push the current branch of DIR to <remote>/<same name>, never to a
   differently named upstream."
  [{:keys [directory remote set-upstream]} timeout-ms]
  (if-let [branch (git-out directory ["symbolic-ref" "--quiet" "--short" "HEAD"])]
    (let [remote (push-remote directory branch remote)
          target (str remote "/" branch)
          upstream (git-out directory ["rev-parse" "--abbrev-ref" "@{upstream}"])
          argv (push-argv {:remote remote :branch branch :set-upstream set-upstream})
          {:keys [exit out]} (git! directory argv nil (or timeout-ms remote-timeout-ms))]
      (if (zero? exit)
        (result/ok
         (str/join "\n"
                   (cond-> [(str "Pushed " branch " to " target ".")]
                     (and upstream (not= upstream target) set-upstream)
                     (conj (str "Upstream moved from " upstream " to " target "."))
                     (and upstream (not= upstream target) (not set-upstream))
                     (conj (str "Upstream left at " upstream "; set_upstream=true points it at "
                                target "."))
                     (and (nil? upstream) set-upstream)
                     (conj (str "Upstream set to " target "."))
                     (not (str/blank? out)) (conj (str/trim out)))))
        (result/err :magit/push-failed
                    {:message (str ":magit/push-failed - git " (str/join " " argv) " - "
                                   (str/trim out))})))
    (result/err :magit/push-failed
                {:message (str ":magit/push-failed - HEAD is detached in " directory
                               "; check out a branch to push")})))

(defn- cli-fetch
  "Fetch REMOTE, or each configured remote in turn when none is named. One
   unreachable remote does not fail the others: the answer names it and the
   fetch succeeds when at least one remote answered."
  [{:keys [directory remote]} timeout-ms]
  (let [remotes (if remote [remote] (git-lines directory ["remote"]))
        runs (mapv (fn [r] [r (git! directory ["fetch" r] nil (or timeout-ms remote-timeout-ms))])
                   remotes)
        fetched (filterv (comp zero? :exit second) runs)
        failed (filterv (comp not zero? :exit second) runs)
        failures (map (fn [[r {:keys [out]}]] (str "Could not fetch " r ": " (str/trim out))) failed)]
    (cond
      (empty? remotes)
      (result/err :magit/fetch-failed
                  {:message (str ":magit/fetch-failed - no remote configured in " directory)})

      (empty? fetched)
      (result/err :magit/fetch-failed
                  {:message (str ":magit/fetch-failed - " (str/join "\n" failures))})

      :else
      (result/ok (str/join "\n" (concat [(str "Fetched " (str/join ", " (map first fetched)) ".")]
                                        failures
                                        (keep (fn [[_ {:keys [out]}]] (not-empty (str/trim out)))
                                              fetched)))))))

(defn- cli-op
  "Plain-git twin of every closed :magit/* op: the backend for the remote ops,
   and the failover for the others when the editor vessel cannot run them (no
   Emacs, as for a coordinator in tmux, or an Emacs whose bridge did not load).
   nil for an op it does not know."
  ([op] (cli-op op nil))
  ([{:keys [op directory files paths message all target] :as m} timeout-ms]
   (case op
     :magit/stage
     (git-ok (git! directory (if (= :all files) ["add" "-A"] (into ["add" "-A" "--"] files)))
             :magit/stage-failed)

     :magit/stage-verify
     (if-let [missing (first (remove #(exists-follow? (.getPath (java.io.File. ^String directory ^String %)))
                                     paths))]
       (result/ok (json/write-str {:status "missing" :path missing}))
       (let [added (git! directory (into ["add" "--"] paths))]
         (if-not (zero? (:exit added))
           (git-ok added :magit/stage-failed)
           (let [staged (git! directory (into ["diff" "--cached" "--name-only" "--"] paths))]
             (result/ok (json/write-str {:status (if (str/blank? (:out staged)) "empty" "ok")}))))))

     :magit/commit
     (git-ok (git! directory (cond-> ["commit"] all (conj "-a") true (conj "-F" "-")) message)
             :magit/commit-failed)

     :magit/status (cli-status directory)
     :magit/branches (result/ok (json/write-str (cli-branches directory)))
     :magit/log (result/ok (json/write-str (recent-commits directory (or (:count m) 10))))
     :magit/diff (cli-diff directory target)
     :magit/feature-branches
     (let [{:keys [current local]} (cli-branches directory)]
       (result/ok (json/write-str
                   {:current current
                    :feature_branches (filterv #(re-find #"^(feature|fix|feat)/" %) local)})))
     :magit/push (cli-push m timeout-ms)
     :magit/pull (git-ok (git! directory ["pull" "--no-edit"] nil (or timeout-ms remote-timeout-ms))
                         :magit/pull-failed)
     :magit/fetch (cli-fetch m timeout-ms)
     nil)))

(def ^:private cli-backed-ops
  "Every op `cli-op` can run."
  #{:magit/stage :magit/stage-verify :magit/commit :magit/status :magit/branches
    :magit/log :magit/diff :magit/feature-branches :magit/push :magit/pull :magit/fetch})

(def ^:private cli-only-ops
  "Ops that never reach the editor. They wait on a network remote, which is
   where a 5 s emacsclient budget times out, and the editor's push let a bare
   `git push` pick the refspec from the branch's upstream, which refuses (or
   targets the wrong branch) when that upstream has another name."
  #{:magit/push :magit/pull :magit/fetch})

(def ^:private idempotent-ops
  "Ops safe to run again on the CLI after the editor timed out on them: the
   reads, and staging (adding a path twice leaves the same index). A commit is
   not here: a timed-out commit may have landed, and a rerun would commit again."
  #{:magit/status :magit/branches :magit/log :magit/diff :magit/feature-branches
    :magit/stage :magit/stage-verify})

(def ^:private vessel-never-ran
  "Vessel failures that prove the op never executed in the editor: no vessel
   registered, the Emacs bridge did not load, the circuit breaker is open, or
   emacsclient could not reach (or find) a daemon. Rerunning the op on the CLI
   after one of these cannot run it twice."
  #"(?i)bridge entrypoints failed to load|circuit breaker open|can't find socket|connection refused|server did not respond|socket.*not available|cannot run program|capability-unavailable|:unsupported")

(defn- failover?
  "True when op OP should run on the CLI after the vessel answered ENVELOPE."
  [op {:keys [error timed-out]}]
  (boolean
   (and (:directory op)
        (or (re-find vessel-never-ran (str error))
            (and timed-out (contains? idempotent-ops (:op op)))))))

(def ^:dynamic *handshake-race-ms*
  "An 'entrypoints failed to load' answered faster than this is a load race
   (another emacsclient call mid-load, a cold daemon), worth one retry on the
   editor. A slower one is a hung handshake: the editor is not retried, the op
   goes straight to the CLI."
  1500)

(def ^:dynamic *handshake-retry-delay-ms*
  "Pause before the single handshake retry."
  250)

(def ^:private handshake-race
  #"(?i)bridge entrypoints failed to load")

(defn- scrub
  "A vessel error as the git tool shows it: hive-emacs's hint ends with 'Plain
   CLI git works meanwhile', which sends agents off hive-git; the tool has its
   own CLI backend, so that sentence is dropped."
  [error]
  (str/trim (str/replace (str error) #"\s*Plain CLI git works meanwhile\.?" "")))

(defn- invoke-vessel
  "[envelope elapsed-ms] of one editor dispatch of OP."
  [op timeout-ms]
  (let [t0 (System/nanoTime)
        envelope (svc/invoke :vessel :dispatch op timeout-ms)]
    [envelope (quot (- (System/nanoTime) t0) 1000000)]))

(defn- handshake-race?
  "True when ENVELOPE is a fast 'entrypoints failed to load': the op never ran
   and the bridge is likely mid-load, so one retry on the editor is safe."
  [{:keys [success timed-out error]} elapsed-ms]
  (boolean (and (not success) (not timed-out)
                (re-find handshake-race (str error))
                (< elapsed-ms *handshake-race-ms*))))

(def ^:dynamic *bridge-down-ms*
  "After a slow handshake failure or an editor timeout, how long the bridge is
   remembered as down: ops in that window skip the editor and run on the CLI
   at once, instead of each paying the full handshake budget again (about 25 s
   per op against a daemon wedged on a prompt)."
  30000)

(defonce ^:private bridge-latch
  ;; {:dispatch <the vessel dispatch fn that failed> :until epoch-ms :why text}.
  ;; Keyed by the dispatch fn, so a re-registered vessel starts trusted.
  (atom nil))

(defn- vessel-dispatch-fn []
  (get-in (svc/registered) [:vessel :dispatch]))

(defn- latched-down
  "The latch, when the registered vessel is remembered as down right now."
  []
  (let [{:keys [dispatch until] :as l} @bridge-latch]
    (when (and l (identical? dispatch (vessel-dispatch-fn))
               (< (System/currentTimeMillis) until))
      l)))

(defn- latch-down! [why]
  (when (pos? *bridge-down-ms*)
    (reset! bridge-latch {:dispatch (vessel-dispatch-fn) :why why
                          :until (+ (System/currentTimeMillis) *bridge-down-ms*)})))

(defn- invoke-with-handshake-retry
  "Dispatch OP to the editor; retry ONCE after a short pause when the first
   answer is a handshake race (see `handshake-race?`).
   {:envelope :retried? :elapsed-ms}: what the error text may claim is read
   from here, never assumed."
  [op timeout-ms]
  (let [[envelope elapsed] (invoke-vessel op timeout-ms)]
    (if (handshake-race? envelope elapsed)
      (do (log/info "magit op: bridge handshake race, retrying once" {:op (:op op) :elapsed-ms elapsed})
          (Thread/sleep (long *handshake-retry-delay-ms*))
          (let [[envelope2 elapsed2] (invoke-vessel op timeout-ms)]
            {:envelope envelope2 :retried? true :elapsed-ms elapsed2}))
      {:envelope envelope :retried? false :elapsed-ms elapsed})))

(defn- bridge-state
  "One line naming the bridge state behind a vessel ERROR, from what actually
   happened (ATTEMPT is `invoke-with-handshake-retry`'s answer, or a latch).
   hive-emacs reports a dead daemon, an open circuit breaker and a missing
   socket all as 'entrypoints failed to load', so that text alone never
   proves a race: after the retry failed too, the bridge is down."
  [error {:keys [retried? elapsed-ms latched]}]
  (cond
    latched
    (str "bridge down: remembered from an earlier call (" (:why latched)
         "); the editor was skipped for up to " (quot *bridge-down-ms* 1000) " s")

    (and (re-find handshake-race (str error)) retried?)
    (str "bridge down: its entrypoints failed to load on the first call and again on one retry "
         "(a dead or wedged daemon, an open circuit breaker, or a bad load-path)")

    (re-find handshake-race (str error))
    (str "bridge down: its entrypoints failed to load after " elapsed-ms
         " ms (a hung handshake); the editor was not retried")

    :else (str "bridge down: " (first (str/split-lines (scrub error))))))

(defn- on-cli-backend
  "R, a CLI failover result; an error says which backend ran and what to do."
  [r error attempt]
  (if (find r :ok)
    r
    (update r :message str
            "\nThis ran on the hive-git git CLI backend because the Emacs editor could not run it ("
            (bridge-state error attempt)
            "). Next: fix the git error above, then re-run the same hive git call; "
            "it takes the CLI backend whenever Emacs cannot answer.")))

(defn- latch-worthy?
  "True when ATTEMPT shows the bridge is down for longer than one call: the
   editor timed out, or the entrypoints failed slowly or twice."
  [{:keys [envelope retried? elapsed-ms]}]
  (let [{:keys [success timed-out error]} envelope]
    (boolean (and (not success)
                  (or timed-out
                      (and (re-find handshake-race (str error))
                           (or retried? (>= elapsed-ms *handshake-race-ms*))))))))

(defn- dispatch->result
  "Run a closed magit operation and convert its answer to Result.
   Remote ops (push, pull, fetch) run on the git CLI. Every other op goes to
   the editor vessel first (one retry on a handshake race, see
   `invoke-with-handshake-retry`) and fails over to the git CLI in :directory
   when the vessel never ran it (see `vessel-never-ran`), or, for an
   idempotent op, timed out. A commit that timed out is NOT rerun: it may have
   completed. Every error names the bridge state and the next step."
  [op timeout-ms]
  (cond
    (contains? cli-only-ops (:op op))
    (cli-op op timeout-ms)

    ;; The bridge was seen down moments ago: the editor is not asked again, so
    ;; the op never runs there and the CLI cannot run it twice.
    (and (:directory op) (contains? cli-backed-ops (:op op)) (latched-down))
    (let [latched (latched-down)]
      (log/info "magit op: bridge remembered down, running on the git CLI" {:op (:op op)})
      (on-cli-backend (cli-op op timeout-ms) (:why latched) {:latched latched}))

    :else
    (let [{:keys [envelope] :as attempt} (invoke-with-handshake-retry op timeout-ms)
          {:keys [success result error]} envelope]
      (when (latch-worthy? attempt)
        (latch-down! (first (str/split-lines (scrub error)))))
      (when success (reset! bridge-latch nil))
      (cond
        success (result/ok result)
        (failover? op envelope)
        (do (log/info "magit op failing over to the git CLI" {:op (:op op) :vessel-error (str error)})
            (if-let [r (cli-op op timeout-ms)]
              (on-cli-backend r error attempt)
              (result/err :magit/dispatch-failed
                          {:message (str (scrub error) "\n" (bridge-state error attempt)
                                         "; " (:op op) " has no git CLI backend. Next: re-run once Emacs answers (emacsclient --eval t).")})))
        (:timed-out envelope)
        (result/err :magit/dispatch-failed
                    {:message (str (scrub error) "\nbridge timed out: the editor may still have run " (:op op)
                                   "; it was not retried, so nothing ran twice. Next: read `git status` (hive git command=status), "
                                   "then repeat the call only if the change is not there.")})
        :else (result/err :magit/dispatch-failed
                          {:message (str (scrub error) "\nThe editor answered an error; this is not a bridge outage. "
                                         "Next: fix the cause above and re-run the call.")})))))


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

(defn staged-outside-files
  "Return staged paths not named by FILES. An explicit directory covers paths
   beneath it; a named file covers only itself. Git's --no-renames reports both
   sides of a staged rename, so either side outside the selection is refused."
  [files staged-paths]
  (let [selected (map #(if (str/starts-with? % "./") (subs % 2) %) files)]
    (->> staged-paths
         (remove (fn [path]
                   (some (fn [selected-path]
                           (or (= path selected-path)
                               (and (str/ends-with? selected-path "/")
                                    (str/starts-with? path selected-path))))
                         selected)))
         vec)))

(defn- names?
  "True when the root-relative path REL names ENTRY: the path itself, or a
   path beneath it when REL is a directory."
  [rel entry]
  (or (= rel ".") (= entry rel) (str/starts-with? entry (str rel "/"))))

(defn- abs-in [^String root ^String rel]
  (.getPath (java.io.File. root rel)))

(defn- staged-paths
  "Read the index directly, without asking the shared Emacs buffer to choose a repo.
   A failed git inspection is a refusal, never an empty index."
  [dir]
  (try
    (let [{:keys [exit out]} (git! dir ["diff" "--cached" "--name-only" "--no-renames" "-z"])]
      (if (zero? exit)
        (result/ok (vec (remove str/blank? (str/split out (re-pattern (str (char 0)))))))
        (result/err :magit/index-inspection-failed
                    {:message (str ":magit/index-inspection-failed - git diff --cached failed in " dir ": " out)})))
    (catch Exception e
      (result/err :magit/index-inspection-failed
                  {:message (str ":magit/index-inspection-failed - " (.getMessage e))}))))

(defn- verify-index-scope
  "Refuse an explicit-files commit if the index holds any other path. FILES are
   root-relative (see `resolve-paths`) when RESOLVED?, and ROOT is the
   repository top; a named path covers itself and, when it is or was a
   directory, its descendants, whether or not it still exists on disk."
  ([files dir] (verify-index-scope files dir false))
  ([files dir resolved?]
   (let [inspection (staged-paths dir)
         scope-files (if resolved?
                       files
                       (mapv (fn [path]
                               (if (and (not (str/ends-with? path "/"))
                                        (= :dir (:type (path-kind (abs-in dir path)))))
                                 (str path "/")
                                 path)) files))]
     (if-let [paths (:ok inspection)]
       (let [extras (if resolved?
                      (filterv (fn [p] (not-any? #(names? % p) scope-files)) paths)
                      (staged-outside-files scope-files paths))]
         (if (seq extras)
           (result/err :magit/unrelated-staged
                       {:message (str ":magit/unrelated-staged - refusing commit; index also contains: "
                                      (pr-str extras)
                                      ". Next: name them in files, unstage them, or pass include_staged=true.")})
           (result/ok files)))
       inspection))))

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

(defn- nio ^java.nio.file.Path [^String s]
  (java.nio.file.Paths/get s (make-array String 0)))

(defn- resolve-paths
  "Each caller PATH as the repo-root-relative path it names, never a pattern.
   A relative path is read from DIR (a subdirectory works: `./a.txt` from
   <repo>/src is src/a.txt); an absolute path must lie inside the repository.
   (ok {:root R :paths [rel ...] :resolved? true}), or (err
   :magit/path-outside-repo). When DIR is not inside a git work tree nothing
   is resolved, (ok {:root DIR :paths PATHS :resolved? false}), and the ops
   report that themselves."
  [paths dir]
  (if-let [root (git-out dir ["rev-parse" "--show-toplevel"])]
    (let [d (.normalize (nio dir))
          r (.normalize (nio root))
          prefix (nio (or (git-out dir ["rev-parse" "--show-prefix"]) ""))
          one (fn [p]
                (let [pp (.normalize (nio p))
                      rel (cond
                            (not (.isAbsolute pp)) (.normalize (.resolve prefix pp))
                            (.startsWith pp d) (.normalize (.resolve prefix (.relativize d pp)))
                            (.startsWith pp r) (.relativize r pp))
                      s (some-> rel str)]
                  (cond
                    (or (nil? s) (= s "..") (str/starts-with? s "../")) {:outside p}
                    (= s "") {:rel "."}
                    :else {:rel s})))
          rs (mapv one paths)
          outside (keep :outside rs)]
      (if (seq outside)
        (result/err :magit/path-outside-repo
                    {:message (str ":magit/path-outside-repo - " (str/join " " outside)
                                   " is not inside the repository " root
                                   ". Next: name paths relative to " dir " or absolute under " root ".")})
        (result/ok {:root root :paths (vec (distinct (map :rel rs))) :resolved? true})))
    (result/ok {:root dir :paths (vec paths) :resolved? false})))

(defn- tracked-under
  "Index and HEAD entries REL names in ROOT (literal pathspec, exact match)."
  [root rel]
  (filterv #(names? rel %)
           (distinct (concat (git-lines root ["ls-files" "--cached" "--" rel])
                             (git-lines root ["ls-tree" "-r" "--name-only" "HEAD" "--" rel])))))

(defn- cli-staged
  "The root-relative PATHS that are staged on the git CLI rather than through
   the editor's stage-verify, which tests existence by following links: a
   tracked path with NOTHING on disk (a deletion) and a symlink (dangling or
   not; its target is not the caller's business). A path that exists on disk
   is never treated as deleted."
  [paths root]
  (filterv (fn [rel]
             (let [{:keys [type]} (path-kind (abs-in root rel))]
               (or (= :symlink type)
                   (and (= :absent type) (seq (tracked-under root rel))))))
           paths))

(defn- index-entries
  "ROOT's index as {path \"mode sha stage\"}, or nil when git cannot read it."
  [root]
  (let [{:keys [exit out]} (git! root ["ls-files" "-s" "-z"])]
    (when (zero? exit)
      (into {} (keep (fn [rec]
                       (when-let [[_ meta p] (re-matches #"(?s)(\S+ \S+ \S+)\t(.*)" rec)]
                         [p meta])))
            (str/split out (re-pattern (str (char 0))))))))

(defn- index-file ^java.io.File [root]
  (when-let [p (git-out root ["rev-parse" "--git-path" "index"])]
    (let [f (java.io.File. ^String p)]
      (if (.isAbsolute f) f (java.io.File. ^String root ^String p)))))

(defn- snapshot-index
  "ROOT's index as it stands, to put back if the call is refused."
  [root]
  {:entries (index-entries root)
   :raw (if-let [f (:snapshot-index *repo-port*)]
          (f root)
          (when-let [file (index-file root)]
            {:file file
             :bytes (when (.isFile file) (java.nio.file.Files/readAllBytes (.toPath file)))}))})

(defn- restore-index!
  "Put ROOT's index back as SNAP (from `snapshot-index`) recorded it."
  [root {:keys [raw]}]
  (if-let [f (:restore-index! *repo-port*)]
    (f root raw)
    (when-let [^java.io.File file (:file raw)]
      (if-let [bytes (:bytes raw)]
        (java.nio.file.Files/write (.toPath file) ^bytes bytes
                                   ^"[Ljava.nio.file.OpenOption;" (make-array java.nio.file.OpenOption 0))
        (.delete file))))
  nil)

(defn- touched-outside
  "Index paths whose entry changed since SNAP that no root-relative path in
   PATHS names: what a stage or commit must never touch."
  [root snap paths]
  (let [before (:entries snap)
        after (index-entries root)]
    (when (and before after)
      (->> (into (set (keys before)) (keys after))
           (filter #(not= (get before %) (get after %)))
           (remove (fn [p] (some #(names? % p) paths)))
           sort vec))))

(defn- scope-violation
  "The refusal for a stage that changed index paths the caller did not name."
  [outside]
  (result/err :magit/scope-violation
              {:message (str ":magit/scope-violation - staging changed index paths that were not named: "
                             (str/join " " outside)
                             "; the index was put back as it was. Next: name each path to stage.")}))

(def ^:private pathspec-magic
  "A path git (or Magit, which passes no --literal-pathspecs) would read as a
   pattern: wildcards, a character class, an escape, a :(magic) prefix."
  #"[*?\[\\]|^:")

(defn- stage-literal-cli
  "Stage root-relative PATHS in ROOT on the git CLI, literally: a tracked path
   with nothing on disk is removed from the index (rm --cached -r, so a
   deleted directory works too), every other path is added (add -A, so a
   symlink is recorded as the link it is). (ok PATHS) or (err)."
  [paths root]
  (let [gone (filterv #(= :absent (:type (path-kind (abs-in root %)))) paths)
        here (vec (remove (set gone) paths))
        rm (when (seq gone)
             (git! root (into ["rm" "--cached" "-r" "-q" "--ignore-unmatch" "--"] gone)))
        add (when (and (seq here) (or (nil? rm) (zero? (:exit rm))))
              (git! root (into ["add" "-A" "--"] here)))
        failed (first (filter #(and % (not (zero? (:exit %)))) [rm add]))]
    (if failed
      (git-ok failed :magit/stage-failed)
      (result/ok paths))))

(defn- unknown-paths
  "The root-relative PATHS that name nothing: absent from disk and from the
   index and HEAD."
  [paths root]
  (filterv #(and (= :absent (:type (path-kind (abs-in root %))))
                 (empty? (tracked-under root %)))
           paths))

(defn- stage-explicit
  "Stage the caller's PATHS for a commit, each read literally and resolved
   against the repository root (see `resolve-paths`).

   - A path that exists on disk is never staged as a deletion; a tracked path
     with nothing on disk is (git rm --cached -r).
   - Deletions, symlinks and paths git or Magit would read as a pattern are
     staged on the git CLI with --literal-pathspecs; the rest go through the
     editor's stage-verify (failing over to the CLI).
   - The index is snapshotted first. If staging changed an index entry the
     caller did not name, or the call fails, the index is put back as it was:
     a refused stage leaves nothing behind.

   (ok {:root R :paths [rel ...] :resolved? bool}), or an error Result."
  [paths dir timeout-ms]
  (let [res (resolve-paths paths dir)]
    (if-not (find res :ok)
      res
      (let [{:keys [root paths resolved?] :as where} (:ok res)]
        (if-not resolved?
          (let [r (stage-verify paths dir timeout-ms)]
            (if (find r :ok) (result/ok where) r))
          (if-let [unknown (first (unknown-paths paths root))]
            (result/err :magit/path-not-found
                        {:message (str ":magit/path-not-found - no such path under " root ": " unknown
                                       " (not on disk, not in the index, not in HEAD). Next: check the spelling;"
                                       " paths are read literally, never as patterns.")})
            (let [snap (snapshot-index root)
                  cli (vec (distinct (concat (cli-staged paths root)
                                             (filter #(re-find pathspec-magic %) paths))))
                  rest-paths (vec (remove (set cli) paths))
                  r (cond-> (if (seq cli) (stage-literal-cli cli root) (result/ok cli))
                      (seq rest-paths)
                      (as-> r (if (find r :ok)
                                (let [v (stage-verify rest-paths root timeout-ms)]
                                  ;; "at least one named path staged": a deletion
                                  ;; staged above counts, the final check decides.
                                  (if (and (seq cli) (= :magit/nothing-staged (:error v)))
                                    (result/ok paths)
                                    v))
                                r)))
                  outside (when (find r :ok) (touched-outside root snap paths))
                  staged-any (when (and (find r :ok) (empty? outside))
                               (seq (git-lines root (into ["diff" "--cached" "--name-only" "--"] paths))))]
              (cond
                (not (find r :ok)) (do (restore-index! root snap) r)
                (seq outside) (do (restore-index! root snap) (scope-violation outside))
                (not staged-any)
                (do (restore-index! root snap)
                    (result/err :magit/nothing-staged
                                {:message (str ":magit/nothing-staged - staging left the index empty for "
                                               (str/join " " paths)
                                               "; refusing to commit what was already staged")}))
                :else (result/ok where)))))))))

(defn- ignored-paths
  "The root-relative PATHS that are untracked and gitignored."
  [paths root]
  (filterv #(and (empty? (tracked-under root %))
                 (seq (git-lines root ["ls-files" "--others" "--ignored" "--exclude-standard" "--" %])))
           paths))

(defn- stage-report
  "The stage verb's answer for the root-relative PATHS in ROOT once the stage
   op returned R; SNAP is the index before the call. It is read back from the
   index, never from the op's answer (the editor's api-stage answers nil
   whatever git did):
   - paths that still carry changes are staged on the git CLI;
   - a path that names nothing, or only an ignored file, is an error;
   - an index entry the caller did not name that changed is an error;
   on any error the index is put back as SNAP recorded it. A path with no
   change to stage is named as such, not reported as staged."
  [r paths root snap]
  (let [fail (fn [e] (restore-index! root snap) e)]
    (if-not (find r :ok)
      (fail r)
      (let [left (vec (distinct (concat (git-lines root (into ["diff" "--name-only" "--"] paths))
                                        (git-lines root (into ["ls-files" "--others" "--exclude-standard" "--"] paths)))))
            fixed (if (seq left) (stage-literal-cli paths root) r)
            outside (when (find fixed :ok) (touched-outside root snap paths))
            ignored (when (find fixed :ok) (ignored-paths paths root))
            staged (set (git-lines root (into ["diff" "--cached" "--name-only" "--"] paths)))
            staged? (fn [p] (some #(names? p %) staged))
            done (filterv staged? paths)
            unchanged (filterv (complement staged?) paths)]
        (cond
          (not (find fixed :ok)) (fail fixed)
          (seq outside) (fail (scope-violation outside))
          (seq ignored)
          (fail (result/err :magit/path-ignored
                            {:message (str ":magit/path-ignored - " (str/join " " ignored)
                                           " is ignored by .gitignore and was not staged; the index was put back. "
                                           "Next: drop it from files, or change .gitignore.")}))
          :else
          (result/ok (str/join "\n"
                               (cond-> []
                                 (seq done) (conj (str "Staged " (count done) " path(s): " (str/join " " done)))
                                 (seq unchanged) (conj (str "Nothing to stage for " (str/join " " unchanged)
                                                            ": no change against the index."))
                                 (seq left) (conj (str "The editor answered without staging "
                                                       (str/join " " left)
                                                       "; they were staged on the git CLI backend."))))))))))

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

(defn- stage-named
  "Stage the caller's NORM paths in DIR: resolved literally against the
   repository root, never read as patterns, verified against the index."
  [norm dir timeout-ms]
  (let [res (resolve-paths norm dir)]
    (if-not (find res :ok)
      res
      (let [{:keys [root paths resolved?]} (:ok res)]
        (if-not resolved?
          (dispatch->result {:op :magit/stage :files paths :directory dir} timeout-ms)
          (if-let [unknown (first (unknown-paths paths root))]
            (result/err :magit/path-not-found
                        {:message (str ":magit/path-not-found - no such path under " root ": " unknown
                                       " (not on disk, not in the index, not in HEAD). Next: check the spelling;"
                                       " paths are read literally, never as patterns.")})
            (let [snap (snapshot-index root)
                  cli (vec (distinct (concat (cli-staged paths root)
                                             (filter #(re-find pathspec-magic %) paths))))
                  rest-paths (vec (remove (set cli) paths))
                  r (cond-> (if (seq cli) (stage-literal-cli cli root) (result/ok nil))
                      (seq rest-paths)
                      (as-> r (if (find r :ok)
                                (dispatch->result {:op :magit/stage :files rest-paths :directory root}
                                                  timeout-ms)
                                r)))]
              (stage-report r paths root snap))))))))

(defn handle-magit-stage
  "Stage files for commit.

   `files` takes a list of paths, a single path, several paths in one
   whitespace-separated string, or 'all' for every modified file. An absent or
   blank `files` is an error, never a silent no-op. Paths are literal (never
   globs) and resolve against the repository root; the answer is read back
   from the index and names what was staged (see `stage-report`)."
  [{:keys [files directory] :as params}]
  (let [dir (resolve-directory directory)
        norm (normalize-files files)
        timeout-ms (emacs-timeout-ms params)]
    (log/info "magit-stage" {:files norm :directory dir})
    (cond
      (nil? norm)
      (mcp-error (str ":magit/no-files - files is required: a path, a list of paths, "
                      "a whitespace-separated path string, or 'all'"))

      (= :all norm)
      (result->mcp
       (try-result :magit/stage-failed
                   #(let [r (dispatch->result {:op :magit/stage :files :all :directory dir} timeout-ms)]
                      (if (and (find r :ok) (str/blank? (str (:ok r))))
                        (result/ok "Staged all changes")
                        r))))

      :else
      (result->mcp (try-result :magit/stage-failed #(stage-named norm dir timeout-ms))))))

(defn handle-magit-commit
  "Create a commit with the given message.

   `files` restricts the commit to explicit paths: a list, a single path,
   several paths in one whitespace-separated string, or 'all'. Named paths are
   read literally (never as globs), resolved against the repository root
   (absolute paths inside it work), staged and VERIFIED before the commit runs
   (see `stage-explicit`); a path that names nothing, or a stage that leaves
   the index empty for those paths, fails the operation instead of committing
   whatever the index already held. An explicit path list also refuses a
   foreign staged path, naming it, unless include_staged is true. A deps.edn
   :local/root leaving the repo is refused (git-local-root/refusal). Every
   refusal before the commit runs puts the index back as it was."
  [{:keys [message all directory files include_staged] :as params}]
  (let [dir (resolve-directory directory)
        timeout-ms (emacs-timeout-ms params)
        norm (normalize-files files)
        stage-all (boolean (or all (= :all norm)))
        root (when (vector? norm) (git-out dir ["rev-parse" "--show-toplevel"]))
        snap (when root (snapshot-index root))
        refuse (fn [r] (when snap (restore-index! root snap)) (result->mcp r))
        staged (when (vector? norm) (stage-explicit norm dir timeout-ms))]
    (log/info "magit-commit" {:message-len (count message) :all stage-all
                              :files norm :directory dir})
    (if (and (some? staged) (nil? (find staged :ok)))
      (refuse staged)
      (let [{:keys [root paths resolved?]} (:ok staged)
            scope (when (and (vector? norm) (not stage-all) (not include_staged))
                    (if resolved?
                      (verify-index-scope paths root true)
                      (verify-index-scope norm dir)))]
        (if (and (some? scope) (nil? (find scope :ok)))
          (refuse scope)
          (if-let [refused (local-root/refusal dir stage-all)]
            (do (when snap (restore-index! root snap)) refused)
            (handle-op :magit/commit-failed {:op :magit/commit :message message
                                             :all stage-all :directory dir} timeout-ms)))))))

(defn handle-magit-push
  "Push the current branch to <remote>/<current-branch>, on the git CLI.

   The refspec is always explicit (HEAD:refs/heads/<branch>), so a branch that
   tracks a differently named upstream (a worktree cut from origin/staging)
   pushes to its own name and never to that upstream. set_upstream=true points
   tracking at <remote>/<branch>. `remote` selects the remote; omitted or
   blank, the branch's configured remote is used, else origin. Runs without
   Emacs; `timeout_ms` bounds the git run (default 120000)."
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
  "Get list of feature/fix/feat branches. To ship one, use `git ship land|status|reconcile`."
  [{:keys [directory] :as params}]
  (let [dir (resolve-directory directory)]
    (log/info "magit-feature-branches" {:directory dir})
    (handle-op :magit/feature-branches-failed
               {:op :magit/feature-branches :directory dir}
               (emacs-timeout-ms params))))

;;; =============================================================================
;;; Contracts
;;; =============================================================================

(def Files
  "The `files` parameter as callers send it: a path collection, one path,
   a whitespace-separated path string, or the `all` marker."
  [:maybe [:or :string :keyword :symbol [:sequential :any] [:set :any]]])

(m/=> resolve-directory [:=> [:cat :any] :string])

(m/=> normalize-files
      [:=> [:cat Files] [:maybe [:or [:= :all] [:vector {:min 1} :string]]]])

(m/=> staged-outside-files
      [:=> [:cat [:sequential :string] [:sequential :string]] [:vector :string]])

;; Every handler answers one MCP text response through the Result boundary.
;; A malformed timeout_ms THROWS by design (core/emacs-timeout-ms).
(m/=> handle-magit-status [:=> [:cat [:maybe :map]] core/McpResponse])

(m/=> handle-magit-branches [:=> [:cat [:maybe :map]] core/McpResponse])

(m/=> handle-magit-log [:=> [:cat [:maybe :map]] core/McpResponse])

(m/=> handle-magit-diff [:=> [:cat [:maybe :map]] core/McpResponse])

(m/=> handle-magit-stage [:=> [:cat [:maybe :map]] core/McpResponse])

(m/=> handle-magit-commit [:=> [:cat [:maybe :map]] core/McpResponse])

(m/=> handle-magit-push [:=> [:cat [:maybe :map]] core/McpResponse])

(m/=> handle-magit-pull [:=> [:cat [:maybe :map]] core/McpResponse])

(m/=> handle-magit-fetch [:=> [:cat [:maybe :map]] core/McpResponse])

(m/=> handle-magit-feature-branches [:=> [:cat [:maybe :map]] core/McpResponse])

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
