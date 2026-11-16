(ns hive-mcp.tools.magit-cli-fallback-test
  (:require [clojure.data.json :as json]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.magit :as magit]
            [hive-mcp.test.stub.swarm-host :as swarm]
            [hive-spi.editor.services :as svc]))

(defn- temp-repo []
  (let [dir (str (java.nio.file.Files/createTempDirectory
                  "magit-cli" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (sh/sh "git" "init" "-q" :dir dir)
    (sh/sh "git" "config" "user.name" "test" :dir dir)
    (sh/sh "git" "config" "user.email" "test@example.invalid" :dir dir)
    (sh/sh "git" "config" "commit.gpgsign" "false" :dir dir)
    (spit (str dir "/a.txt") "a\n")
    dir))

(def ^:private cli-op #'magit/cli-op)

(deftest commit-path-ops-run-on-plain-git
  (let [dir (temp-repo)]
    (testing "stage-verify answers the same JSON verdicts the vessel op does"
      (is (= {:status "missing" :path "nope.txt"}
             (json/read-str (:ok (cli-op {:op :magit/stage-verify :directory dir
                                          :paths ["nope.txt"]}))
                            :key-fn keyword)))
      (is (= {:status "ok"}
             (json/read-str (:ok (cli-op {:op :magit/stage-verify :directory dir
                                          :paths ["a.txt"]}))
                            :key-fn keyword))))
    (testing "commit records the message from stdin"
      (is (contains? (cli-op {:op :magit/commit :directory dir :message "first commit"}) :ok))
      (is (= "first commit" (str/trim (:out (sh/sh "git" "log" "-1" "--format=%s" :dir dir))))))
    (testing "an op with no plain-git twin stays unsupported"
      (is (nil? (cli-op {:op :magit/eval :directory dir}))))))

;;; ===========================================================================
;;; A. push goes to <remote>/<current-branch>, never a differently named upstream
;;; (friction 20261109130937-248e5d53)
;;; ===========================================================================

(defn- git [dir & args]
  (let [r (apply sh/sh "git" (concat args [:dir dir]))]
    (when-not (zero? (:exit r)) (throw (ex-info (str "git " (str/join " " args) " failed") r)))
    (str/trim (:out r))))

(defn- worktree-cut-from-staging
  "A bare remote with `staging`, and a clone whose branch BRANCH was cut from
   origin/staging, so it TRACKS origin/staging, carrying one new commit."
  [branch]
  (let [root (str (java.nio.file.Files/createTempDirectory
                   "magit-push" (make-array java.nio.file.attribute.FileAttribute 0)))
        remote (str root "/remote.git")
        work (str root "/work")]
    (sh/sh "git" "init" "-q" "--bare" remote)
    (sh/sh "git" "clone" "-q" remote work)
    (doseq [[k v] [["user.name" "test"] ["user.email" "test@example.invalid"]
                   ["commit.gpgsign" "false"] ["push.default" "simple"]]]
      (git work "config" k v))
    (git work "commit" "-q" "--allow-empty" "-m" "base")
    (git work "push" "-q" "origin" "HEAD:refs/heads/staging")
    (git work "fetch" "-q" "origin")
    (git work "checkout" "-q" "-b" branch "--track" "origin/staging")
    (git work "commit" "-q" "--allow-empty" "-m" "work")
    {:remote remote :work work}))

(defn- remote-sha [remote ref]
  (some-> (sh/sh "git" "rev-parse" "--verify" "-q" ref :dir remote) :out str/trim not-empty))

(def ^:private no-vessel
  "A vessel that is never reached for remote ops; answering it is a failure."
  (fn [_ _] {:success false :error "vessel must not be called"}))

(deftest push-targets-the-branch-own-name-not-its-upstream
  (let [{:keys [remote work]} (worktree-cut-from-staging "fix/x")
        staging-before (remote-sha remote "refs/heads/staging")
        head (git work "rev-parse" "HEAD")]
    (testing "plain git refuses the bare push: the defect the tool inherited"
      (is (not (zero? (:exit (sh/sh "git" "push" :dir work))))))
    (swarm/with-swarm-host [host no-vessel]
      (let [r (magit/handle-magit-push {:directory work})]
        (is (nil? (:isError r)) (:text r))
        (is (str/includes? (:text r) "origin/fix/x"))
        (is (empty? (swarm/calls host)) "push never reaches the editor")))
    (is (= head (remote-sha remote "refs/heads/fix/x")) "landed on origin/fix/x")
    (is (= staging-before (remote-sha remote "refs/heads/staging"))
        "the differently named upstream is never moved")
    (is (= "origin/staging" (git work "rev-parse" "--abbrev-ref" "@{upstream}"))
        "without set_upstream, tracking is left alone")))

(deftest push-with-set-upstream-repoints-tracking-at-the-branch-own-name
  (let [{:keys [remote work]} (worktree-cut-from-staging "ci/test-before-tag")
        staging-before (remote-sha remote "refs/heads/staging")]
    (swarm/with-swarm-host [_ no-vessel]
      (let [r (magit/handle-magit-push {:directory work :set_upstream true :remote "origin"})]
        (is (nil? (:isError r)) (:text r))
        (is (str/includes? (:text r) "Upstream moved from origin/staging to origin/ci/test-before-tag"))))
    (is (= "origin/ci/test-before-tag" (git work "rev-parse" "--abbrev-ref" "@{upstream}")))
    (is (= (git work "rev-parse" "HEAD") (remote-sha remote "refs/heads/ci/test-before-tag")))
    (is (= staging-before (remote-sha remote "refs/heads/staging")))))

(deftest push-argv-always-names-the-branch
  (let [push-argv #'magit/push-argv]
    (is (= ["push" "origin" "HEAD:refs/heads/fix/x"]
           (push-argv {:remote "origin" :branch "fix/x" :set-upstream false})))
    (is (= ["push" "-u" "gitea" "HEAD:refs/heads/fix/x"]
           (push-argv {:remote "gitea" :branch "fix/x" :set-upstream true})))))

(deftest push-on-a-detached-head-is-a-named-refusal
  (let [{:keys [work]} (worktree-cut-from-staging "fix/d")]
    (git work "checkout" "-q" "--detach")
    (let [r (magit/handle-magit-push {:directory work})]
      (is (true? (:isError r)))
      (is (str/includes? (:text r) "detached")))))

;;; ===========================================================================
;;; B. every verb works with no Emacs, or an Emacs whose bridge will not load
;;; (frictions 20261109131824-3da0c04c, 20261109131132-74fed135,
;;;  20261109121337-44bc247b, 20261109122140-5a9b7abb, 20261109122531-4b8ec731,
;;;  20261109131603-13e12bbf, 20261109132721-79819464)
;;; ===========================================================================

(def ^:private bridge-down
  "The answer an Emacs whose bridge entrypoints did not load gives."
  (fn [_ _] {:success false :timed-out false
             :error (str "Emacs bridge entrypoints failed to load. Fix: check the Emacs "
                         "daemon answers (emacsclient --eval t).")}))

(def ^:private emacs-timeout
  (fn [_ _] {:success false :timed-out true :error "Emacsclient call timed out after 5000ms"}))

(defn- without-vessel
  "Run F with no :vessel capability registered at all (a coordinator in tmux)."
  [f]
  (let [prior (get (svc/registered) :vessel)]
    (svc/unregister-services! :vessel)
    (try (f) (finally (swarm/restore! prior)))))

(defn- reads-and-stage-succeed [work]
  (let [st (magit/handle-magit-status {:directory work})]
    (is (nil? (:isError st)) (:text st))
    (is (= "fix/b" (:branch (json/read-str (:text st) :key-fn keyword))))
    (is (not (re-find #"(?i)plain (cli )?git" (:text st)))))
  (is (nil? (:isError (magit/handle-magit-branches {:directory work}))))
  (is (nil? (:isError (magit/handle-magit-log {:directory work :count 2}))))
  (is (nil? (:isError (magit/handle-magit-diff {:directory work}))))
  (is (nil? (:isError (magit/handle-magit-feature-branches {:directory work}))))
  (is (nil? (:isError (magit/handle-magit-fetch {:directory work}))))
  (is (nil? (:isError (magit/handle-magit-stage {:directory work :files ["b.txt"]})))))

(deftest every-verb-fails-over-when-the-bridge-does-not-load
  (let [{:keys [remote work]} (worktree-cut-from-staging "fix/b")]
    (spit (str work "/b.txt") "b\n")
    (testing "bridge entrypoints failed to load"
      (swarm/with-swarm-host [_ bridge-down] (reads-and-stage-succeed work)))
    (testing "no vessel registered at all"
      (without-vessel #(reads-and-stage-succeed work)))
    (swarm/with-swarm-host [_ bridge-down]
      (let [c (magit/handle-magit-commit {:directory work :message "add b" :files ["b.txt"]})]
        (is (nil? (:isError c)) (:text c))
        (is (= "add b" (git work "log" "-1" "--format=%s"))))
      (let [p (magit/handle-magit-push {:directory work :set_upstream true})]
        (is (nil? (:isError p)) (:text p))
        (is (= (git work "rev-parse" "HEAD") (remote-sha remote "refs/heads/fix/b")))))))

(deftest a-read-fails-over-after-an-editor-timeout
  (let [{:keys [work]} (worktree-cut-from-staging "fix/t")]
    (swarm/with-swarm-host [_ emacs-timeout]
      (let [r (magit/handle-magit-status {:directory work})]
        (is (nil? (:isError r)) (:text r))
        (is (str/includes? (:text r) "git-cli"))))))

(deftest a-commit-that-timed-out-in-the-editor-is-not-rerun
  (let [{:keys [work]} (worktree-cut-from-staging "fix/c")
        before (git work "rev-parse" "HEAD")]
    (swarm/with-swarm-host [_ emacs-timeout]
      (let [r (magit/handle-magit-commit {:directory work :message "maybe" :all true})]
        (is (true? (:isError r)))
        (is (str/includes? (:text r) "not retried"))
        (is (= before (git work "rev-parse" "HEAD")) "no second commit was made")))))

(deftest fetch-of-every-remote-survives-one-unreachable-remote
  (let [{:keys [work]} (worktree-cut-from-staging "fix/f")]
    (git work "remote" "add" "tunnel" "/nonexistent/remote.git")
    (let [r (magit/handle-magit-fetch {:directory work})]
      (is (nil? (:isError r)) (:text r))
      (is (str/includes? (:text r) "Fetched origin"))
      (is (str/includes? (:text r) "Could not fetch tunnel")))))
