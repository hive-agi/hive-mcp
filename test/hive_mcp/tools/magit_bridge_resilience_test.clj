(ns hive-mcp.tools.magit-bridge-resilience-test
  "stage / commit / batch-commit when the Emacs bridge is down or racing
   (friction wave 2026-11-11, lane git-bridge: frictions 20261110132904-06e01a4f,
   20261110133549-604564c6, 20261110133832-0c081375; 20261110123738-56bf2ca2,
   20261110132809-56c88df0)."
  (:require [clojure.data.json :as json]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.consolidated.git :as git-tool]
            [hive-mcp.tools.magit :as magit]
            [hive-mcp.test.stub.swarm-host :as swarm]))

(defn- git [dir & args]
  (let [r (apply sh/sh "git" (concat args [:dir dir]))]
    (when-not (zero? (:exit r)) (throw (ex-info (str "git " (str/join " " args) " failed") r)))
    (str/trim (:out r))))

(defn- repo
  "A repo with one commit holding a.txt and b.txt."
  []
  (let [dir (str (java.nio.file.Files/createTempDirectory
                  "magit-bridge" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (git dir "init" "-q")
    (doseq [[k v] [["user.name" "test"] ["user.email" "test@example.invalid"]
                   ["commit.gpgsign" "false"]]]
      (git dir "config" k v))
    (spit (str dir "/a.txt") "a\n")
    (spit (str dir "/b.txt") "b\n")
    (git dir "add" "-A")
    (git dir "commit" "-q" "-m" "base")
    dir))

(def ^:private entrypoints-failed
  {:success false :timed-out false
   :error (str "Emacs bridge entrypoints failed to load. Fix: check the Emacs daemon "
               "answers (emacsclient --eval t). Plain CLI git works meanwhile.")})

(def ^:private circuit-open
  {:success false :timed-out false
   :error "Circuit breaker open (backoff 2000ms, 1500ms remaining). Last error: server did not respond"})

(defn- scripted
  "A RESPOND that answers the Nth call with the Nth of ANSWERS (the last repeats)."
  [& answers]
  (let [n (atom -1)]
    (fn [_ _] (nth answers (min (swap! n inc) (dec (count answers)))))))

(def ^:private vessel-status
  {:success true :timed-out false :result (json/write-str {:branch "from-vessel"})})

;;; ---------------------------------------------------------------------------
;;; Bounded retry on the transient handshake
;;; ---------------------------------------------------------------------------

(deftest a-fast-entrypoint-failure-is-retried-on-the-vessel-once
  (let [dir (repo)]
    (swarm/with-swarm-host [host (scripted entrypoints-failed vessel-status)]
      (let [r (magit/handle-magit-status {:directory dir})]
        (is (nil? (:isError r)) (:text r))
        (is (str/includes? (:text r) "from-vessel") "the retry reached the editor")
        (is (= 2 (count (swarm/calls-of host :magit/status))))))))

(deftest the-retry-is-bounded-then-the-cli-answers
  (let [dir (repo)]
    (swarm/with-swarm-host [host (scripted entrypoints-failed)]
      (let [r (magit/handle-magit-status {:directory dir})]
        (is (nil? (:isError r)) (:text r))
        (is (str/includes? (:text r) "git-cli"))
        (is (= 2 (count (swarm/calls-of host :magit/status))) "one retry, no more")))))

(deftest a-dead-bridge-is-not-retried
  (let [dir (repo)]
    (swarm/with-swarm-host [host (scripted circuit-open)]
      (let [r (magit/handle-magit-status {:directory dir})]
        (is (str/includes? (:text r) "git-cli"))
        (is (= 1 (count (swarm/calls-of host :magit/status))))))))

(deftest a-slow-entrypoint-failure-is-a-hung-handshake-and-not-retried
  (let [dir (repo)]
    (binding [magit/*handshake-race-ms* 0]
      (swarm/with-swarm-host [host (scripted entrypoints-failed vessel-status)]
        (let [r (magit/handle-magit-status {:directory dir})]
          (is (str/includes? (:text r) "git-cli"))
          (is (= 1 (count (swarm/calls-of host :magit/status)))))))))

;;; ---------------------------------------------------------------------------
;;; Error text: retryable versus down, and the next step
;;; ---------------------------------------------------------------------------

(deftest a-failed-cli-commit-names-the-backend-and-never-sends-to-plain-git
  (let [dir (repo)]
    (swarm/with-swarm-host [_ (scripted entrypoints-failed)]
      (let [r (magit/handle-magit-commit {:directory dir :message "nothing" :all false})]
        (is (true? (:isError r)))
        (is (str/includes? (:text r) "git CLI backend") (:text r))
        (is (re-find #"(?i)bridge (down|retryable)" (:text r)) (:text r))
        (is (str/includes? (:text r) "Next:") (:text r))
        (is (not (re-find #"(?i)plain (cli )?git" (:text r))) (:text r))))))

(deftest a-timed-out-commit-says-it-is-not-safely-retryable
  (let [dir (repo)
        before (git dir "rev-parse" "HEAD")]
    (spit (str dir "/a.txt") "changed\n")
    (swarm/with-swarm-host [_ (constantly {:success false :timed-out true
                                           :error "Emacsclient call timed out after 5000ms"})]
      (let [r (magit/handle-magit-commit {:directory dir :message "m" :all true})]
        (is (true? (:isError r)))
        (is (re-find #"(?i)bridge timed out" (:text r)) (:text r))
        (is (str/includes? (:text r) "Next:") (:text r))
        (is (= before (git dir "rev-parse" "HEAD")))))))

;;; ---------------------------------------------------------------------------
;;; stage never answers a bare nil, and never claims success on an unstaged path
;;; ---------------------------------------------------------------------------

(deftest a-vessel-stage-answering-nil-and-staging-nothing-is-completed-on-the-cli
  (let [dir (repo)]
    (spit (str dir "/a.txt") "a2\n")
    (spit (str dir "/c.txt") "c\n")
    (swarm/with-swarm-host [_ (constantly {:success true :timed-out false :result "nil"})]
      (let [r (magit/handle-magit-stage {:directory dir :files ["a.txt" "c.txt"]})]
        (is (nil? (:isError r)) (:text r))
        (is (not= "nil" (:text r)))
        (is (str/includes? (:text r) "Staged 2 path(s)") (:text r))))
    (is (= #{"a.txt" "c.txt"} (set (str/split-lines (git dir "diff" "--cached" "--name-only")))))))

(deftest a-json-array-and-a-string-stage-the-same
  (doseq [files [["a.txt" "b.txt"] "a.txt b.txt"]]
    (let [dir (repo)]
      (spit (str dir "/a.txt") "a2\n")
      (spit (str dir "/b.txt") "b2\n")
      (swarm/with-swarm-host [_ (scripted entrypoints-failed)]
        (let [r (magit/handle-magit-stage {:directory dir :files files})]
          (is (str/includes? (:text r) "Staged 2 path(s)") (pr-str files (:text r)))))
      (is (= #{"a.txt" "b.txt"} (set (str/split-lines (git dir "diff" "--cached" "--name-only"))))
          (pr-str files)))))

;;; ---------------------------------------------------------------------------
;;; A deleted path in an explicit files list stages as a deletion
;;; ---------------------------------------------------------------------------

(defn- deleted? [dir path]
  (= (str "D\t" path) (git dir "diff" "--name-status" "HEAD~1" "HEAD" "--" path)))

(deftest a-path-removed-from-disk-commits-as-a-deletion
  (let [dir (repo)]
    (.delete (java.io.File. dir "a.txt"))
    (swarm/with-swarm-host [_ (scripted entrypoints-failed)]
      (let [r (magit/handle-magit-commit {:directory dir :message "drop a" :files ["a.txt"]})]
        (is (nil? (:isError r)) (:text r))))
    (is (= "drop a" (git dir "log" "-1" "--format=%s")))
    (is (deleted? dir "a.txt"))))

(deftest a-git-rmd-path-commits-as-a-deletion
  (let [dir (repo)]
    (git dir "rm" "-q" "a.txt")
    (swarm/with-swarm-host [_ (scripted entrypoints-failed)]
      (let [r (magit/handle-magit-commit {:directory dir :message "rm a" :files ["a.txt"]})]
        (is (nil? (:isError r)) (:text r))))
    (is (deleted? dir "a.txt"))))

(deftest a-deleted-path-beside-an-edited-one-goes-through-the-vessel-for-the-rest
  (let [dir (repo)
        verified (atom nil)]
    (.delete (java.io.File. dir "a.txt"))
    (spit (str dir "/b.txt") "b2\n")
    (swarm/with-swarm-host [_ (fn [op _]
                                (case (:op op)
                                  :magit/stage-verify
                                  (do (reset! verified (:paths op))
                                      (git dir "add" "--" "b.txt")
                                      {:success true :result (json/write-str {:status "ok"})})
                                  :magit/commit
                                  (do (git dir "commit" "-q" "-m" (:message op))
                                      {:success true :result "committed"})))]
      (let [r (magit/handle-magit-commit {:directory dir :message "both" :files ["a.txt" "b.txt"]})]
        (is (nil? (:isError r)) (:text r))))
    (is (= ["b.txt"] @verified) "the vessel only sees paths that exist")
    (is (deleted? dir "a.txt"))))

(deftest a-path-git-never-knew-is-still-path-not-found
  (let [dir (repo)]
    (swarm/with-swarm-host [_ (scripted entrypoints-failed)]
      (let [r (magit/handle-magit-commit {:directory dir :message "x" :files ["nope.txt"]})]
        (is (true? (:isError r)))
        (is (str/includes? (:text r) "path-not-found"))))))

(deftest batch-commit-survives-the-bridge-and-stages-deletions
  (let [dir (repo)]
    (.delete (java.io.File. dir "a.txt"))
    (spit (str dir "/b.txt") "b2\n")
    (swarm/with-swarm-host [_ (scripted entrypoints-failed)]
      (let [r (git-tool/handle-git {:command "batch-commit" :directory dir
                                    :operations [{:message "drop a" :files ["a.txt"]}
                                                 {:message "edit b" :files "b.txt"}]})
            summary (:summary (json/read-str (:text r) :key-fn keyword))]
        (is (= {:total 2 :success 2 :failed 0} summary) (:text r))))
    (is (= ["edit b" "drop a"] (str/split-lines (git dir "log" "-2" "--format=%s"))))))
