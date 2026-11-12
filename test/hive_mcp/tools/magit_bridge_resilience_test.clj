(ns hive-mcp.tools.magit-bridge-resilience-test
  "stage / commit / batch-commit when the Emacs bridge is down or racing
   (friction wave 2026-11-11, lane git-bridge: frictions 20261110132904-06e01a4f,
   20261110133549-604564c6, 20261110133832-0c081375; 20261110123738-56bf2ca2,
   20261110132809-56c88df0).

   Default suite, on stubs only (convention 20261111105238-0ceaa524): the
   repository is hive-mcp.test.stub.git-world bound as `magit/*repo-port*`, the
   editor vessel is a scripted swarm-host stub. No git binary, no disk, no
   Emacs, nothing left in /tmp."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.consolidated.git :as git-tool]
            [hive-mcp.tools.magit :as magit]
            [hive-mcp.test.stub.git-world :as gw]
            [hive-mcp.test.stub.swarm-host :as swarm]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- repo
  "A world with one commit holding a.txt and b.txt; extra keys go to gw/world."
  ([] (repo {}))
  ([opts] (gw/world (merge {:files {"a.txt" "a\n" "b.txt" "b\n"}} opts))))

(defmacro ^:private on
  "BODY with W bound as the repository, no latch carried between tests and
   no pause before the handshake retry."
  [w & body]
  `(binding [magit/*repo-port* (gw/port ~w)
             magit/*bridge-down-ms* 0
             magit/*handshake-retry-delay-ms* 0]
     ~@body))

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

(defn- staged
  "Paths whose index entry differs from HEAD (deletions included)."
  [w]
  (let [h (gw/head w) i (gw/index w)]
    (set (filter #(not= (get h %) (get i %)) (distinct (concat (keys h) (keys i)))))))

(defn- messages [w] (mapv :message (gw/commits w)))

(defn- deleted?
  "The last commit deleted PATH."
  [w path]
  (and (contains? (:changes (peek (gw/commits w))) path)
       (not (contains? (gw/head w) path))))

;;; ---------------------------------------------------------------------------
;;; Bounded retry on the transient handshake
;;; ---------------------------------------------------------------------------

(deftest a-fast-entrypoint-failure-is-retried-on-the-vessel-once
  (let [w (repo)]
    (on w
      (swarm/with-swarm-host [host (scripted entrypoints-failed vessel-status)]
        (let [r (magit/handle-magit-status {:directory gw/root})]
          (is (nil? (:isError r)) (:text r))
          (is (str/includes? (:text r) "from-vessel") "the retry reached the editor")
          (is (= 2 (count (swarm/calls-of host :magit/status)))))))))

(deftest the-retry-is-bounded-then-the-cli-answers
  (let [w (repo)]
    (on w
      (swarm/with-swarm-host [host (scripted entrypoints-failed)]
        (let [r (magit/handle-magit-status {:directory gw/root})]
          (is (nil? (:isError r)) (:text r))
          (is (str/includes? (:text r) "git-cli"))
          (is (= 2 (count (swarm/calls-of host :magit/status))) "one retry, no more"))))))

(deftest a-dead-bridge-is-not-retried
  (let [w (repo)]
    (on w
      (swarm/with-swarm-host [host (scripted circuit-open)]
        (let [r (magit/handle-magit-status {:directory gw/root})]
          (is (str/includes? (:text r) "git-cli"))
          (is (= 1 (count (swarm/calls-of host :magit/status)))))))))

(deftest a-slow-entrypoint-failure-is-a-hung-handshake-and-not-retried
  (let [w (repo)]
    (on w
      (binding [magit/*handshake-race-ms* 0]
        (swarm/with-swarm-host [host (scripted entrypoints-failed vessel-status)]
          (let [r (magit/handle-magit-status {:directory gw/root})]
            (is (str/includes? (:text r) "git-cli"))
            (is (= 1 (count (swarm/calls-of host :magit/status))))))))))

;;; ---------------------------------------------------------------------------
;;; Error text: retryable versus down, and the next step
;;; ---------------------------------------------------------------------------

(deftest a-failed-cli-commit-names-the-backend-and-never-sends-to-plain-git
  (let [w (repo)]
    (on w
      (swarm/with-swarm-host [_ (scripted entrypoints-failed)]
        (let [r (magit/handle-magit-commit {:directory gw/root :message "nothing" :all false})]
          (is (true? (:isError r)))
          (is (str/includes? (:text r) "git CLI backend") (:text r))
          (is (re-find #"(?i)bridge (down|retryable)" (:text r)) (:text r))
          (is (str/includes? (:text r) "Next:") (:text r))
          (is (not (re-find #"(?i)plain (cli )?git" (:text r))) (:text r)))))))

(deftest a-timed-out-commit-says-it-is-not-safely-retryable
  (let [w (repo)]
    (gw/write! w "a.txt" "changed\n")
    (on w
      (swarm/with-swarm-host [_ (constantly {:success false :timed-out true
                                             :error "Emacsclient call timed out after 5000ms"})]
        (let [r (magit/handle-magit-commit {:directory gw/root :message "m" :all true})]
          (is (true? (:isError r)))
          (is (re-find #"(?i)bridge timed out" (:text r)) (:text r))
          (is (str/includes? (:text r) "Next:") (:text r)))))
    (is (empty? (gw/commits w)) "the commit was not rerun on the CLI")))

;;; ---------------------------------------------------------------------------
;;; stage never answers a bare nil
;;; ---------------------------------------------------------------------------

(deftest a-vessel-stage-answering-nil-and-staging-nothing-is-completed-on-the-cli
  (let [w (repo)]
    (gw/write! w "a.txt" "a2\n")
    (gw/write! w "c.txt" "c\n")
    (on w
      (swarm/with-swarm-host [_ (constantly {:success true :timed-out false :result "nil"})]
        (let [r (magit/handle-magit-stage {:directory gw/root :files ["a.txt" "c.txt"]})]
          (is (nil? (:isError r)) (:text r))
          (is (not= "nil" (:text r)))
          (is (str/includes? (:text r) "Staged 2 path(s)") (:text r)))))
    (is (= #{"a.txt" "c.txt"} (staged w)))))

(deftest a-json-array-and-a-string-stage-the-same
  (doseq [files [["a.txt" "b.txt"] "a.txt b.txt"]]
    (let [w (repo)]
      (gw/write! w "a.txt" "a2\n")
      (gw/write! w "b.txt" "b2\n")
      (on w
        (swarm/with-swarm-host [_ (scripted entrypoints-failed)]
          (let [r (magit/handle-magit-stage {:directory gw/root :files files})]
            (is (str/includes? (:text r) "Staged 2 path(s)") (pr-str files (:text r))))))
      (is (= #{"a.txt" "b.txt"} (staged w)) (pr-str files)))))

;;; ---------------------------------------------------------------------------
;;; A deleted path in an explicit files list stages as a deletion
;;; ---------------------------------------------------------------------------

(deftest a-path-removed-from-disk-commits-as-a-deletion
  (let [w (repo)]
    (gw/rm! w "a.txt")
    (on w
      (swarm/with-swarm-host [_ (scripted entrypoints-failed)]
        (let [r (magit/handle-magit-commit {:directory gw/root :message "drop a" :files ["a.txt"]})]
          (is (nil? (:isError r)) (:text r)))))
    (is (= ["drop a"] (messages w)))
    (is (deleted? w "a.txt"))))

(deftest a-git-rmd-path-commits-as-a-deletion
  (let [w (repo)]
    (gw/rm! w "a.txt")
    (swap! w update :index dissoc "a.txt")
    (on w
      (swarm/with-swarm-host [_ (scripted entrypoints-failed)]
        (let [r (magit/handle-magit-commit {:directory gw/root :message "rm a" :files ["a.txt"]})]
          (is (nil? (:isError r)) (:text r)))))
    (is (deleted? w "a.txt"))))

(deftest a-deleted-path-beside-an-edited-one-goes-through-the-vessel-for-the-rest
  (let [w (repo)
        verified (atom nil)
        editor (gw/editor w)]
    (gw/rm! w "a.txt")
    (gw/write! w "b.txt" "b2\n")
    (on w
      (swarm/with-swarm-host [_ (fn [op t]
                                  (when (= :magit/stage-verify (:op op)) (reset! verified (:paths op)))
                                  (editor op t))]
        (let [r (magit/handle-magit-commit {:directory gw/root :message "both" :files ["a.txt" "b.txt"]})]
          (is (nil? (:isError r)) (:text r)))))
    (is (= ["b.txt"] @verified) "the vessel only sees paths that exist")
    (is (deleted? w "a.txt"))
    (is (= "b2\n" (get (gw/head w) "b.txt")))))

(deftest a-path-git-never-knew-is-still-path-not-found
  (let [w (repo)]
    (on w
      (swarm/with-swarm-host [_ (scripted entrypoints-failed)]
        (let [r (magit/handle-magit-commit {:directory gw/root :message "x" :files ["nope.txt"]})]
          (is (true? (:isError r)))
          (is (str/includes? (:text r) "path-not-found")))))
    (is (empty? (gw/commits w)))))

(deftest batch-commit-survives-the-bridge-and-stages-deletions
  (let [w (repo)]
    (gw/rm! w "a.txt")
    (gw/write! w "b.txt" "b2\n")
    (on w
      (swarm/with-swarm-host [_ (scripted entrypoints-failed)]
        (let [r (git-tool/handle-git {:command "batch-commit" :directory gw/root
                                      :operations [{:message "drop a" :files ["a.txt"]}
                                                   {:message "edit b" :files "b.txt"}]})
              summary (:summary (json/read-str (:text r) :key-fn keyword))]
          (is (= {:total 2 :success 2 :failed 0} summary) (:text r)))))
    (is (= ["drop a" "edit b"] (messages w)))))
