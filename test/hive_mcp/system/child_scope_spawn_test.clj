(ns hive-mcp.system.child-scope-spawn-test
  "Spawn paths through child-scope: executable resolution, the expiring probe,
   scope-start failure recovery, and proof that headless lings and stdio
   bridges route through it. The seam is child-scope/*probe* and *prefix*
   (dynamic vars read at call time): a prefix of `env MARK=1` stands in for
   systemd-run, so the scoped path is observable without a user manager, and
   a prefix that prints systemd-run's failure line and exits 1 simulates a
   scope that could not start."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.system.child-scope :as cs]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.addons.mcp-bridge :as bridge]
            [hive-mcp.addons.stdio-bridge :as stdio])
  (:import [java.util.concurrent TimeUnit]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private path (System/getenv "PATH"))

(def ^:private marking-prefix
  "Stands in for systemd-run: execs the command with SCOPED_BY_TEST=1 set."
  ["env" "SCOPED_BY_TEST=1"])

(def ^:private failing-prefix
  "Behaves like systemd-run when the user manager refuses the scope: one line
   on stderr, exit 1, the command never runs (it lands in $1.. and is ignored)."
  ["sh" "-c" "echo 'Failed to start transient scope unit: Connection timed out' >&2; exit 1" "sh"])

(defn- run-out
  "Start pb through cs/start! with opts and return [stdout exit]."
  [^ProcessBuilder pb opts]
  (let [p (cs/start! pb opts)
        out (slurp (.getInputStream p))]
    (.waitFor p 10 TimeUnit/SECONDS)
    [(str/trim out) (.exitValue p)]))

(defn- printenv-pb []
  (ProcessBuilder. ^java.util.List ["sh" "-c" "echo mark=${SCOPED_BY_TEST:-none}"]))

;; ---------------------------------------------------------------------------
;; Resolution (finding 1)
;; ---------------------------------------------------------------------------

(deftest resolve-executable-follows-the-childs-path
  (is (some? (cs/resolve-executable "sh" path nil)))
  (is (nil? (cs/resolve-executable "hive-no-such-exe-x" path nil)))
  (testing "the CHILD's PATH decides, not the JVM's"
    (is (nil? (cs/resolve-executable "sh" "/nonexistent-dir" nil))))
  (testing "a path must exist and be executable"
    (is (some? (cs/resolve-executable "/bin/sh" "" nil)))
    (is (nil? (cs/resolve-executable "/nonexistent/hive-x" path nil)))
    (let [f (java.io.File/createTempFile "child-scope" ".txt")]
      (try (is (nil? (cs/resolve-executable (.getPath f) path nil)) "not executable")
           (finally (.delete f)))))
  (testing "relative path against cwd"
    (is (some? (cs/resolve-executable "./sh" path "/bin")))
    (is (nil? (cs/resolve-executable "./sh" path "/")))))

(deftest plan-argv-scopes-only-what-resolves
  (is (= (into cs/scope-prefix ["sh" "-c" "x"]) (cs/plan-argv true ["sh" "-c" "x"] {:path path})))
  (is (= ["hive-no-such-exe-x"] (cs/plan-argv true ["hive-no-such-exe-x"] {:path path})))
  (is (= ["sh"] (cs/plan-argv false ["sh"] {:path path})))
  (is (= ["env" "A=1" "sh"] (cs/plan-argv true ["sh"] {:path path :prefix ["env" "A=1"]}))))

(deftest missing-executable-still-throws-at-start
  (testing "probe true, but the command does not resolve: plain argv, IOException"
    (let [pb (ProcessBuilder. ^java.util.List ["/nonexistent/hive-no-such-exe"])]
      (is (thrown? java.io.IOException
                   (cs/start! pb {:probe (constantly true) :prefix marking-prefix})))
      (is (= ["/nonexistent/hive-no-such-exe"] (vec (.command pb))) "never prefixed"))))

;; ---------------------------------------------------------------------------
;; Expiring probe (finding 2)
;; ---------------------------------------------------------------------------

(deftest expiring-probe-reprobes-after-ttl-and-records-answers
  (let [now (atom 0)
        calls (atom 0)
        probe (cs/expiring-probe #(do (swap! calls inc) true) {:ttl-ms 1000 :clock #(deref now)})]
    (is (true? (probe)))
    (is (true? (probe)))
    (is (= 1 @calls) "cached within the ttl")
    (reset! now 1500)
    (is (true? (probe)))
    (is (= 2 @calls) "re-probed after the ttl")
    (cs/record-answer! probe false)
    (is (false? (probe)) "a recorded failure stands")
    (is (= 2 @calls))
    (reset! now 3000)
    (is (true? (probe)) "and expires like any answer")
    (is (= 3 @calls)))
  (testing "a throwing probe answers false; record-answer! on a plain fn is a no-op"
    (is (false? ((cs/expiring-probe #(throw (ex-info "no bus" {}))))))
    (is (nil? (cs/record-answer! (constantly true) false)))))

(deftest scope-start-failure-signature
  (is (cs/scope-start-failure? {:exit 1 :stdout-quiet? true
                                :stderr "Failed to start transient scope unit: Connection timed out\n"}))
  (is (cs/scope-start-failure? {:exit 1 :stdout-quiet? true
                                :stderr "Failed to connect to bus: No such file or directory"}))
  (is (not (cs/scope-start-failure? {:exit 1 :stdout-quiet? false
                                     :stderr "Failed to start transient scope unit"}))
      "the command wrote stdout: it ran")
  (is (not (cs/scope-start-failure? {:exit 2 :stdout-quiet? true
                                     :stderr "Failed to start transient scope unit"})))
  (is (not (cs/scope-start-failure? {:exit 1 :stdout-quiet? true :stderr "boom"})))
  (is (not (cs/scope-start-failure? {:exit 1 :stdout-quiet? true :stderr ""}))))

;; ---------------------------------------------------------------------------
;; start!: scoped path, recovery, no false retries
;; ---------------------------------------------------------------------------

(deftest start-routes-through-the-prefix
  (is (= ["mark=1" 0] (run-out (printenv-pb) {:probe (constantly true) :prefix marking-prefix})))
  (is (= ["mark=none" 0] (run-out (printenv-pb) {:probe (constantly false) :prefix marking-prefix})))
  (testing "the dynamic seam is read at call time"
    (binding [cs/*probe* (constantly true) cs/*prefix* marking-prefix]
      (is (= ["mark=1" 0] (run-out (printenv-pb) {}))))))

(deftest scope-start-failure-retries-plain-and-flips-the-probe
  (let [now (atom 0)
        probe (cs/expiring-probe (constantly true) {:ttl-ms 60000 :clock #(deref now)})]
    (is (true? (probe)))
    (is (= ["mark=none" 0] (run-out (printenv-pb) {:probe probe :prefix failing-prefix}))
        "the command ran once, plain")
    (is (false? (probe)) "the cached answer flipped to false")
    (is (= ["mark=none" 0] (run-out (printenv-pb) {:probe probe :prefix marking-prefix}))
        "later spawns go plain until the ttl runs out")
    (reset! now 120000)
    (is (= ["mark=1" 0] (run-out (printenv-pb) {:probe probe :prefix marking-prefix}))
        "then the probe is asked again")))

(deftest a-command-that-ran-is-never-retried
  (let [probe (cs/expiring-probe (constantly true))
        marker (java.io.File/createTempFile "child-scope-ran" ".log")]
    (try
      (testing "exit 1 with the failure text, but the command ran (it wrote stdout)"
        (let [pb (ProcessBuilder. ^java.util.List
                                  ["sh" "-c" (str "echo ran >> " (.getPath marker)
                                                  "; echo out; echo 'Failed to start transient scope unit' >&2; exit 1")])]
          (is (= ["out" 1] (run-out pb {:probe probe :prefix marking-prefix})))))
      (testing "an ordinary failing command"
        (let [pb (ProcessBuilder. ^java.util.List ["sh" "-c" (str "echo ran >> " (.getPath marker) "; exit 1")])]
          (is (= ["" 1] (run-out pb {:probe probe :prefix marking-prefix})))))
      (is (= 2 (count (str/split-lines (slurp marker)))) "each command ran exactly once")
      (is (true? (probe)) "no failure recorded")
      (finally (.delete marker)))))

;; ---------------------------------------------------------------------------
;; Callers: headless lings and stdio bridges route through child-scope
;; ---------------------------------------------------------------------------

(defn- await-line [ling-id pred]
  (loop [n 0]
    (let [lines (map #(if (map? %) (:text %) %) (headless/get-stdout ling-id))]
      (or (some #(when (pred %) %) lines)
          (when (< n 100) (Thread/sleep 50) (recur (inc n)))))))

(deftest headless-spawn-routes-through-child-scope
  (let [cwd (System/getProperty "java.io.tmpdir")]
    (testing "scoped: the prefix wraps the ling's argv"
      (let [id (str "child-scope-test-" (random-uuid))]
        (try
          (binding [cs/*probe* (constantly true) cs/*prefix* marking-prefix]
            (headless/spawn-headless! id {:cwd cwd :claude-cmd "printenv" :task "SCOPED_BY_TEST"}))
          (is (= "1" (await-line id #(= "1" %))))
          (finally (try (headless/kill-headless! id {:force? true}) (catch Exception _ nil))))))
    (testing "missing executable: plain argv, :headless/spawn-failed, nothing registered"
      (let [id (str "child-scope-test-" (random-uuid))]
        (binding [cs/*probe* (constantly true) cs/*prefix* marking-prefix]
          (let [e (try (headless/spawn-headless! id {:cwd cwd :claude-cmd "/nonexistent/hive-no-such-ling"})
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
            (is (some? e))
            (is (str/includes? (str (ex-message e)) "Failed to start headless ling process"))
            (is (not (headless/headless? id)))))))
    (testing "scope fails to start: the ling still runs, plain"
      (let [id (str "child-scope-test-" (random-uuid))
            probe (cs/expiring-probe (constantly true))]
        (try
          (binding [cs/*probe* probe cs/*prefix* failing-prefix]
            (headless/spawn-headless! id {:cwd cwd :claude-cmd "echo" :task "plain-ran"}))
          (is (= "plain-ran" (await-line id #(= "plain-ran" %))))
          (is (false? (probe)))
          (finally (try (headless/kill-headless! id {:force? true}) (catch Exception _ nil))))))))

(def ^:private fake-mcp-server
  "Answers initialize with serverInfo.name = $SCOPED_BY_TEST (or none)."
  ["sh" "-c" (str "read l; printf '%s\\n' \"{\\\"jsonrpc\\\":\\\"2.0\\\",\\\"id\\\":1,\\\"result\\\":"
                  "{\\\"protocolVersion\\\":\\\"2024-11-05\\\",\\\"serverInfo\\\":{\\\"name\\\":\\\"${SCOPED_BY_TEST:-none}\\\"}}}\"; "
                  "cat > /dev/null")])

(deftest stdio-bridge-routes-through-child-scope
  (testing "scoped: the bridge's server runs under the prefix"
    (let [b (stdio/->stdio-bridge :bridge/child-scope-test)]
      (try
        (let [r (binding [cs/*probe* (constantly true) cs/*prefix* marking-prefix]
                  (bridge/start-bridge! b {:command fake-mcp-server}))]
          (is (:success? r) (pr-str r))
          (is (= "1" (get-in r [:metadata :server-info :name]))))
        (finally (bridge/stop-bridge! b)))))
  (testing "missing executable: the error names the cause, not the handshake"
    (let [b (stdio/->stdio-bridge :bridge/child-scope-test-missing)
          r (binding [cs/*probe* (constantly true) cs/*prefix* marking-prefix]
              (bridge/start-bridge! b {:command ["/nonexistent/hive-no-such-server"]}))]
      (is (false? (:success? r)))
      (is (not (str/includes? (str (:errors r)) "handshake")) (pr-str r))
      (is (str/includes? (str (:errors r)) "hive-no-such-server") (pr-str r)))))
