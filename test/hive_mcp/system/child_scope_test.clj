(ns hive-mcp.system.child-scope-test
  "Child-scope argv policy: pure argv building, probe injection, caching,
   and one real spawn proving the child gets its own cgroup."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.system.child-scope :as cs])
  (:import [java.util.concurrent TimeUnit]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private gen-argv
  (gen/vector (gen/not-empty gen/string-alphanumeric) 0 5))

(deftrifecta scoped-argv-policy
  hive-mcp.system.child-scope/scoped-argv
  {:golden-path "test/golden/system/child-scope-argv.edn"
   :apply? true
   :cases {:available [true ["claude" "-p" "hi"]]
           :unavailable [false ["claude" "-p" "hi"]]
           :nil-answer [nil ["bash" "-c" "ls"]]
           :truthy-not-true [:yes ["bash" "-c" "ls"]]
           :empty-argv [true []]}
   :gen (gen/tuple (gen/one-of [gen/boolean (gen/return nil)]) gen-argv)
   :pred (fn [argv] (and (vector? argv) (every? string? argv)))
   :num-tests 100
   :mutations [["always scope, even when unavailable"
                (fn [_ argv] (into cs/scope-prefix argv))]
               ["never scope"
                (fn [_ argv] (vec argv))]
               ["prefix after the command"
                (fn [ok? argv] (if (true? ok?) (into (vec argv) cs/scope-prefix) (vec argv)))]]
   :assert (fn []
             (is (= ["systemd-run" "--user" "--scope" "--collect" "--quiet"
                     "-p" "ManagedOOMPreference=none" "sleep" "5"]
                    (cs/scoped-argv true ["sleep" "5"])))
             (is (= ["sleep" "5"] (cs/scoped-argv false '("sleep" "5")))))})

(deftest argv-for-applies-the-injected-probe
  (testing "probe true: scoped"
    (is (= (into cs/scope-prefix ["x"]) (cs/argv-for (constantly true) ["x"]))))
  (testing "probe false: plain"
    (is (= ["x"] (cs/argv-for (constantly false) ["x"]))))
  (testing "probe throws: plain, never a failure"
    (is (= ["x"] (cs/argv-for #(throw (ex-info "no bus" {})) ["x"]))))
  (testing "child-argv with an explicit probe"
    (is (= ["x"] (cs/child-argv (constantly false) ["x"])))))

(deftest caching-probe-runs-once
  (let [calls (atom 0)
        probe (cs/caching-probe #(do (swap! calls inc) true))]
    (is (true? (probe)))
    (is (true? (probe)))
    (is (= 1 @calls)))
  (let [calls (atom 0)
        probe (cs/caching-probe #(do (swap! calls inc) (throw (ex-info "boom" {}))))]
    (is (false? (probe)))
    (is (false? (probe)))
    (is (= 1 @calls))))

;; ---------------------------------------------------------------------------
;; Real spawn: only where a systemd user manager answers the probe.
;; ---------------------------------------------------------------------------

(defn- proc-lines
  "Lines of /proc/<pid>/<leaf>, read through java.nio: slurp's
   InputStream.available() answers EINVAL on procfs."
  [pid leaf]
  (let [path (java.nio.file.Paths/get (str "/proc/" pid "/" leaf) (make-array String 0))]
    (when (java.nio.file.Files/exists path (make-array java.nio.file.LinkOption 0))
      (vec (java.nio.file.Files/readAllLines path)))))

(defn- cgroup-of [pid]
  (some #(when (str/starts-with? % "0::") (subs % 3)) (proc-lines pid "cgroup")))

(deftest real-spawn-lands-in-a-sibling-scope
  (if-not (cs/systemd-probe)
    (is true "no systemd user manager here: plain spawn path, nothing to measure")
    (let [self (cgroup-of (.pid (java.lang.ProcessHandle/current)))]
      (testing "pid is the command itself, cgroup differs, kill reaches it"
        (let [p (.start (ProcessBuilder. ^java.util.List (cs/child-argv cs/systemd-probe ["sleep" "30"])))
              pid (.pid p)]
          (try
            (Thread/sleep 300)
            (is (= "sleep" (some-> (proc-lines pid "comm") first str/trim))
                "systemd-run --scope execs in place: the held pid runs sleep")
            (is (not= self (cgroup-of pid)))
            (is (str/ends-with? (str (cgroup-of pid)) ".scope"))
            (.destroy p)
            (is (.waitFor p 5 TimeUnit/SECONDS) "destroy reaches the real child")
            (finally (.destroyForcibly p)))))
      (testing "stdin, stdout and exit code pass through"
        (let [p (.start (ProcessBuilder. ^java.util.List
                                         (cs/child-argv cs/systemd-probe ["sh" "-c" "read l; echo got:$l; exit 7"])))]
          (with-open [w (java.io.OutputStreamWriter. (.getOutputStream p))]
            (.write w "ping\n"))
          (is (= "got:ping" (str/trim (slurp (.getInputStream p)))))
          (is (.waitFor p 10 TimeUnit/SECONDS))
          (is (= 7 (.exitValue p))))))))
