(ns hive-mcp.system.child-scope
  "Where hive-mcp's child processes land in the cgroup tree.

   bin/hive-mcp runs the server JVM in a systemd user scope with
   ManagedOOMPreference=avoid (bin/oom-policy.sh). Every process the JVM
   forks inherits that cgroup: headless lings, stdio MCP bridges, and the
   builds and test JVMs they start. A runaway build is then shielded from
   systemd-oomd, and when oomd finally acts it kills the whole scope, the
   server with it.

   This namespace is the one policy for that: a spawn's argv is prefixed with
   `systemd-run --user --scope --collect --quiet -p ManagedOOMPreference=none`
   so the child gets a sibling scope oomd may pick on its own. systemd-run
   --scope execs the command in place, so the pid the caller holds IS the
   command (Process.destroy / destroyForcibly still reach it), and stdin,
   stdout, stderr, cwd, environment and exit code pass through unchanged.

   Probe semantics mirror bin/oom-policy.sh: one real
   `systemd-run --user --scope --collect --quiet -p ManagedOOMPreference=none true`
   per JVM, cached. Any failure (no systemd-run, no user manager, timeout,
   exception) means a plain spawn. The policy never fails a spawn.

   Layers: `scoped-argv` is a pure calculation over a probe answer;
   `argv-for` applies a probe (an injected 0-arg fn, the port) to it;
   `systemd-probe` is the default adapter, `caching-probe` the once-per-JVM
   wrapper and `default-probe` the process-wide instance.

   Opt-out: HIVE_MCP_CHILD_SCOPE=off makes the default probe answer false."
  (:require [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.util.concurrent TimeUnit]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def oom-preference
  "ManagedOOMPreference a child scope carries: none, so oomd judges it alone."
  "none")

(def scope-prefix
  "argv prefix that runs a command in its own transient user scope."
  ["systemd-run" "--user" "--scope" "--collect" "--quiet"
   "-p" (str "ManagedOOMPreference=" oom-preference)])

(def probe-argv
  "The real command the probe runs; exit 0 means scoped spawns work here."
  (conj scope-prefix "true"))

(def ^:private probe-timeout-ms
  "Longest the probe may take before it counts as a failure."
  10000)

;; ---------------------------------------------------------------------------
;; Pure calculation
;; ---------------------------------------------------------------------------

(defn scoped-argv
  "argv to spawn given the probe's answer: `scope-prefix` + argv when
   available? is exactly true, argv unchanged (as a vector) otherwise.
   An empty argv is returned unchanged: there is nothing to scope."
  [available? argv]
  (let [argv (vec argv)]
    (if (and (true? available?) (seq argv))
      (into scope-prefix argv)
      argv)))

;; ---------------------------------------------------------------------------
;; Port application
;; ---------------------------------------------------------------------------

(defn argv-for
  "Apply `probe` (0-arg fn answering whether scoped spawns work) to argv.
   A probe that throws or answers anything but true yields the plain argv."
  [probe argv]
  (let [available? (try
                     (probe)
                     (catch Throwable t
                       (log/debug "child-scope probe threw, plain spawn" {:error (ex-message t)})
                       false))]
    (scoped-argv available? argv)))

;; ---------------------------------------------------------------------------
;; Adapters
;; ---------------------------------------------------------------------------

(defn systemd-probe
  "Run `probe-argv` once and answer true when it exits 0 within the timeout.
   Any exception, timeout or non-zero exit answers false. Honours the
   HIVE_MCP_CHILD_SCOPE=off opt-out."
  []
  (if (= "off" (some-> (System/getenv "HIVE_MCP_CHILD_SCOPE") str/trim str/lower-case))
    false
    (try
      (let [p (-> (ProcessBuilder. ^java.util.List probe-argv)
                  (.redirectErrorStream true)
                  (.redirectOutput java.lang.ProcessBuilder$Redirect/DISCARD)
                  (.start))]
        (.close (.getOutputStream p))
        (if (.waitFor p probe-timeout-ms TimeUnit/MILLISECONDS)
          (zero? (.exitValue p))
          (do (.destroyForcibly p) false)))
      (catch Throwable _ false))))

(defn caching-probe
  "Wrap a probe so it runs at most once; later calls return the first answer.
   A probe that throws caches false."
  [probe]
  (let [answer (delay (try (true? (probe)) (catch Throwable _ false)))]
    (fn [] @answer)))

(def default-probe
  "Process-wide probe: the real systemd-run check, run once per JVM."
  (caching-probe
   (fn []
     (let [ok? (systemd-probe)]
       (log/info "child-scope probe" {:scoped? ok? :preference oom-preference})
       ok?))))

(defn child-argv
  "argv for a child process spawned by hive-mcp: in a sibling
   ManagedOOMPreference=none scope when available, plain otherwise.
   With one argument the process-wide `default-probe` decides."
  ([argv] (child-argv default-probe argv))
  ([probe argv] (argv-for probe argv)))
