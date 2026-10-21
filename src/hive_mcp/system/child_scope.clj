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

   Opt-out: HIVE_MCP_CHILD_SCOPE=off makes the default probe answer false.

   Since 2026-10-17 the cached answer EXPIRES (`expiring-probe`,
   `default-ttl-ms`): each spawn makes its own call to the user manager, which
   can go away after the probe said yes. Spawns go through `start!`, which
   (1) scopes only a command that resolves on the child's PATH, so a missing
   executable still throws IOException at start instead of systemd-run
   printing `Failed to find executable` and exiting 1 behind a live pid, and
   (2) watches the start until systemd-run execs the command; on systemd-run's
   own failure (`scope-start-failure?`) it records false on the probe and
   starts the command once more, plain. Coverage limits are in `start!`'s
   doc. `*probe*` / `*prefix*` are the call-time seams for callers and tests."
  (:require [clojure.string :as str]
            [malli.core :as m]
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

(def ^:dynamic *probe*
  "Probe the call-time spawn paths (`child-argv` with one argument, `start!`)
   use instead of `default-probe` when bound. The seam for callers and tests:
   read at call time, so a `binding` reaches spawns made inside it."
  nil)

(def ^:dynamic *prefix*
  "argv prefix `start!` puts in front of a scoped command when bound, instead
   of `scope-prefix`. A test seam: binding it to e.g. [\"env\" \"SCOPED=1\"]
   proves a spawn went through the scoped path without a systemd user manager,
   and a prefix that prints systemd-run's failure line and exits 1 simulates a
   scope that failed to start."
  nil)

(def probe-argv
  "The real command the probe runs; exit 0 means scoped spawns work here."
  (conj scope-prefix "true"))

(def ^:private probe-timeout-ms
  "Longest the probe may take before it counts as a failure."
  10000)

;; ---------------------------------------------------------------------------
;; Pure calculation
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Schemas
;; ---------------------------------------------------------------------------

(def Argv
  "A command line. Any sequential of strings in; argv fns answer a vector."
  [:sequential :string])

(def Probe
  "The probe port: a fn answering whether scoped spawns work. An
   expiring-probe also takes one argument (record an answer)."
  [:fn {:gen/elements [(constantly true) (constantly false)]} ifn?])

(def ProbeAnswer
  "Whatever a probe answered; only exactly true counts as available."
  :any)

(def PlanOpts
  "Where the child runs: its PATH, its cwd, and the prefix to use."
  [:maybe [:map
           [:path {:optional true} [:maybe :string]]
           [:cwd {:optional true} [:maybe :string]]
           [:prefix {:optional true} [:maybe Argv]]]])

(def ExecFile
  "A resolved executable java.io.File."
  [:fn {:gen/elements [(java.io.File. "/bin/sh")]} #(instance? java.io.File %)])

(def ProcBuilder
  "A java.lang.ProcessBuilder already configured with the plain argv."
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (ProcessBuilder. ^java.util.List ["true"]))}
   #(instance? ProcessBuilder %)])

(def Proc
  "A started java.lang.Process."
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (.start (ProcessBuilder. ^java.util.List ["true"])))}
   #(instance? Process %)])

(defn scoped-argv
  "argv to spawn given the probe's answer: `scope-prefix` + argv when
   available? is exactly true, argv unchanged (as a vector) otherwise.
   An empty argv is returned unchanged: there is nothing to scope."
  [available? argv]
  (let [argv (vec argv)]
    (if (and (true? available?) (seq argv))
      (into scope-prefix argv)
      argv)))

(m/=> scoped-argv [:=> [:cat ProbeAnswer [:maybe Argv]] [:vector :string]])

(defn resolve-executable
  "The executable File `argv0` names for a child whose PATH is `path` and whose
   working directory is `cwd` (nil: the JVM's), or nil when it does not resolve.

   A name with a slash is a path (relative to cwd) that must be an executable
   file. A bare name must be an executable file in one of PATH's directories:
   the PATH the CHILD sees (the ProcessBuilder environment), because under
   systemd-run it is systemd-run, with that environment, that looks it up. An
   empty PATH element, or a relative one, is taken against cwd."
  [argv0 path cwd]
  (when-not (str/blank? argv0)
    (let [cwd    (or cwd (System/getProperty "user.dir"))
          exe?   (fn [^java.io.File f] (and (.isFile f) (.canExecute f)))
          dir-of (fn [^String d]
                   (let [f (java.io.File. (if (str/blank? d) "." d))]
                     (if (.isAbsolute f) f (java.io.File. ^String cwd (.getPath f)))))]
      (if (str/includes? argv0 "/")
        (let [f (java.io.File. ^String argv0)
              f (if (.isAbsolute f) f (java.io.File. ^String cwd ^String argv0))]
          (when (exe? f) f))
        (some (fn [d]
                (let [f (java.io.File. ^java.io.File (dir-of d) ^String argv0)]
                  (when (exe? f) f)))
              (str/split (or path "") #":" -1))))))

(m/=> resolve-executable [:=> [:cat [:maybe :string] [:maybe :string] [:maybe :string]] [:maybe ExecFile]])

(defn plan-argv
  "argv to spawn given the probe's answer and where the child will run:
   `prefix` (default `scope-prefix`) + argv when available? is exactly true AND
   argv's first element resolves for the child's PATH and cwd
   (`resolve-executable`), argv unchanged otherwise.

   The resolution check keeps a missing or non-executable command failing the
   way it always did: plain, ProcessBuilder.start throws IOException. Under
   systemd-run the start would succeed, systemd-run would print `Failed to find
   executable X` and exit 1, and the caller would hold a pid for a command that
   never ran."
  [available? argv {:keys [path cwd prefix]}]
  (let [argv (vec argv)]
    (if (and (true? available?) (seq argv) (resolve-executable (first argv) path cwd))
      (into (vec (or prefix scope-prefix)) argv)
      argv)))

(m/=> plan-argv [:=> [:cat ProbeAnswer [:maybe Argv] PlanOpts] [:vector :string]])

(def scope-failure-signatures
  "How systemd-run's OWN failures begin on stderr, the ones where it exits 1
   before it ever execs the command: the user manager refused the transient
   unit, or the user bus could not be reached."
  ["Failed to start transient scope unit"
   "Failed to connect to bus"
   "Failed to connect to user scope bus"
   "Failed to create bus connection"])

(defn scope-start-failure?
  "True when an exited scoped spawn looks like systemd-run failing to set up
   the scope, not the command failing: exit 1, nothing on stdout, and stderr
   no more than two lines that begin with one of `scope-failure-signatures`.
   Pure: the caller hands in what it observed."
  [{:keys [exit stdout-quiet? stderr]}]
  (let [text (str/trim (str stderr))]
    (boolean
     (and (= 1 exit)
          (true? stdout-quiet?)
          (<= 1 (count (str/split-lines text)) 2)
          (some #(str/starts-with? text %) scope-failure-signatures)))))

(m/=> scope-start-failure?
  [:=> [:cat [:map
              [:exit {:optional true} [:maybe :int]]
              [:stdout-quiet? {:optional true} :any]
              [:stderr {:optional true} [:maybe :string]]]]
   :boolean])

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

(m/=> argv-for [:=> [:cat Probe [:maybe Argv]] [:vector :string]])

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

(m/=> systemd-probe [:=> [:cat] :boolean])

(defn caching-probe
  "Wrap a probe so it runs at most once; later calls return the first answer.
   A probe that throws caches false."
  [probe]
  (let [answer (delay (try (true? (probe)) (catch Throwable _ false)))]
    (fn [] @answer)))

(m/=> caching-probe [:=> [:cat Probe] Probe])

(def default-ttl-ms
  "How long `default-probe`'s answer stands before a spawn re-probes."
  (* 5 60 1000))

(defn expiring-probe
  "Wrap a probe so its answer is reused for `ttl-ms`, then probed again.

   Called with no argument it answers. Called with one argument it RECORDS
   that answer as of now (a spawn that watched the scope fail to start records
   false), and the recorded answer stands until the ttl runs out again. A probe
   that throws answers false. opts: :ttl-ms (default `default-ttl-ms`),
   :clock (0-arg fn answering epoch ms, a test seam)."
  ([probe] (expiring-probe probe {}))
  ([probe {:keys [ttl-ms clock] :or {ttl-ms default-ttl-ms}}]
   (let [clock (or clock #(System/currentTimeMillis))
         state (atom nil)]
     (fn
       ([]
        (let [{:keys [answer at]} @state]
          (if (and (some? at) (< (- (long (clock)) (long at)) (long ttl-ms)))
            answer
            (let [a (try (true? (probe)) (catch Throwable _ false))]
              (reset! state {:answer a :at (clock)})
              a))))
       ([answer]
        (reset! state {:answer (true? answer) :at (clock)})
        (true? answer))))))

(m/=> expiring-probe
  [:function
   [:=> [:cat Probe] Probe]
   [:=> [:cat Probe [:maybe [:map
                             [:ttl-ms {:optional true} :int]
                             [:clock {:optional true} [:maybe Probe]]]]]
    Probe]])

(def default-probe
  "Process-wide probe: the real systemd-run check. Its answer stands for
   `default-ttl-ms`, then the next spawn re-probes; a spawn that sees the scope
   fail to start records false on it (see `start!`). It is not cached for the
   JVM's life: the user manager can go away and come back (daemon-reexec,
   user@ restart, a D-Bus timeout under memory pressure)."
  (expiring-probe
   (fn []
     (let [ok? (systemd-probe)]
       (log/info "child-scope probe" {:scoped? ok? :preference oom-preference})
       ok?))))

(defn record-answer!
  "Tell `probe` that scoped spawns do (true) or do not (false) work as of now.
   An `expiring-probe` keeps the answer until its ttl runs out; a probe that
   takes no argument (a plain 0-arg fn) cannot record and is left alone."
  [probe answer]
  (try (probe answer)
       (catch clojure.lang.ArityException _ nil)
       (catch Throwable t
         (log/debug "child-scope probe refused an answer" {:error (ex-message t)})))
  nil)

(m/=> record-answer! [:=> [:cat Probe :any] :nil])

(defn child-argv
  "argv for a child process spawned by hive-mcp: in a sibling
   ManagedOOMPreference=none scope when available, plain otherwise.
   With one argument the probe bound to `*probe*` decides, else the
   process-wide `default-probe`. With opts {:path :cwd} the argv is scoped
   only when its first element resolves for that PATH and cwd (see
   `plan-argv`); without them no resolution check is made."
  ([argv] (child-argv (or *probe* default-probe) argv))
  ([probe argv] (argv-for probe argv))
  ([probe argv opts]
   (let [available? (try (true? (probe)) (catch Throwable _ false))]
     (plan-argv available? argv opts))))

(m/=> child-argv
  [:function
   [:=> [:cat [:maybe Argv]] [:vector :string]]
   [:=> [:cat Probe [:maybe Argv]] [:vector :string]]
   [:=> [:cat Probe [:maybe Argv] PlanOpts] [:vector :string]]])

(def ^:private default-watch-ms
  "Longest `start!` watches a scoped spawn for systemd-run's own failure. The
   wait normally ends within milliseconds, as soon as systemd-run execs the
   command; the bound is the D-Bus call timeout (25 s) plus slack, for a user
   manager that hangs before refusing."
  30000)

(defn- peek-text
  "Up to 4 KiB already buffered on `in`, without consuming it (mark/reset), or
   nil when the stream cannot be peeked."
  [^java.io.InputStream in]
  (when (.markSupported in)
    (let [n (min 4096 (.available in))
          buf (byte-array n)]
      (.mark in 4096)
      (let [r (.readNBytes in buf 0 n)]
        (.reset in)
        (String. buf 0 (int r) java.nio.charset.StandardCharsets/UTF_8)))))

(defn- comm-of
  "The kernel's short name for pid (/proc/<pid>/comm), or nil when unreadable.
   Read through java.nio: slurp's InputStream.available() answers EINVAL on
   procfs."
  [pid]
  (try
    (let [path (java.nio.file.Paths/get (str "/proc/" pid "/comm") (make-array String 0))]
      (some-> (java.nio.file.Files/readAllLines path) first str/trim))
    (catch Throwable _ nil)))

(defn- await-exec-or-exit
  "Watch a just-started scoped process until it has exec'd the command (its
   /proc comm is none of `waiting`: the prefix's own program, the JDK's
   jspawnhelper, the JVM itself) -> :exec, exited -> :exit, or `watch-ms` ran
   out -> :running. Where /proc cannot be read it waits for an exit within a
   short window instead."
  [^Process p waiting watch-ms]
  (let [deadline (+ (System/currentTimeMillis) (long watch-ms))]
    (loop []
      (let [comm (comm-of (.pid p))]
        (cond
          (not (.isAlive p)) :exit
          (nil? comm) (if (.waitFor p 200 TimeUnit/MILLISECONDS) :exit :running)
          (not (contains? waiting comm)) :exec
          (> (System/currentTimeMillis) deadline) :running
          :else (do (Thread/sleep 5) (recur)))))))

(defn- waiting-comms
  "/proc comm values a scoped start shows before the command itself runs:
   the prefix program's basename (comm is cut to 15 chars) and jspawnhelper
   (the JDK's posix_spawn launcher). Any other comm counts as the command
   running, which errs toward NOT retrying: the safe direction."
  [prefix]
  (let [cut  #(when % (subs % 0 (min 15 (count %))))
        base (some-> (first prefix) (java.io.File.) .getName cut)]
    (cond-> #{"jspawnhelper"} base (conj base))))

(defn start!
  "Start `pb` (a ProcessBuilder already configured with the PLAIN argv, its
   environment, directory and redirects) in a sibling OOM scope when that
   works, and return the Process. This is the spawn entry point for headless
   lings and stdio bridges.

   - Probe: opts :probe, else `*probe*`, else `default-probe`.
   - Prefix: opts :prefix, else `*prefix*`, else `scope-prefix`.
   - A command that does not resolve for the child's PATH and directory is
     started plain (`plan-argv`), so a missing executable still throws
     IOException from here exactly as an unscoped spawn did.
   - Recovery: a scoped start is watched until systemd-run execs the command
     (normally milliseconds; bounded by opts :watch-ms). If instead it exits
     with systemd-run's own failure (`scope-start-failure?`: exit 1, no stdout,
     a `Failed to start transient scope unit` / `Failed to connect to bus` line
     on stderr), the probe records false (re-probed after its ttl) and the
     command is started once more, plain. A start that exec'd the command is
     never retried, and neither is one that wrote to stdout.

   Not covered: a scope failure that arrives after the watch bound, a command
   that itself exits 1 within the window, silent on stdout, with exactly
   systemd-run's failure text on stderr (it would run twice), and streams that
   cannot be peeked (stderr redirected to a file: then no retry). pb's command
   is left as the argv that was started."
  ([^ProcessBuilder pb] (start! pb {}))
  ([^ProcessBuilder pb {:keys [probe prefix watch-ms]}]
   (let [probe  (or probe *probe* default-probe)
         prefix (vec (or prefix *prefix* scope-prefix))
         plain  (vec (.command pb))
         argv   (child-argv probe plain {:path   (get (.environment pb) "PATH")
                                         :cwd    (some-> (.directory pb) .getPath)
                                         :prefix prefix})]
     (if (= argv plain)
       (.start pb)
       (let [_ (.command pb ^java.util.List argv)
             p (.start pb)]
         (if-not (= :exit (await-exec-or-exit p (waiting-comms prefix)
                                              (or watch-ms default-watch-ms)))
           p
           (let [merged? (.redirectErrorStream pb)
                 out     (peek-text (.getInputStream p))
                 err     (if merged? out (peek-text (.getErrorStream p)))
                 failed? (scope-start-failure? {:exit          (.exitValue p)
                                                :stdout-quiet? (if merged? true (= "" out))
                                                :stderr        err})]
             (if-not failed?
               p
               (do (log/warn "child-scope: systemd-run could not start the scope, starting plain"
                             {:stderr (some-> err str/trim) :cmd (first plain)})
                   (record-answer! probe false)
                   (.command pb ^java.util.List plain)
                   (.start pb))))))))))

(m/=> start!
  [:function
   [:=> [:cat ProcBuilder] Proc]
   [:=> [:cat ProcBuilder [:maybe [:map
                                   [:probe {:optional true} [:maybe Probe]]
                                   [:prefix {:optional true} [:maybe Argv]]
                                   [:watch-ms {:optional true} [:maybe :int]]]]]
    Proc]])
