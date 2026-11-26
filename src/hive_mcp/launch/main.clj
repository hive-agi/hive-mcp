(ns hive-mcp.launch.main
  "The bb launcher: hive-mcp.launch.plan, then the effects it describes.

   Two strata, and the seam between them is one function:

   `argv` is PURE. (plan, overlay) -> the exact argv vector, so the launch a
   machine would perform can be read, compared and tested without performing
   it. Every parity test in test/hive_mcp/launch/main_test.clj rests on this
   function alone.

   `-main` is the boundary. It is the only place that reads the host: the
   environment (SystemEnv through hive-system.env, once), the config file, the
   AOT stamp, and the subprocesses. It resolves the plan from those inputs and
   then performs today's effects in today's order:

     flock restart lock -> oom-policy (adj + scope) -> supervise -> exec

   STDOUT IS NEVER WRITTEN. It is the stdio JSON-RPC channel: the exec'd JVM
   inherits it, and every notice this namespace emits goes to stderr.

   Effects: the QUESTIONS (the `find` that decides AOT staleness, the lock
   probe) go through hive-system's portable IShell, which captures stdio,
   because capturing is what a question needs. Replacing the process image and
   supervising the JVM need INHERITED stdio, which IShell refuses by
   declaration (hive-system.shell.sh/capabilities) -- so those are
   babashka.process, and bin/oom-policy.sh is sourced rather than ported."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [hive-dsl.result :as r]
            [hive-mcp.launch.deps :as deps]
            [hive-mcp.launch.plan :as plan]
            [hive-system.env :as env]
            [hive-system.path :as path]
            [hive-system.protocols :as proto]
            [hive-system.shell.sh :as shell]
            [hive-system.xdg :as xdg]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def notice-prefix
  "Every launcher line starts with this, on stderr."
  plan/notice-prefix)

(def dry-run-flag
  "Print the LaunchPlan EDN to stderr and exit 0. Takes no lock, starts nothing."
  "--dry-run")

(def print-argv-flag
  "Print the exact argv as an EDN vector to stderr and exit 0. No lock, no exec."
  "--print-argv")

(def lock-exit
  "Exit status of a launch refused by the restart lock: EX_TEMPFAIL."
  75)

;; =============================================================================
;; Flags
;; =============================================================================

(defn split-flags
  "Separate the launcher's own flags from the arguments the JVM receives.

   Both flags print to stderr and exit before any effect, so they are consumed
   here and never reach the planner's passthrough (which would otherwise hand
   `--dry-run` to the server as an argument)."
  [args]
  (reduce (fn [acc a]
            (cond
              (= a dry-run-flag) (assoc acc :dry-run? true)
              (= a print-argv-flag) (assoc acc :print-argv? true)
              :else (update acc :args conj a)))
          {:dry-run? false :print-argv? false :args []}
          args))

(m/=> split-flags
      [:=> [:cat [:sequential [:maybe :string]]]
       [:map [:dry-run? :boolean] [:print-argv? :boolean] [:args [:vector [:maybe :string]]]]])

;; =============================================================================
;; EDN spelling: the same bytes bin/hive-mcp prints
;; =============================================================================

(defn chomp
  "S without its trailing newlines: exactly what `$(cat FILE)` yields.

   The deprecated local.deps.edn fallback is spliced into -Sdeps verbatim, and
   bash's command substitution is what spells it, so the launcher trims the
   same characters and nothing else (no leading whitespace, no carriage
   returns)."
  [s]
  (if (string? s)
    (let [n (count s)
          keep (loop [i n]
                 (if (and (pos? i) (= \newline (nth s (dec i))))
                   (recur (dec i))
                   i))]
      (subs s 0 keep))
    s))

(defn edn-string
  "S as an EDN string literal, escaping only what EDN cannot carry raw:
   backslash and double quote.

   Newlines stay literal -- legal inside an EDN string -- because
   bin/hive-mcp-legacy prints them that way with bash parameter expansion. The
   two launchers therefore spell the same vector with the same bytes, so the
   parity test can compare the printed text and not merely the parsed value."
  [s]
  (let [out (StringBuilder.)]
    (.append out \")
    (doseq [c (str s)]
      (cond
        (= c \\) (.append out "\\\\")
        (= c \") (.append out "\\\"")
        :else (.append out c)))
    (.append out \")
    (str out)))

(defn argv-edn
  "ARGV as one EDN vector of strings, on one line."
  [argv]
  (str "[" (str/join " " (map edn-string argv)) "]"))

(m/=> chomp [:=> [:cat :any] :any])
(m/=> edn-string [:=> [:cat :any] :string])
(m/=> argv-edn [:=> [:cat [:sequential :any]] :string])

;; =============================================================================
;; The -Sdeps overlay
;; =============================================================================

(defn overlay-edn
  "The -Sdeps payload for OVERLAY, or nil when there is nothing to overlay.

   A map (what hive-mcp.launch.deps/overlay answers) is serialized; a STRING is
   kept byte for byte, because that is the deprecated local.deps.edn fallback:
   bin/hive-mcp hands the file text itself to -Sdeps, and argv parity means the
   launcher cannot re-spell it."
  [overlay]
  (cond
    (string? overlay)
    (let [body (chomp overlay)]
      (when-not (str/blank? body) body))

    (map? overlay)
    (when (seq overlay)
      (binding [*print-namespace-maps* false] (pr-str overlay)))

    :else nil))

(m/=> overlay-edn [:=> [:cat [:maybe [:or :map :string]]] [:maybe :string]])

(defn argv
  "The exact argv this launcher execs, as data.

   PLAN is a hive-mcp.launch.plan/LaunchPlan and OVERLAY is the -Sdeps payload:
   the map hive-mcp.launch.deps/overlay answers, or the text of local.deps.edn.
   Pure: the same two inputs always answer the same vector, and nothing is
   read, run or written."
  [plan overlay]
  (let [sdeps (overlay-edn overlay)]
    (into (cond-> (into ["clojure"] (:java-opts plan))
            sdeps (into ["-Sdeps" sdeps]))
          (concat [(str "-M" (:aliases plan)) "-m" (:main plan)]
                  (:passthrough plan)))))

(m/=> argv [:=> [:cat :map [:maybe [:or :map :string]]] [:vector :string]])

;; =============================================================================
;; Shell questions: hive-system's portable IShell
;; =============================================================================

(def ^:private default-shell
  "One IShell for the lifetime of the launch. hive-system.shell.sh is the
   portable adapter: it rides the host's clojure.java.shell, which babashka
   ships, and it declares what it cannot do (no deadline, no inherited stdio)
   rather than pretending otherwise."
  (delay (shell/make-shell)))

(defn capture
  "Run CMD and answer {:exit N :stdout S :stderr S}.

   For QUESTIONS -- the `find` that decides AOT staleness, the lock probe --
   where capturing stdio is the point. An effect that could not run at all
   answers the same shape with a nil :exit, so a caller reads one map."
  ([cmd] (capture cmd {}))
  ([cmd opts]
   (let [res (proto/shell-exec! @default-shell (vec cmd) opts)]
     (if (r/ok? res)
       (select-keys (:ok res) [:exit :stdout :stderr])
       {:exit nil :stdout "" :stderr "" :error (:error res)}))))

;; =============================================================================
;; Host inputs: the config, the checkout, the AOT facts
;; =============================================================================

(defn config-path
  "Where the per-person config lives, resolved through hive-system only:

     HIVE_MCP_CONFIG, else XDG_CONFIG_HOME/hive-mcp/config.edn,
     else HOME/.config/hive-mcp/config.edn.

   hive-system.xdg answers both of the last two from one call (the variable,
   then the HOME fallback) and answers nil when the environment names neither.
   A leading `~` in the override is expanded against HOME, because an override
   written by hand says ~ far more often than a resolved path does."
  [src]
  (if-let [override (env/present src "HIVE_MCP_CONFIG")]
    (path/expand-tilde override (xdg/home src))
    (xdg/app-dir src :config "hive-mcp" "config.edn")))

(defn read-config
  "The config map at PATH, as {:config M :notice S-or-nil}.

   Read with clojure.edn and unknown tags answered as tagged literals, so a
   config carrying a tag this image does not know still loads. A missing file,
   a PATH the environment cannot name, a directory, bad EDN and a non-map all
   answer a nil config plus a notice: the launcher falls back to the defaults,
   it never refuses to start."
  [path]
  (cond
    (nil? path)
    {:config nil
     :notice (str notice-prefix
                  "no config.edn: HIVE_MCP_CONFIG is unset and the environment "
                  "names no config directory; using the defaults")}

    (not (fs/exists? path))
    {:config nil
     :notice (str notice-prefix "no config.edn at " path "; using the defaults")}

    :else
    (let [read (r/try-effect* :launch/config-unreadable
                 (edn/read-string {:default tagged-literal} (slurp path)))]
      (cond
        (r/err? read)
        {:config nil
         :notice (str notice-prefix "cannot read " path ": " (:message read)
                      "; using the defaults")}

        ;; A file that reads as something other than a map is not a config, and
        ;; saying so is more useful than silently launching from {}.
        (not (map? (:ok read)))
        {:config nil
         :notice (str notice-prefix path " is not an EDN map; using the defaults")}

        :else {:config (:ok read) :notice nil}))))

(defn project-dir
  "The hive-mcp checkout the launcher belongs to.

   HIVE_MCP_PROJECT_DIR names it explicitly, for a launcher installed outside
   the checkout and for the tests. Otherwise it is the directory holding the
   bb.edn bb was told to use (bin/hive-mcp passes `--config \"$DIR/../bb.edn\"`),
   and failing that the working directory."
  [src props]
  (or (env/present src "HIVE_MCP_PROJECT_DIR")
      (when-let [config (get props "babashka.config")]
        (str (fs/normalize (fs/parent config))))
      (get props "user.dir")
      "."))

(defn resolve-overlay
  "The -Sdeps overlay for LAUNCH-CFG, with local.deps.edn as a DEPRECATED input.

   Answers {:overlay MAP-or-TEXT-or-nil :map MAP-or-nil :notices [S ...]}.

   `:overlay` is what argv splices; `:map` is the same content as a map, for
   --dry-run to show. When `:launch :deps` is present the config wins (and a
   local.deps.edn beside it is named as deprecated); when it is absent and the
   checkout has a local.deps.edn, that file supplies the overlay VERBATIM,
   because that is what bin/hive-mcp does today."
  [launch-cfg project]
  (let [cfg-deps (:deps launch-cfg)
        cfg? (and (map? cfg-deps) (seq cfg-deps))
        local-file (path/join project "local.deps.edn")
        local? (fs/exists? local-file)]
    (cond
      cfg?
      (let [overlay (deps/overlay launch-cfg (deps/load-presets (:presets launch-cfg)))]
        {:overlay overlay
         :map overlay
         :notices (if local?
                    [(str notice-prefix "config :launch :deps wins over "
                          local-file "; local.deps.edn is deprecated and can be deleted")]
                    [])})

      local?
      (let [read (r/try-effect* :launch/local-deps-unreadable (slurp local-file))]
        (if (r/ok? read)
          {:overlay (:ok read)
           :map (let [parsed (r/try-effect* :launch/local-deps-unparseable
                              (edn/read-string {:default tagged-literal} (:ok read)))]
                  (when (r/ok? parsed)
                    (deps/import-local-deps (:ok parsed))))
           :notices [(str notice-prefix "merging local.deps.edn (deprecated: "
                         "move its :deps to :launch in config.edn)")]}
          {:overlay nil
           :map nil
           :notices [(str notice-prefix "cannot read " local-file ": "
                         (:message read) "; launching without an overlay")]}))

      :else {:overlay nil :map nil :notices []})))

(defn aot-facts
  "The two AOT facts hive-mcp.launch.plan reads: is the stamp there, and is any
   source newer than it.

   The staleness question goes to the SAME `find` invocation bin/hive-mcp
   makes -- the checkout's src plus each sibling checkout's src -- so the
   answer cannot drift from the legacy script's. A partly stale directory boots
   a mixed image, so the planner's job is to DECLINE the optimization, and this
   answers only what find says."
  [project]
  (let [stamp (path/join project "target/boot-classes/.aot-stamp.edn")]
    (if-not (fs/exists? stamp)
      {:aot-stamp? false :aot-stale? false}
      (let [res (capture ["bash" "-c"
                          (str "find \"$1/src\" \"$1\"/../*/src -type f "
                               "\\( -name '*.clj' -o -name '*.cljc' \\) "
                               "-newer \"$2\" -print -quit 2>/dev/null")
                          "hive-mcp-aot" project stamp]
                         {})]
        {:aot-stamp? true
         :aot-stale? (not (str/blank? (:stdout res)))}))))

;; =============================================================================
;; The supervising shell: bin/oom-policy.sh, sourced, not ported
;; =============================================================================

(def prelude-script
  "One bash that performs the effect chain inside the exec.

   Everything here must run in the process that BECOMES the JVM, because each
   step is inherited rather than applied: a raised RLIMIT_CORE, an
   oom_score_adj, a systemd user scope. bash is the vehicle, `exec` keeps the
   pid, and bin/oom-policy.sh supplies the policy itself (oom_adj_set,
   oom_scope_prefix, oom_supervise) so none of it is reimplemented here.

   Positional: $1 oom-policy.sh, $2 score-adj, $3 coredump 0|1,
   $4 supervise 0|1, $5 oomd preference, $6 project dir, then the argv."
  (str/join
   "\n"
   ["set -uo pipefail"
    "OOM_POLICY=$1; ADJ=$2; COREDUMP=$3; SUPERVISE=$4; PREF=$5; PROJECT=$6"
    "shift 6"
    "source \"$OOM_POLICY\""
    "if [ \"$COREDUMP\" = 1 ]; then"
    "  ulimit -c \"$(ulimit -H -c)\" 2>/dev/null || true"
    "  echo \"hive-mcp: core dumps enabled (ulimit -c $(ulimit -c))\" >&2"
    "fi"
    "oom_adj_set \"$ADJ\""
    "cd \"$PROJECT\" || exit 1"
    "if [ \"$SUPERVISE\" = 1 ]; then"
    "  set +e"
    "  oom_supervise hive-mcp \"$PREF\" \"$@\""
    "  exit $?"
    "fi"
    "if [ \"$PREF\" != off ]; then oom_scope_prefix hive-mcp \"$PREF\"; fi"
    "exec ${OOM_SCOPE_PREFIX[@]+\"${OOM_SCOPE_PREFIX[@]}\"} \"$@\""]))

(defn prelude-argv
  "The ONE bash argv that runs PRELUDE-SCRIPT over PLAN's effects, then ARGV.

   FLAT, not nested: this is a single process's argv, so the plan's words are
   spliced after the launcher's own rather than appended as a sub-vector."
  [plan project argv]
  (into ["bash" "-c" prelude-script "hive-mcp-launch"
         (str (path/join project "bin/oom-policy.sh"))
         (str (get-in plan [:oom :score-adj]))
         (if (get-in plan [:forensics :coredump?]) "1" "0")
         (if (get-in plan [:supervise :enabled?]) "1" "0")
         (name (get-in plan [:oom :oomd-preference]))
         project]
        argv))

(m/=> prelude-argv [:=> [:cat :map :string [:sequential :string]] [:vector :any]])

;; =============================================================================
;; The effects
;; =============================================================================

(defn- notice
  "One launcher line, on stderr."
  [s]
  (binding [*out* *err*] (println s)))

(defn- parent-description
  "Who launched us when HIVE_MCP_RESTART_BY says nothing: `ppid <n> (<comm>)`."
  []
  (let [parent (.parent (java.lang.ProcessHandle/current))]
    (if (.isPresent parent)
      (let [p (.get parent)
            info (.info p)
            comm (if (.isPresent (.command info)) (.get (.command info)) "?")]
        (str "ppid " (.pid p) " (" comm ")"))
      "?")))

(defn- holder-line
  "`pid=<pid> by=<who> started=<iso>`, the line recorded beside the lock."
  [by]
  (str "pid=" (.pid (java.lang.ProcessHandle/current))
       " by=" by
       ;; OffsetDateTime, not Instant: babashka's analysis resolves the former
       ;; and not the latter, and the ISO-8601 answer is what date -Iseconds
       ;; writes into the same line in bin/hive-mcp.
       " started=" (java.time.OffsetDateTime/now)))

(defn- claim-lock!
  "Take the restart lock, or refuse.

   Answers {:ok? true} once this launcher may start, or {:ok? false :exit 75}
   after naming the holder on stderr.

   hive-mcp.launch.plan resolved the file; the probe asks flock whether anyone
   holds it (the same non-blocking ask bin/hive-mcp makes on fd 9) and, when it
   is free, records who took it so a refused second launch can say so. The lock
   itself is taken by the exec'd `flock -F` in launch!, which keeps the pid and
   drops the lock when the JVM dies. Between the probe and that exec another
   launcher can win; it then starts and this one exits 75 without starting a
   JVM, which is the safe direction for a lock whose job is one server per port."
  [lock]
  (let [file (:file lock)
        by (or (:by lock) (parent-description))]
    (cond
      (not (:enabled? lock))
      {:ok? true}

      (nil? file)
      (do (notice (str notice-prefix "restart lock has no file; launching without it"))
          {:ok? true})

      (nil? (fs/which "flock"))
      (do (notice (str notice-prefix "flock not on PATH: launching without the restart lock"))
          {:ok? true})

      :else
      (let [dir (str (fs/parent file))
            _ (r/try-effect* :launch/lock-dir-unwritable (fs/create-dirs dir))
            probe (capture ["flock" "-n" "-E" (str lock-exit) file "-c" "true"] {})]
        (if (= 0 (:exit probe))
          (let [holder (holder-line by)]
            (r/try-effect* :launch/holder-unwritable
              (spit (str file ".holder") (str holder "\n")))
            (r/try-effect* :launch/restarts-log-unwritable
              (spit (path/join dir "restarts.log") (str holder "\n") :append true))
            (notice (str notice-prefix "restart lock held (" holder ")"))
            {:ok? true})
          (do
            (notice (str notice-prefix "restart refused: another launcher holds " file))
            (when (fs/exists? (str file ".holder"))
              (notice (str notice-prefix "  holder: "
                           (str/trim (slurp (str file ".holder"))))))
            {:ok? false :exit lock-exit}))))))

(defn- prepare-heap-dump-dir!
  "Create the OOM heap dump directory. A dump lands where it is found, so the
   directory existing is the difference between a 10 GB .hprof beside the
   launcher's cwd and one where the next person looks."
  [plan]
  (when-let [dir (get-in plan [:forensics :heap-dump-dir])]
    (r/try-effect* :launch/heap-dump-dir-unwritable (fs/create-dirs dir))
    nil))

(defn- exec-env
  "The variables the JVM must inherit: the plan's exports, plus the supervisor's
   knobs, which bin/oom-policy.sh reads as OOM_MAX_RESTARTS / OOM_RESTART_WINDOW."
  [plan]
  (cond-> (:env-exports plan)
    (get-in plan [:supervise :enabled?])
    (assoc "OOM_MAX_RESTARTS" (str (get-in plan [:supervise :max-restarts]))
           "OOM_RESTART_WINDOW" (str (get-in plan [:supervise :window-s])))))

(defn chain
  "The whole effect chain for PLAN: `flock -F` (when the lock is on and flock is
   present), then the prelude bash, then ARGV.

   Every link execs in place, so the launcher's pid is the JVM's pid, which is
   what an unsupervised caller and a SIGTERM both rely on. Pure."
  [plan argv project]
  (let [inner (prelude-argv plan project argv)
        lock-file (get-in plan [:lock :file])]
    (if (and (get-in plan [:lock :enabled?]) lock-file (fs/which "flock"))
      (into ["flock" "-n" "-E" (str lock-exit) "-F" lock-file] inner)
      inner)))

(m/=> chain [:=> [:cat :map [:sequential :string] :string] [:vector :any]])

(defn launch!
  "Perform the plan: heap-dump dir, restart lock, then exec the chain."
  [plan argv project]
  (prepare-heap-dump-dir! plan)
  (let [claim (claim-lock! (:lock plan))]
    (if (:ok? claim)
      (process/exec {:cmd (chain plan argv project) :extra-env (exec-env plan)})
      (System/exit (:exit claim)))))

;; =============================================================================
;; The boundary
;; =============================================================================

(defn- report
  "Every notice the run accumulated, on stderr, in discovery order: config,
   overlay, then the plan's own."
  [& groups]
  (doseq [n (apply concat groups) :when (string? n)]
    (notice n)))

(defn -main
  "Run the launcher: resolve, then act.

   --dry-run prints the LaunchPlan to stderr; --print-argv prints the exact
   argv; both exit 0 before any effect (no lock, no subprocess). Otherwise the
   plan is performed. STDOUT stays empty in every mode."
  [& args]
  (let [src (env/map-env (env/snapshot))
        props (System/getProperties)
        project (project-dir src props)
        {:keys [config notice]} (read-config (config-path src))
        launch-cfg (:launch config)
        {:keys [overlay map] overlay-notices :notices} (resolve-overlay launch-cfg project)
        flags (split-flags args)
        launch-plan (plan/plan {:config launch-cfg
                                :env (env/snapshot src)
                                :args (:args flags)
                                :files (aot-facts project)})
        launch-plan (assoc launch-plan :deps-overlay map)
        launch-argv (argv launch-plan overlay)]
    (report [notice] overlay-notices (:notices launch-plan))
    (cond
      (:print-argv? flags)
      (do (binding [*out* *err*] (println (argv-edn launch-argv)))
          (System/exit 0))

      (:dry-run? flags)
      (do (binding [*out* *err*] (prn launch-plan))
          (System/exit 0))

      :else
      (launch! launch-plan launch-argv project))))

(m/=> -main [:=> [:cat [:* :string]] :any])
