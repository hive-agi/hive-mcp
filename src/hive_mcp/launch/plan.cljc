(ns hive-mcp.launch.plan
  "The pure launch planner: (:config, :env, :args, :files) -> LaunchPlan.

   `plan` is total and side-effect free. It reads no environment of its own
   and touches no filesystem: the environment, the argv and the two AOT facts
   are inputs, so a caller can render the exact launch a machine would perform
   without performing it.

   Precedence is uniform for every knob: the --profile flag (profile alone) >
   env, through hive-mcp.launch.knobs, > config > hive-mcp.launch.defaults.
   An env value that does not parse is ignored and reported in :notices; the
   layer underneath stands.

   Paths resolve from env values only (HOME, XDG_STATE_HOME, XDG_DATA_HOME),
   never a literal home directory, so the plan is shareable across hosts.
   The XDG Base Directory specification is hive-system.xdg's, and the :env map
   is read through a hive-system.env MapEnv, so the host environment is never
   touched here: SystemEnv belongs to the -main boundary alone."
  (:require [clojure.string :as str]
            [hive-mcp.launch.defaults :as defaults]
            [hive-mcp.launch.knobs :as knobs]
            [hive-mcp.launch.schema :as schema]
            [hive-system.env :as env]
            [hive-system.path :as path]
            [hive-system.xdg :as xdg]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def main-class
  "The namespace -m runs."
  "hive-mcp.server.core")

(def base-alias
  "The alias every launch carries."
  ":mcp")

(def fast-boot-alias
  "The alias added when AOT classes are present and fresh."
  ":mcp:fast-boot")

(def default-nrepl-port
  "The port the restart-lock filename falls back to."
  7910)

(def notice-prefix
  "The prefix bin/hive-mcp writes before every launcher notice."
  "hive-mcp: ")

;; =============================================================================
;; Paths: descriptors resolved from env, never a literal home
;; =============================================================================

(defn- script-base-dir
  "The base directory of KIND in SRC, or the legacy script's expansion of it.

   Mirrors bin/hive-mcp, which writes ${XDG_STATE_HOME:-$HOME/.local/state}
   (and its XDG_DATA_HOME twin) and lets bash substitute. bash expands a
   missing or empty HOME to the empty string and then writes the separator
   itself, so the answer is the fallback hung off the ROOT: unset HOME gives
   the state base directory at the filesystem root, not a relative path and
   not no path at all. A HOME that is set is kept verbatim, spaces included.
   bin/hive-mcp is the parity oracle until LC3 replaces it, so the planner
   keeps that answer. hive-system.xdg answers nil whenever it cannot name an
   absolute base directory, so the script's expansion applies exactly then.

   Answers [BASE FRAGILE?]: FRAGILE? is true when the answer is the script's
   HOME-less expansion, which the caller reports in the plan's notices."
  [src kind fallback]
  (if-let [base (xdg/base-dir src kind)]
    [base false]
    [(apply path/join
            (str (or (env/env-get src "HOME") "") (path/separator path/*flavor*))
            fallback)
     true]))

(defn- resolve-path
  "Resolve a path default, answering [PATH FRAGILE?].

   Accepts a literal string (a config or env override, used as given) or a
   descriptor from hive-mcp.launch.defaults. A descriptor resolves its base
   through `script-base-dir`; a :port descriptor splices the effective port
   into the name. FRAGILE? is true when the base came from the legacy script's
   empty-HOME expansion rather than from a real HOME or XDG variable."
  [src descriptor port]
  (cond
    (string? descriptor) [descriptor false]
    (not (map? descriptor)) [nil false]
    :else
    (let [[base fragile?] (script-base-dir src (:xdg descriptor) (:fallback descriptor))
          stem (path/join base (:suffix descriptor))
          ported (if (:port descriptor) (str stem port) stem)]
      [(if (:port descriptor)
         (str ported (:suffix-after-port descriptor))
         ported)
       fragile?])))

;; =============================================================================
;; Precedence
;; =============================================================================

(defn- deep-merge
  "Merge `b` over `a`, recursing into nested maps so a config that names one
   JVM knob keeps the defaults' other knobs."
  [a b]
  (if (and (map? a) (map? b))
    (merge-with deep-merge a b)
    b))

(defn- config-layer
  "The defaults with the config section merged over them."
  [config]
  (if (map? config)
    (deep-merge defaults/defaults config)
    defaults/defaults))

(defn- profile-flag-value
  "The value of a `--profile=VALUE` argument, or nil."
  [arg]
  (when (and (string? arg) (str/starts-with? arg "--profile="))
    (subs arg (count "--profile="))))

(defn- split-args
  "Separate the --profile flag from the arguments that go to the JVM.

   Answers {:profile RAW :present? bool :passthrough [..] :notices [..]} where
   RAW is the flag's value, unparsed. A trailing --profile with no value is
   consumed and reported, never left to reach the JVM."
  [args]
  (loop [in (seq args)
         out []
         flag nil
         present? false
         notes []]
    (if-let [a (first in)]
      (cond
        (and (string? a) (= a "--profile"))
        (if-let [v (second in)]
          (recur (nnext in) out v true notes)
          (recur nil out flag true
                 (conj notes (str notice-prefix
                                 "--profile requires a value (desktop, k8s-headless, k8s-minimal)"))))

        (profile-flag-value a)
        (recur (next in) out (profile-flag-value a) true notes)

        (and (string? a) (= a "--profile="))
        (recur (next in) out "" true notes)

        :else
        (recur (next in) (conj out a) flag present? notes))
      {:profile flag :present? present? :passthrough out :notices notes})))

(defn- ignored-notices
  "One notice per knob whose env value did not parse."
  [ignored]
  (into []
        (map (fn [[var-name raw]]
               (str notice-prefix "ignoring " var-name "=" (pr-str raw)
                    " (does not parse)")))
        ignored))

;; =============================================================================
;; Notices
;; =============================================================================

(defn- profile-notice
  "The notice bin/hive-mcp prints after resolving the profile."
  [profile]
  (if (some? profile)
    (str notice-prefix "profile=" (name profile))
    (str notice-prefix "profile from config.edn [:system :profile], else desktop")))

(defn- no-home-notice
  "One notice when a path fell back to the legacy script's empty-HOME
   expansion, else none."
  [fragile?]
  (if fragile?
    [(str notice-prefix "HOME is unset or relative; resolving paths from the working directory, as bin/hive-mcp does")]
    []))

(defn- build-instructions
  "The rebuild hint the script prints when fast boot declines."
  []
  [(str notice-prefix "  build them: clojure -Sdeps \"$(cat local.deps.edn)\" -X:aot-boot")])

(defn- fast-boot-notices
  "The notices the script prints about AOT classes, given the request and the
   two file facts."
  [requested? stamp? stale?]
  (cond
    (not requested?) []
    (not stamp?)
    (into [(str notice-prefix "HIVE_FAST_BOOT set but no AOT classes \u2014 booting from source")]
          (build-instructions))
    stale?
    (into [(str notice-prefix "AOT classes are stale (a source file is newer) \u2014 booting from source")]
          (build-instructions))
    :else
    [(str notice-prefix "fast boot \u2014 AOT classes from target/boot-classes")]))

;; =============================================================================
;; The plan
;; =============================================================================

(def LockPlan
  "The restart lock: one launcher owns a port, holders are named."
  [:map
   [:enabled? :boolean]
   [:file [:maybe :string]]
   [:by [:maybe :string]]])

(def OomPlan
  "OOM standing: the kernel score adjustment and the systemd-oomd preference."
  [:map
   [:score-adj :int]
   [:oomd-preference schema/OomdPreference]])

(def SupervisePlan
  "Restart-on-OOM: how many deaths inside the window before giving up."
  [:map
   [:enabled? :boolean]
   [:max-restarts :int]
   [:window-s :int]])

(def ForensicsPlan
  "Crash forensics: the core-dump switch and where an OOM heap dump lands."
  [:map
   [:coredump? :boolean]
   [:heap-dump-dir [:maybe :string]]])

(def LaunchPlan
  "Everything a launcher needs to exec the server, as data.

   :deps-overlay is the -Sdeps payload; LC2 fills it and the planner leaves it
   nil, so planning never needs the dependency graph."
  [:map
   [:java-opts [:vector :string]]
   [:aliases :string]
   [:main :string]
   [:passthrough [:vector :string]]
   [:env-exports [:map-of :string :string]]
   [:lock LockPlan]
   [:oom OomPlan]
   [:supervise SupervisePlan]
   [:forensics ForensicsPlan]
   [:deps-overlay [:maybe :map]]
   [:notices [:vector :string]]])

(defn- enum-knobs
  "The knob paths whose parsed value must also be a MEMBER of a closed set.

   A keyword parser accepts any word-shaped string, so HIVE_MCP_OOMD_PREFERENCE=
   \"-\" parses to :- and would leave the plan holding a preference that
   bin/oom-policy.sh cannot act on. Membership is checked here, and a value
   outside the set falls back to the default with a notice, exactly as an
   unparsable value does."
  []
  [{:path [:oom :oomd-preference] :schema schema/OomdPreference
    :env "HIVE_MCP_OOMD_PREFERENCE"}])

(defn- enforce-enums
  "Restore the default for any resolved value outside its closed set.

   Answers [launch offenders]; each offender is [VAR VALUE]."
  [launch]
  (reduce (fn [[acc bad] {:keys [path schema env]}]
            (let [v (get-in acc path)]
              (if (or (nil? v) (m/validate schema v))
                [acc bad]
                [(assoc-in acc path (get-in defaults/defaults path))
                 (conj bad [env v])])))
          [launch []]
          (enum-knobs)))

(defn resolve-launch
  "The effective `:launch` map: defaults, config, env, then the --profile flag.

   Answers {:launch MAP :ignored [[VAR RAW] ...] :passthrough [ARG ...]
   :flag-notices [S ...]}. The layers are applied in one fixed order, so a
   caller that wants to ask \"what did this knob resolve to, and why\" asks
   here rather than re-deriving the precedence.

   The flag rides on the same knob as HIVE_PROFILE ([:profile], parser
   :keyword), so \"the flag beats env\" is the order of two assoc-in calls and
   not a special case."
  [{:keys [config env args]}]
  (let [split (split-args args)
        {:keys [launch ignored]} (knobs/read-knobs env (config-layer config))
        flag-profile (when (:present? split)
                       (knobs/parse-knob :keyword (:profile split)))
        ;; A flag value that does not parse is IGNORED, never applied as nil:
        ;; assoc-ing nil here would silently discard an HIVE_PROFILE the person
        ;; did set, turning a typo into a different launch.
        launch (if (some? flag-profile)
                 (assoc launch :profile flag-profile)
                 launch)
        [launch offenders] (enforce-enums launch)]
    {:launch launch
     :ignored (into (vec ignored) offenders)
     :passthrough (:passthrough split)
     :flag-notices (cond-> (:notices split)
                     (and (:present? split) (nil? flag-profile))
                     (conj (str notice-prefix "ignoring --profile="
                                (pr-str (:profile split)) " (does not parse)")))}))

(defn plan
  "Plan a launch.

   :config is the config.edn :launch section or nil, :env is the environment
   as a string map, :args is argv after the program name, and :files carries
   {:aot-stamp? :aot-stale?}. Answers a LaunchPlan. Total: no input throws."
  [{:keys [config env args files]}]
  (let [{:keys [launch ignored passthrough flag-notices]}
        (resolve-launch {:config config :env env :args args})
        ;; The planner's :env map, as a pure MapEnv: hive-system.env never
        ;; consults the host here. SystemEnv is the LC3 -main boundary's.
        src (env/map-env env)
        profile (:profile launch)
        port (or (:nrepl-port launch) default-nrepl-port)
        jvm (:jvm launch)
        requested? (true? (:fast-boot? launch))
        stamp? (true? (:aot-stamp? files))
        stale? (true? (:aot-stale? files))
        fast? (and requested? stamp? (not stale?))
        [heap-dump-dir heap-fragile?]
        (resolve-path src (get-in launch [:forensics :heap-dump-dir]) port)
        [lock-file lock-fragile?]
        (resolve-path src (get-in launch [:restart-lock :file]) port)]
    {:java-opts (into [(str "-J-Xmx" (:xmx jvm))
                       (str "-J-Xms" (:xms jvm))
                       (str "-J-XX:SoftMaxHeapSize=" (:soft-max-heap jvm))
                       "-J-XX:+CreateCoredumpOnCrash"
                       (str "-J-XX:HeapDumpPath=" heap-dump-dir)]
                      (or (:extra-opts jvm) []))
     :aliases (if fast? fast-boot-alias base-alias)
     :main main-class
     :passthrough passthrough
     :env-exports (cond-> {"MALLOC_ARENA_MAX" (str (:malloc-arena-max jvm))}
                    (some? profile) (assoc "HIVE_PROFILE" (name profile)))
     :lock {:enabled? (true? (get-in launch [:restart-lock :enabled?]))
            :file lock-file
            :by (get-in launch [:restart-lock :by])}
     :oom {:score-adj (get-in launch [:oom :score-adj])
           :oomd-preference (get-in launch [:oom :oomd-preference])}
     :supervise {:enabled? (true? (get-in launch [:supervise :enabled?]))
                 :max-restarts (get-in launch [:supervise :max-restarts])
                 :window-s (get-in launch [:supervise :window-s])}
     :forensics {:coredump? (true? (get-in launch [:forensics :coredump?]))
                 :heap-dump-dir heap-dump-dir}
     :deps-overlay nil
     :notices (into (into (into (into [(profile-notice profile)]
                                     (fast-boot-notices requested? stamp? stale?))
                               flag-notices)
                           (no-home-notice (or heap-fragile? lock-fragile?)))
                    (ignored-notices ignored))}))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> resolve-launch
      [:=> [:cat [:map]]
       [:map
        [:launch :map]
        [:ignored [:vector [:tuple :string :any]]]
        [:passthrough [:vector :string]]
        [:flag-notices [:vector :string]]]])

(m/=> plan [:=> [:cat :map] LaunchPlan])

(comment
  (plan {:config nil
         :env {"HOME" "/home/someone"}
         :args []
         :files {:aot-stamp? false :aot-stale? false}})
  ;=> {:java-opts ["-J-Xmx10g" "-J-Xms2g" "-J-XX:SoftMaxHeapSize=8g"
  ;                "-J-XX:+CreateCoredumpOnCrash"
  ;                "-J-XX:HeapDumpPath=/home/someone/.local/state/hive-mcp/heapdumps"]
  ;    :aliases ":mcp"
  ;    :main "hive-mcp.server.core"
  ;    :passthrough []
  ;    :env-exports {"MALLOC_ARENA_MAX" "4"}
  ;    :lock {:enabled? true :file "/home/someone/.local/share/hive-mcp/restart-7910.lock" :by nil}
  ;    ...}
  )
