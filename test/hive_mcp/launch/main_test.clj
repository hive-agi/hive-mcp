(ns hive-mcp.launch.main-test
  "argv parity between the bb launcher and bin/hive-mcp-legacy, plus the pure
   `argv` function and the stdout promise.

   Every case runs the two launchers as subprocesses with an EXPLICIT,
   minimal environment map (babashka.process :env REPLACES the environment, it
   does not merge), pointing HOME and the three XDG directories at a temp dir
   and the restart lock at a temp file. Nothing here reads the real config,
   the real local.deps.edn, the real HOME or the real ~/.config.

   No test starts the server: both launchers are asked to print and exit.
   HIVE_MCP_PRINT_ARGV=1 for the legacy script, --print-argv and --dry-run for
   the new one -- the only two modes that take no lock and exec nothing."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.launch.main :as main]
            [hive-system.env :as env]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def repo-root
  "The checkout the tests run from: the directory holding the bb.edn babashka
   was told to use, else the working directory."
  (or (some-> (System/getProperty "babashka.config") fs/parent str fs/normalize)
      (System/getProperty "user.dir")
      "."))

(def launcher-bin
  "The shim every user runs."
  (str (fs/path repo-root "bin/hive-mcp")))

(def legacy-bin
  "The pre-LC3 script, unchanged but for its argv printer."
  (str (fs/path repo-root "bin/hive-mcp-legacy")))

(def oom-policy
  "bin/oom-policy.sh, which the legacy script sources. The new launcher sources
   it from its own checkout at effect time, not at print time."
  (str (fs/path repo-root "bin/oom-policy.sh")))

(def inherited-path
  "The host's PATH, carried into each fixture so `flock`, `bash` and `bb` are
   findable. Everything else in the child's environment is the fixture's."
  (or (System/getenv "PATH") "/usr/bin:/bin"))

;; =============================================================================
;; Fixture scaffolding
;; =============================================================================

(defn- temp-dir!
  "A fresh temp directory under the system temp dir."
  [prefix]
  (str (fs/create-temp-dir {:prefix prefix})))

(defn- project!
  "A throwaway checkout: bin/ with both launchers and the OOM policy, and the
   src/ tree the fast-boot staleness probe walks. Answers its path."
  [root]
  (let [p (str (fs/path root "proj"))]
    (fs/create-dirs (fs/path p "bin"))
    (fs/create-dirs (fs/path p "src/hive_mcp"))
    (fs/copy legacy-bin (fs/path p "bin/hive-mcp-legacy"))
    (fs/copy oom-policy (fs/path p "bin/oom-policy.sh"))
    (spit (str (fs/path p "src/hive_mcp/a.clj")) "(ns hive-mcp.a)\n")
    p))

(defn- env-for
  "The child environment: PATH, the temp HOME and XDG dirs, the temp restart
   lock, the project dir, and whatever the case adds."
  [root project extra]
  (merge {"PATH" inherited-path
          "HOME" (str (fs/path root "home"))
          "XDG_CONFIG_HOME" (str (fs/path root "xdg/config"))
          "XDG_DATA_HOME" (str (fs/path root "xdg/data"))
          "XDG_STATE_HOME" (str (fs/path root "xdg/state"))
          "XDG_CACHE_HOME" (str (fs/path root "xdg/cache"))
          "TMPDIR" (System/getProperty "java.io.tmpdir")
          "HIVE_MCP_PROJECT_DIR" project
          "HIVE_MCP_RESTART_LOCK_FILE" (str (fs/path root "restart.lock"))}
         extra))

(defn- run-capture
  "Run ARGV with ENV as its whole environment; answers {:exit :stdout :stderr}.
   STDIN is closed, so a launcher that ever waited for input would end."
  [argv env]
  ;; apply, not a direct second positional: babashka.process/sh takes
  ;; ([opts? & args]), so a vector handed straight over arrives as ONE argument
  ;; and gets stringified into "[bb --config ...]" -- a program name that
  ;; cannot exist.
  (let [res (apply process/sh {:continue true
                               :out :string
                               :err :string
                               :env env
                               :in ""}
                   (vec argv))]
    ;; babashka.process calls the captured streams :out and :err; this test
    ;; vocabulary is stdout/stderr, so the two are renamed here once.
    {:exit (:exit res)
     :stdout (str (:out res))
     :stderr (str (:err res))}))

(def new-argv-command
  "The bb launcher, invoked the way the shim invokes it: the checkout's bb.edn
   supplies the classpath (src, resources, malli, hive-system, babashka)."
  ["bb" "--config" (str (fs/path repo-root "bb.edn")) "-m" "hive-mcp.launch.main"])

(defn- print-argv-new
  "The new launcher's --print-argv result."
  [env]
  (run-capture (conj new-argv-command "--print-argv") env))

(defn- print-argv-legacy
  "The legacy script's HIVE_MCP_PRINT_ARGV=1 result."
  [env project]
  (run-capture [(str (fs/path project "bin/hive-mcp-legacy"))]
               (assoc env "HIVE_MCP_PRINT_ARGV" "1")))

(defn- argv-text
  "The argv vector TEXT of a launcher's stderr: from the last line that opens
   the vector to the end.

   Not merely the last line: an -Sdeps payload spliced verbatim from
   local.deps.edn carries literal newlines inside its EDN string, so the
   printed vector legitimately spans several lines. Everything before it is a
   notice, and no notice opens with a bracket."
  [stderr]
  (let [lines (str/split-lines (str stderr))
        start (last (keep-indexed (fn [i l] (when (str/starts-with? l "[") i)) lines))]
    (when start
      (str/join "\n" (drop start lines)))))

(defn- parity
  "Ask both launchers for argv under ENV and answer
   {:new TEXT :legacy TEXT :same? bool :new-argv V :legacy-argv V :same-vector? bool}."
  [root project extra]
  (let [env (env-for root project extra)
        new (print-argv-new env)
        leg (print-argv-legacy env project)
        nt (argv-text (:stderr new))
        lt (argv-text (:stderr leg))]
    {:new nt
     :legacy lt
     :same? (= nt lt)
     :new-argv (edn/read-string nt)
     :legacy-argv (edn/read-string lt)
     :same-vector? (= (edn/read-string nt) (edn/read-string lt))
     :new-exit (:exit new)
     :legacy-exit (:exit leg)
     :new-stdout (:stdout new)}))

(defn- has-arg?
  "True when ARGV holds WORD.

   A helper rather than an inline some-with-a-set inside `is`: clojure.test
   routes that shape through assert-predicate, which rebuilds it as
   (apply some ...) and turns a plain membership test into an exception. A
   boolean keeps the assertion reading as it looks."
  [argv word]
  (boolean (some #{word} argv)))

(defn- assert-parity
  "Both launchers must print the same bytes, exit 0, and leave stdout empty."
  [label result]
  (is (= 0 (:new-exit result)) (str label ": new launcher exit"))
  (is (= 0 (:legacy-exit result)) (str label ": legacy launcher exit"))
  (is (= "" (:new-stdout result)) (str label ": new launcher stdout must be empty"))
  (is (:same-vector? result)
      (str label ": argv differs\n  new:    " (:new result)
           "\n  legacy: " (:legacy result)))
  (is (:same? result)
      (str label ": printed argv differs byte for byte\n  new:    " (:new result)
           "\n  legacy: " (:legacy result))))

;; =============================================================================
;; The six parity fixtures
;; =============================================================================

(deftest parity-no-config
  (testing "no config.edn anywhere: both launchers use bin/hive-mcp's defaults"
    (let [root (temp-dir! "lc3-none")
          project (project! root)]
      (assert-parity "no config" (parity root project {})))))

(deftest parity-env-overrides
  (testing "HIVE_MCP_XMX and HIVE_PROFILE: every knob still works through env"
    (let [root (temp-dir! "lc3-env")
          project (project! root)
          result (parity root project {"HIVE_MCP_XMX" "24g"
                                       "HIVE_MCP_XMS" "3g"
                                       "HIVE_MCP_SOFT_MAX_HEAP" "20g"
                                       "MALLOC_ARENA_MAX" "8"
                                       "HIVE_MCP_SUPERVISE" "0"
                                       "HIVE_PROFILE" "k8s-minimal"})]
      (assert-parity "env overrides" result)
      (testing "and the env values reached argv"
        (is (some #{"-J-Xmx24g"} (:new-argv result)))
        (is (some #{"-J-Xms3g"} (:new-argv result)))
        (is (some #{"-J-XX:SoftMaxHeapSize=20g"} (:new-argv result)))))))

(deftest parity-fast-boot-fresh
  (testing "HIVE_FAST_BOOT=1 with a stamp newer than every source: :mcp:fast-boot"
    (let [root (temp-dir! "lc3-fresh")
          project (project! root)
          stamp (str (fs/path project "target/boot-classes/.aot-stamp.edn"))]
      (fs/create-dirs (fs/parent stamp))
      (spit stamp "{:classes 3}\n")
      (let [result (parity root project {"HIVE_FAST_BOOT" "1"})]
        (assert-parity "fast boot fresh" result)
        (testing "and both took the fast-boot alias"
          (is (some #{"-M:mcp:fast-boot"} (:new-argv result))))))))

(deftest parity-fast-boot-stale
  (testing "HIVE_FAST_BOOT=1 with a source newer than the stamp: declines"
    (let [root (temp-dir! "lc3-stale")
          project (project! root)
          stamp (str (fs/path project "target/boot-classes/.aot-stamp.edn"))]
      (fs/create-dirs (fs/parent stamp))
      (spit stamp "{:classes 3}\n")
      ;; Make the stamp look old, then the source newer than it. Both launchers
      ;; ask the same `find ... -newer` question, so ordering the two mtimes is
      ;; enough; no clock arithmetic is relied on.
      (fs/set-last-modified-time stamp (java.time.Instant/ofEpochMilli 1000000000000))
      (spit (str (fs/path project "src/hive_mcp/a.clj")) "(ns hive-mcp.a)\n;; edited\n")
      (let [result (parity root project {"HIVE_FAST_BOOT" "1"})]
        (assert-parity "fast boot stale" result)
        (testing "and both booted from source"
          (is (some #{"-M:mcp"} (:new-argv result)))
          (is (not-any? #{"-M:mcp:fast-boot"} (:new-argv result))))))))

(deftest parity-local-deps-only
  (testing "no :launch :deps, a local.deps.edn present: the deprecated fallback"
    (let [root (temp-dir! "lc3-local")
          project (project! root)
          ;; A small temp file, never the real local.deps.edn. It spans two
          ;; lines on purpose: the payload is spliced VERBATIM into -Sdeps, and
          ;; a newline inside an EDN string is legal -- so this case proves the
          ;; launchers spell the same bytes, not merely the same vector.
          text (str "{:deps {hive-carto/hive-carto {:local/root \"../hive-carto\"}\n"
                    "        hive-dsl/hive-dsl {:local/root \"../hive-dsl\"}}}\n")]
      (spit (str (fs/path project "local.deps.edn")) text)
      (let [result (parity root project {})]
        (assert-parity "local.deps.edn only" result)
        (testing "and the file's text is the -Sdeps payload, chomped"
          (let [flag? (has-arg? (:new-argv result) "-Sdeps")
                payload? (has-arg? (:new-argv result) (main/chomp text))]
            (is flag? "-Sdeps is present")
            (is payload?
                "the -Sdeps payload is the file text without its trailing newline")))))))

(deftest parity-config-launch-only
  (testing "a config.edn carrying only :launch restating the defaults, no :deps"
    ;; The legacy script reads NO config.edn: it has no :launch layer. So this
    ;; fixture is parity WHERE IT MUST HOLD -- a :launch section that restates
    ;; today's defaults. A :launch that CHANGES a value is the feature, and it
    ;; is covered by the pure-argv golden cases below, not by parity.
    (let [root (temp-dir! "lc3-config")
          project (project! root)
          config (str (fs/path root "config.edn"))]
      (spit config
            (str "{:launch {:jvm {:xmx \"10g\" :xms \"2g\" :soft-max-heap \"8g\""
                 " :malloc-arena-max 4}\n"
                 "         :profile :desktop\n"
                 "         :fast-boot? false\n"
                 "         :oom {:score-adj -500 :oomd-preference :avoid}\n"
                 "         :restart-lock {:enabled? true}\n"
                 "         :supervise {:enabled? true :max-restarts 5 :window-s 600}}}"))
      (let [result (parity root project {"HIVE_MCP_CONFIG" config})]
        (assert-parity "config :launch only" result)
        (testing "and the profile reached the environment exports, not argv"
          (is (not-any? #(str/includes? % "desktop") (:new-argv result))))))))

;; =============================================================================
;; stdout is the transport: it must stay empty
;; =============================================================================

(deftest stdout-stays-empty
  (testing "--print-argv and --dry-run write stderr only, through the shim"
    (let [root (temp-dir! "lc3-stdout")
          project (project! root)
          env (env-for root project {})]
      (doseq [flag ["--print-argv" "--dry-run"]]
        (let [via-shim (run-capture [launcher-bin flag] env)
              via-bb (run-capture (conj new-argv-command flag) env)]
          (is (= 0 (:exit via-shim)) (str flag " through the shim: exit"))
          (is (= "" (:stdout via-shim)) (str flag " through the shim: stdout must be empty"))
          (is (not (str/blank? (:stderr via-shim))) (str flag " through the shim: stderr carries the answer"))
          (is (= 0 (:exit via-bb)) (str flag " through bb: exit"))
          (is (= "" (:stdout via-bb)) (str flag " through bb: stdout must be empty")))))))

(deftest dry-run-prints-the-plan
  (testing "--dry-run answers the LaunchPlan as EDN on stderr"
    (let [root (temp-dir! "lc3-dry")
          project (project! root)
          env (env-for root project {})
          res (run-capture [launcher-bin "--dry-run"] env)
          plan (edn/read-string (last (remove str/blank? (str/split-lines (:stderr res)))))]
      (is (= 0 (:exit res)))
      (is (= "" (:stdout res)))
      (is (map? plan))
      (is (= "hive-mcp.server.core" (:main plan)))
      (is (= [":mcp"] [(:aliases plan)]))
      (is (map? (:lock plan)))
      (is (map? (:supervise plan))))))

;; =============================================================================
;; The pure argv fn: golden cases
;; =============================================================================

(def base-plan
  "A minimal LaunchPlan, shaped as hive-mcp.launch.plan/plan answers."
  {:java-opts ["-J-Xmx10g" "-J-Xms2g"]
   :aliases ":mcp"
   :main "hive-mcp.server.core"
   :passthrough []
   :env-exports {"MALLOC_ARENA_MAX" "4"}})

(deftest argv-golden
  (testing "no overlay: no -Sdeps at all, exactly as bin/hive-mcp with no local.deps.edn"
    (is (= ["clojure" "-J-Xmx10g" "-J-Xms2g" "-M:mcp" "-m" "hive-mcp.server.core"]
           (main/argv base-plan nil)))
    (is (= ["clojure" "-J-Xmx10g" "-J-Xms2g" "-M:mcp" "-m" "hive-mcp.server.core"]
           (main/argv base-plan {}))))

  (testing "an overlay map: -Sdeps before -M, serialized without namespace maps"
    (is (= ["clojure" "-J-Xmx10g" "-J-Xms2g"
            "-Sdeps" "{:deps {a/b {:local/root \"../b\"}}}"
            "-M:mcp" "-m" "hive-mcp.server.core"]
           (main/argv base-plan {:deps {'a/b {:local/root "../b"}}}))))

  (testing "an overlay string: spliced verbatim, trailing newlines chomped"
    (is (= ["clojure" "-J-Xmx10g" "-J-Xms2g" "-Sdeps" "{:deps {}}"
            "-M:mcp" "-m" "hive-mcp.server.core"]
           (main/argv base-plan "{:deps {}}\n\n")))
    (is (= ["clojure" "-J-Xmx10g" "-J-Xms2g" "-M:mcp" "-m" "hive-mcp.server.core"]
           (main/argv base-plan "\n \n"))))

  (testing "passthrough and the fast-boot alias reach the tail"
    (is (= ["clojure" "-J-Xmx10g" "-J-Xms2g" "-M:mcp:fast-boot" "-m" "hive-mcp.server.core"
            "--profile" "x"]
           (main/argv (assoc base-plan :aliases ":mcp:fast-boot" :passthrough ["--profile" "x"])
                      nil))))

  (testing "edn-string escapes only backslash and quote, keeping newlines literal"
    (is (= "\"a b\"" (main/edn-string "a b")))
    (is (= "\"a\\\"b\"" (main/edn-string "a\"b")))
    (is (= "\"a\\\\b\"" (main/edn-string "a\\b")))
    (is (= "\"a\nb\"" (main/edn-string "a\nb")))
    (is (= "\"[\\\"x\\\"]\"" (main/edn-string "[\"x\"]")))))

(deftest flags-are-consumed-before-passthrough
  (testing "--print-argv and --dry-run never reach the server's argument list"
    (is (= {:dry-run? true :print-argv? false :args []}
           (main/split-flags ["--dry-run"])))
    (is (= {:dry-run? false :print-argv? true :args []}
           (main/split-flags ["--print-argv"])))
    (is (= {:dry-run? true :print-argv? true :args ["--verbose"]}
           (main/split-flags ["--dry-run" "--verbose" "--print-argv"])))))

;; =============================================================================
;; The config boundary: never a crash, always a notice
;; =============================================================================

(deftest read-config-is-total
  (testing "a missing path, a missing file, bad EDN and a non-map all answer nil plus a notice"
    (let [root (temp-dir! "lc3-cfg")
          bad (str (fs/path root "bad.edn"))
          non-map (str (fs/path root "list.edn"))
          unknown-tagged (str (fs/path root "tagged.edn"))]
      (spit bad "{:launch {:jvm")
      (spit non-map "[1 2 3]")
      ;; A tag this image does not know, INSIDE a map: the config still loads,
      ;; because :default tagged-literal answers the tag instead of throwing.
      (spit unknown-tagged "{:launch {:presets [:starter] :note #app/thing {:a 1}}}")
      (doseq [path [nil (str (fs/path root "absent.edn")) bad non-map]]
        (let [{:keys [config notice]} (main/read-config path)]
          (is (nil? config) (str "nil config for " path))
          (is (string? notice) (str "a notice for " path))))
      (testing "and an unknown reader tag loads as a tagged literal instead of throwing"
        (let [{:keys [config notice]} (main/read-config unknown-tagged)]
          (is (map? config))
          (is (nil? notice)))))))

(deftest config-path-follows-the-precedence
  (testing "HIVE_MCP_CONFIG, else XDG_CONFIG_HOME, else HOME/.config"
    (let [root (temp-dir! "lc3-path")
          home (str (fs/path root "h"))
          xdg (str (fs/path root "x"))]
      (is (= (str (fs/path xdg "hive-mcp/config.edn"))
             (main/config-path (env/map-env {"HOME" home "XDG_CONFIG_HOME" xdg}))))
      (is (= (str (fs/path home ".config/hive-mcp/config.edn"))
             (main/config-path (env/map-env {"HOME" home}))))
      (is (= (str (fs/path root "mine.edn"))
             (main/config-path (env/map-env {"HOME" home
                                          "HIVE_MCP_CONFIG" (str (fs/path root "mine.edn"))})))))))
