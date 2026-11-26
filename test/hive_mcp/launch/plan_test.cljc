(ns hive-mcp.launch.plan-test
  "The planner's three facets: golden, property, mutation.

   GOLDEN. Sixteen env/config/args combos, each stated with the EXACT
   :java-opts and :aliases bin/hive-mcp produces. This table is the parity
   oracle a launcher rewrite must keep green. The XDG base directories and the
   HOME fallback are pinned set and unset.

   PROPERTY. An env value beats the config value for every knob; the --profile
   flag beats the environment; a nil config resolves to exactly the defaults;
   and `plan` is total over generated environments.

   MUTATION. Each mutant must DISAGREE with the real implementation on the
   witness input, which is the statement that some assertion here would go red.
   The check compares two functions; it rebinds no var.

   Runs under `bb test:launch` and under the JVM suite."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.launch.defaults :as defaults]
            [hive-mcp.launch.knobs :as knobs]
            [hive-mcp.launch.plan :as plan]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def home
  "The only home in these tests: a value in the env map, never a literal path
   the planner could have baked in."
  "/home/tester")

(def heap-default (str home "/.local/state/hive-mcp/heapdumps"))

(def lock-default (str home "/.local/share/hive-mcp/restart-7910.lock"))

(defn- opts
  "The five -J flags in bin/hive-mcp's order, with `heapdump` as the path."
  [xmx xms soft heapdump]
  [(str "-J-Xmx" xmx)
   (str "-J-Xms" xms)
   (str "-J-XX:SoftMaxHeapSize=" soft)
   "-J-XX:+CreateCoredumpOnCrash"
   (str "-J-XX:HeapDumpPath=" heapdump)])

(def default-opts
  "What an untouched launch plans."
  (opts "10g" "2g" "8g" heap-default))

;; =============================================================================
;; The golden cases: label -> {input, exact flags, exact alias}
;; =============================================================================

(def golden-cases
  "Every case states :env at least HOME: a plan with no HOME has no path to
   resolve, which is the totality case, not a golden one."
  [{:label :defaults-home-only
    :in {:config nil :env {"HOME" home} :args [] :files {}}
    :java-opts default-opts
    :aliases ":mcp"}

   {:label :xdg-state-set-data-unset
    :in {:config nil :env {"HOME" home "XDG_STATE_HOME" "/xdg/state"}
         :args [] :files {}}
    :java-opts (opts "10g" "2g" "8g" "/xdg/state/hive-mcp/heapdumps")
    :aliases ":mcp"}

   {:label :xdg-data-set-state-unset
    :in {:config nil :env {"HOME" home "XDG_DATA_HOME" "/xdg/data"}
         :args [] :files {}}
    :java-opts default-opts
    :aliases ":mcp"}

   {:label :both-xdg-set
    :in {:config nil
         :env {"HOME" home "XDG_DATA_HOME" "/xdg/data" "XDG_STATE_HOME" "/xdg/state"}
         :args [] :files {}}
    :java-opts (opts "10g" "2g" "8g" "/xdg/state/hive-mcp/heapdumps")
    :aliases ":mcp"}

   {:label :no-xdg-home-fallback
    :in {:config nil :env {"HOME" "/srv/other"} :args [] :files {}}
    :java-opts (opts "10g" "2g" "8g" "/srv/other/.local/state/hive-mcp/heapdumps")
    :aliases ":mcp"}

   {:label :env-xmx
    :in {:config nil :env {"HOME" home "HIVE_MCP_XMX" "24g"} :args [] :files {}}
    :java-opts (opts "24g" "2g" "8g" heap-default)
    :aliases ":mcp"}

   {:label :env-xms-and-soft
    :in {:config nil
         :env {"HOME" home "HIVE_MCP_XMS" "4g" "HIVE_MCP_SOFT_MAX_HEAP" "12g"}
         :args [] :files {}}
    :java-opts (opts "10g" "4g" "12g" heap-default)
    :aliases ":mcp"}

   {:label :config-xmx
    :in {:config {:jvm {:xmx "6g"}} :env {"HOME" home} :args [] :files {}}
    :java-opts (opts "6g" "2g" "8g" heap-default)
    :aliases ":mcp"}

   {:label :env-beats-config
    :in {:config {:jvm {:xmx "6g"}} :env {"HOME" home "HIVE_MCP_XMX" "24g"}
         :args [] :files {}}
    :java-opts (opts "24g" "2g" "8g" heap-default)
    :aliases ":mcp"}

   {:label :heap-dump-dir-override
    :in {:config nil :env {"HOME" home "HIVE_MCP_HEAP_DUMP_DIR" "/var/tmp/dumps"}
         :args [] :files {}}
    :java-opts (opts "10g" "2g" "8g" "/var/tmp/dumps")
    :aliases ":mcp"}

   {:label :fast-boot-fresh
    :in {:config nil :env {"HOME" home "HIVE_FAST_BOOT" "1"} :args []
         :files {:aot-stamp? true :aot-stale? false}}
    :java-opts default-opts
    :aliases ":mcp:fast-boot"}

   {:label :fast-boot-no-stamp
    :in {:config nil :env {"HOME" home "HIVE_FAST_BOOT" "1"} :args []
         :files {:aot-stamp? false :aot-stale? false}}
    :java-opts default-opts
    :aliases ":mcp"}

   {:label :fast-boot-stale
    :in {:config nil :env {"HOME" home "HIVE_FAST_BOOT" "1"} :args []
         :files {:aot-stamp? true :aot-stale? true}}
    :java-opts default-opts
    :aliases ":mcp"}

   {:label :fast-boot-not-requested
    :in {:config nil :env {"HOME" home} :args []
         :files {:aot-stamp? true :aot-stale? false}}
    :java-opts default-opts
    :aliases ":mcp"}

   {:label :junk-xmx-ignored
    :in {:config nil :env {"HOME" home "HIVE_MCP_XMX" "lots"} :args [] :files {}}
    :java-opts default-opts
    :aliases ":mcp"}

   {:label :config-extra-opts
    :in {:config {:jvm {:extra-opts ["-J-Dfoo=1" "-J-Dbar=2"]}}
         :env {"HOME" home} :args [] :files {}}
    :java-opts (conj default-opts "-J-Dfoo=1" "-J-Dbar=2")
    :aliases ":mcp"}

   {:label :profile-from-flag
    :in {:config nil :env {"HOME" home}
         :args ["--profile" "k8s-headless"] :files {}}
    :java-opts default-opts
    :aliases ":mcp"}])

;; ---- Facet 1: golden ------------------------------------------------------

(deftest golden-java-opts-and-aliases
  (testing "each golden combo plans the exact flags and alias"
    (doseq [c golden-cases]
      (let [p (plan/plan (:in c))]
        (is (= (:java-opts c) (:java-opts p)) (str (:label c) " :java-opts"))
        (is (= (:aliases c) (:aliases p)) (str (:label c) " :aliases"))
        (is (= plan/main-class (:main p)) (str (:label c) " :main"))))))

(deftest golden-every-plan-validates
  (testing "no golden case can drift into an invalid LaunchPlan"
    (doseq [c golden-cases]
      (let [p (plan/plan (:in c))]
        (is (m/validate plan/LaunchPlan p)
            (str (:label c) " : " (pr-str (m/explain plan/LaunchPlan p))))
        (is (nil? (:deps-overlay p)) (str (:label c) " leaves the overlay to LC2"))))))

(deftest golden-default-plan
  (testing "the keys the -J order does not show"
    (let [base (plan/plan {:config nil :env {"HOME" home} :args [] :files {}})]
      (is (= {"MALLOC_ARENA_MAX" "4"} (:env-exports base)))
      (is (= {:enabled? true :file lock-default :by nil} (:lock base)))
      (is (= {:score-adj -500 :oomd-preference :avoid} (:oom base)))
      (is (= {:enabled? true :max-restarts 5 :window-s 600} (:supervise base)))
      (is (= {:coredump? false :heap-dump-dir heap-default} (:forensics base)))
      (is (= [] (:passthrough base)))
      (is (= 1 (count (:notices base)))))))

(deftest golden-paths
  (testing "the nREPL port names the lock file, not a flag"
    (let [p (plan/plan {:config nil :env {"HOME" home "HIVE_MCP_NREPL_PORT" "7911"}
                        :args [] :files {}})]
      (is (= (str home "/.local/share/hive-mcp/restart-7911.lock")
             (get-in p [:lock :file])))
      (is (= default-opts (:java-opts p)))))

  (testing "XDG_DATA_HOME moves the lock, XDG_STATE_HOME moves the dumps"
    (let [p (plan/plan {:config nil
                        :env {"HOME" home "XDG_DATA_HOME" "/xdg/data"
                              "XDG_STATE_HOME" "/xdg/state"}
                        :args [] :files {}})]
      (is (= "/xdg/data/hive-mcp/restart-7910.lock" (get-in p [:lock :file])))
      (is (= "/xdg/state/hive-mcp/heapdumps" (get-in p [:forensics :heap-dump-dir])))))

  (testing "no HOME and no XDG hangs the fallback off the root, as bash does"
    ;; bin/hive-mcp expands ${XDG_STATE_HOME:-$HOME/.local/state}; bash writes
    ;; the separator itself, so an empty HOME gives the state base directory at
    ;; the root. This is the legacy script's answer, and the parity oracle now.
    (let [p (plan/plan {:config nil :env {} :args [] :files {}})]
      (is (= "/.local/state/hive-mcp/heapdumps" (get-in p [:forensics :heap-dump-dir])))
      (is (= "/.local/share/hive-mcp/restart-7910.lock" (get-in p [:lock :file])))
      (is (some (fn [s] (str/includes? s "HOME")) (:notices p))
          "and the plan says HOME was missing"))))

(deftest exports-and-passthrough
  (testing "a profile exports HIVE_PROFILE by name, not by keyword"
    (is (= "k8s-minimal"
           (get-in (plan/plan {:config nil
                               :env {"HOME" home "HIVE_PROFILE" "k8s-minimal"}
                               :args [] :files {}})
                   [:env-exports "HIVE_PROFILE"]))))

  (testing "malloc-arena-max is an export, never a -J flag"
    (let [p (plan/plan {:config nil :env {"HOME" home "MALLOC_ARENA_MAX" "8"}
                        :args [] :files {}})]
      (is (= {"MALLOC_ARENA_MAX" "8"} (:env-exports p)))
      (is (= default-opts (:java-opts p)))))

  (testing "the passthrough drops the profile flag and keeps the rest"
    (is (= ["--a" "b"]
           (:passthrough (plan/plan {:config nil :env {"HOME" home}
                                     :args ["--profile" "x" "--a" "b"]
                                     :files {}}))))
    (is (= ["--a"]
           (:passthrough (plan/plan {:config nil :env {"HOME" home}
                                     :args ["--profile=desktop" "--a"]
                                     :files {}}))))))

(deftest notices
  (testing "the script's own profile notices, verbatim"
    (is (= [(str plan/notice-prefix "profile from config.edn [:system :profile], else desktop")]
           (:notices (plan/plan {:config nil :env {"HOME" home} :args [] :files {}}))))
    (is (= [(str plan/notice-prefix "profile=k8s-minimal")]
           (:notices (plan/plan {:config nil :env {"HOME" home
                                                  "HIVE_PROFILE" "k8s-minimal"}
                                 :args [] :files {}})))))

  (testing "fast boot says which of the three ways it declined"
    (let [notes (fn [files]
                  (:notices (plan/plan {:config nil
                                        :env {"HOME" home "HIVE_FAST_BOOT" "1"}
                                        :args [] :files files})))]
      (is (some (fn [n] (str/includes? n "no AOT classes")) (notes {:aot-stamp? false})))
      (is (some (fn [n] (str/includes? n "stale")) (notes {:aot-stamp? true :aot-stale? true})))
      (is (some (fn [n] (str/includes? n "fast boot"))
                (notes {:aot-stamp? true :aot-stale? false})))))

  (testing "an unparsable env value is announced, and the layer below stands"
    (let [p (plan/plan {:config nil :env {"HOME" home "HIVE_MCP_MAX_RESTARTS" "nope"}
                        :args [] :files {}})]
      (is (= 5 (get-in p [:supervise :max-restarts])))
      (is (some (fn [s] (str/includes? s "HIVE_MCP_MAX_RESTARTS")) (:notices p)))))

  (testing "a trailing --profile is announced rather than passed to the JVM"
    (let [p (plan/plan {:config nil :env {"HOME" home} :args ["--profile"] :files {}})]
      (is (= [] (:passthrough p)))
      (is (some (fn [s] (str/includes? s "requires a value")) (:notices p))))))

;; =============================================================================
;; Facet 2: properties
;; =============================================================================

(def sample-env-value
  "A value each parser ACCEPTS, keyed by its KIND: what the environment CAN set.

   :keyword is :omit, a member of the only closed keyword set a knob names
   (schema/OomdPreference), so this sample survives the enum check as well as
   the parser."
  {:int "42" :size "3g" :bool "true" :keyword "omit" :string "/tmp/x"})

(def sample-config-value
  "A value at the same path the environment must displace. It differs from the
   parsed env value for every KIND, so a wrong winner is visible."
  {:int 7 :size "1g" :bool false :keyword :other :string "/server/decides"})

(defn env-vars-for
  "The env map that sets `chosen` knobs, plus a HOME for the paths."
  [chosen]
  (into {"HOME" home}
        (keep (fn [k]
                (let [raw (get sample-env-value (:parse k))]
                  (when raw [(:env k) raw]))))
        chosen))

(defn config-for
  "A config section that sets `chosen` knobs to the losing values."
  [chosen]
  (reduce (fn [acc k] (assoc-in acc (:path k) (get sample-config-value (:parse k))))
          {}
          chosen))

(defn env-wins-every-path?
  "True when, for each chosen knob, the resolved launch holds the parsed ENV
   value rather than the config value."
  [chosen]
  (let [launch (:launch (plan/resolve-launch {:config (config-for chosen)
                                              :env (env-vars-for chosen)
                                              :args []}))]
    (every? (fn [k]
              (let [raw (get sample-env-value (:parse k))]
                (or (nil? raw)
                    (= (knobs/parse-knob (:parse k) raw) (get-in launch (:path k))))))
            chosen)))

(deftest env-beats-config-for-every-knob
  (testing "every knob in the table, one at a time"
    (doseq [k knobs/knobs]
      (is (env-wins-every-path? [k])
          (str (:env k) " must beat the config at " (pr-str (:path k))))))

  (testing "property: over any subset of knobs, the env layer still wins"
    (let [result (tc/quick-check
                  60
                  (prop/for-all
                   [indices (gen/vector (gen/elements (range (count knobs/knobs))) 1 5)]
                   (env-wins-every-path? (map #(nth knobs/knobs %) (distinct indices)))))]
      (is (:pass? result) (pr-str result)))))

(deftest flag-beats-env
  (testing "--profile wins over HIVE_PROFILE, in both flag spellings"
    (doseq [flag-args [["--profile" "k8s-headless"] ["--profile=k8s-headless"]]]
      (is (= :k8s-headless
             (:profile (:launch (plan/resolve-launch
                                 {:config nil
                                  :env {"HIVE_PROFILE" "k8s-minimal"}
                                  :args flag-args}))))
          (pr-str flag-args))
      (is (= "k8s-headless"
             (get-in (plan/plan {:config nil
                                 :env {"HOME" home "HIVE_PROFILE" "k8s-minimal"}
                                 :args flag-args :files {}})
                     [:env-exports "HIVE_PROFILE"])))))

  (testing "and it wins over a configured profile too"
    (is (= :k8s-minimal
           (:profile (:launch (plan/resolve-launch
                               {:config {:profile :desktop}
                                :env {"HIVE_PROFILE" "k8s-headless"}
                                :args ["--profile" "k8s-minimal"]}))))))

  (testing "an unparsable flag value is ignored, so the layer below stands"
    (is (= :k8s-minimal
           (:profile (:launch (plan/resolve-launch
                               {:config nil
                                :env {"HIVE_PROFILE" "k8s-minimal"}
                                :args ["--profile" "not a keyword!"]}))))))

  (testing "the flag is consumed from argv whatever the layer below says"
    (doseq [args [["--profile" "x"] ["--profile=x"] ["--profile"]]]
      (is (not-any? (fn [a] (str/starts-with? a "--profile"))
                    (:passthrough (plan/resolve-launch {:config nil :env {}
                                                        :args args})))
          (pr-str args)))))

(deftest nil-config-is-exactly-the-defaults
  (testing "no config, no env: the resolved launch IS defaults/defaults"
    (is (= defaults/defaults
           (:launch (plan/resolve-launch {:config nil :env {} :args []}))))
    (is (= defaults/defaults
           (:launch (plan/resolve-launch {:config {} :env {} :args []})))
        "an empty section must equal an absent one"))

  (testing "an overlay-only config key changes no flag"
    (is (= (plan/plan {:config nil :env {"HOME" home} :args [] :files {}})
           (plan/plan {:config {:workspace "/somewhere"} :env {"HOME" home}
                       :args [] :files {}})))))

(defn total-plan?
  "A plan of these inputs validates against LaunchPlan; never throws."
  [env args stamp stale]
  (m/validate plan/LaunchPlan
              (plan/plan {:config nil :env env :args args
                          :files {:aot-stamp? stamp :aot-stale? stale}})))

(deftest plan-is-total
  (testing "property: any generated env and args yield a valid plan"
    (let [vars (conj (mapv :env knobs/knobs) "HOME" "XDG_STATE_HOME"
                     "XDG_DATA_HOME" "NOPE")
          result (tc/quick-check
                  80
                  (prop/for-all
                   [env (gen/map (gen/elements vars)
                                 (gen/fmap str/join (gen/vector gen/char 0 8)))
                    args (gen/vector (gen/fmap str/join (gen/vector gen/char 0 6)) 0 4)
                    stamp gen/boolean
                    stale gen/boolean]
                   (total-plan? env args stamp stale)))]
      (is (:pass? result) (pr-str result))))

  (testing "a nil env, nil args and nil files are still a plan"
    (is (total-plan? nil nil nil nil)))

  (testing "a hostile env does not throw"
    (doseq [env [{"HOME" ""}
                 {"HOME" "  "}
                 {"HOME" nil}
                 {"HOME" home "HIVE_MCP_XMX" "nul\u0000byte"}
                 {"HOME" home "HIVE_PROFILE" "a b c"}]]
      (is (total-plan? env [] false false) (pr-str env)))))

;; =============================================================================
;; Facet 3: mutation
;; =============================================================================

(def witness-input
  "The precedence witness: every layer disagrees with the one above it."
  {:config {:jvm {:xmx "6g"}}
   :env {"HOME" home "HIVE_MCP_XMX" "24g" "HIVE_PROFILE" "k8s-minimal"}
   :args ["--profile" "k8s-headless"]})

(defn inverted-resolve
  "A resolve-launch that applies the config OVER the environment: the exact
   inversion of the precedence the planner states."
  [{:keys [config env]}]
  (let [{:keys [launch ignored]} (knobs/read-knobs env {})]
    {:launch (reduce-kv (fn [acc k v]
                          (if (map? v) (update acc k (fnil merge {}) v) (assoc acc k v)))
                        launch (or config {}))
     :ignored ignored :passthrough [] :flag-notices []}))

(defn env-dropped
  "A resolve-launch that honours the config and forgets the environment."
  [{:keys [config]}]
  {:launch (or config defaults/defaults)
   :ignored [] :passthrough [] :flag-notices []})

(defn flag-dropped
  "A resolve-launch that honours config and env and forgets the flag."
  [{:keys [config env]}]
  {:launch (:launch (knobs/read-knobs env (or config defaults/defaults)))
   :ignored [] :passthrough [] :flag-notices []})

(defn plan-dropping-xms
  "A plan whose -J-Xms flag is gone: the drops-one-knob mutation."
  [in]
  (update (plan/plan in)
          :java-opts
          (fn [flags] (into [] (remove (fn [s] (str/starts-with? s "-J-Xms"))) flags))))

(deftest mutations-are-caught
  (testing "reversing the precedence breaks the witness: the env value must win"
    (let [real (:launch (plan/resolve-launch witness-input))
          mutant (:launch (inverted-resolve witness-input))]
      (is (= "24g" (get-in real [:jvm :xmx])) "the real planner prefers the env")
      (is (= "6g" (get-in mutant [:jvm :xmx])) "the mutant prefers the config")
      (is (not= real mutant) "so a witness assertion catches it")))

  (testing "dropping the env layer breaks the witness"
    (let [real (:launch (plan/resolve-launch witness-input))
          mutant (:launch (env-dropped witness-input))]
      (is (not= real mutant))
      (is (not= "24g" (get-in mutant [:jvm :xmx])))))

  (testing "dropping the flag breaks the witness: the flag value must win"
    (let [real (:launch (plan/resolve-launch witness-input))
          mutant (:launch (flag-dropped witness-input))]
      (is (= :k8s-headless (:profile real)))
      (is (= :k8s-minimal (:profile mutant)))
      (is (not= real mutant))))

  (testing "dropping one -J knob breaks the golden table"
    (let [real (plan/plan witness-input)
          mutant (plan-dropping-xms witness-input)]
      (is (= (count (:java-opts real)) (inc (count (:java-opts mutant)))))
      (is (not= (:java-opts real) (:java-opts mutant)))
      (doseq [c golden-cases]
        (is (= (:java-opts c) (:java-opts (plan/plan (:in c))))
            (str (:label c) " :java-opts"))))))
