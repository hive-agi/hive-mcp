(ns hive-mcp.launch.schema-test
  "The `:launch` schema and its defaults, held together.

   Three things are asserted and each one is a gate in its own right:
     1. `defaults` validates against `LaunchConfig` — the data LC1 falls back
        to cannot drift out of the shape LC1 reads.
     2. The defaults EQUAL what bin/hive-mcp does today, value by value. LC0 is
        a data change only; if this test disagrees with the script, one of them
        is wrong and this fails loudly rather than at the first restart.
     3. No default carries a home directory. Path defaults are descriptors so
        the launcher stays shareable; a literal /home/whoever would leak one
        person's box into everyone's.

   Runs under `bb test:launch` and under the JVM suite."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.launch.defaults :as defaults]
            [hive-mcp.launch.schema :as schema]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest defaults-validate-against-the-schema
  (testing "the defaults map is a valid :launch section"
    (is (m/validate schema/LaunchConfig defaults/defaults)
        (str "defaults must validate; explanation: "
             (pr-str (schema/explain defaults/defaults))))
    (is (true? (defaults/validate-defaults))))

  (testing "an EMPTY section is valid too: every key is optional"
    (is (m/validate schema/LaunchConfig {})
        "a person who configures nothing must not be rejected"))

  (testing "a partial section is valid"
    (is (m/validate schema/LaunchConfig {:jvm {:xmx "24g"}}))
    (is (m/validate schema/LaunchConfig {:supervise {:max-restarts 9}}))))

(deftest defaults-equal-todays-script
  (testing "the heap flags bin/hive-mcp writes"
    (is (= "10g" (get-in defaults/defaults [:jvm :xmx])) "HIVE_MCP_XMX 10g")
    (is (= "2g" (get-in defaults/defaults [:jvm :xms])) "HIVE_MCP_XMS 2g")
    (is (= "8g" (get-in defaults/defaults [:jvm :soft-max-heap]))
        "HIVE_MCP_SOFT_MAX_HEAP 8g")
    (is (= 4 (get-in defaults/defaults [:jvm :malloc-arena-max]))
        "MALLOC_ARENA_MAX 4"))

  (testing "profile and fast boot are OFF by default: config.edn decides"
    (is (nil? (:profile defaults/defaults)))
    (is (false? (get-in defaults/defaults [:fast-boot?]))))

  (testing "crash forensics: no core dumps unless asked"
    (is (false? (get-in defaults/defaults [:forensics :coredump?]))))

  (testing "OOM standing: adj -500 and an oomd scope asking to be spared"
    (is (= -500 (get-in defaults/defaults [:oom :score-adj])))
    (is (= :avoid (get-in defaults/defaults [:oom :oomd-preference]))))

  (testing "restart lock is ON"
    (is (true? (get-in defaults/defaults [:restart-lock :enabled?]))))

  (testing "supervise: on, 5 restarts inside a 600 s window"
    (is (true? (get-in defaults/defaults [:supervise :enabled?])))
    (is (= 5 (get-in defaults/defaults [:supervise :max-restarts])))
    (is (= 600 (get-in defaults/defaults [:supervise :window-s])))))

(deftest path-defaults-are-descriptors-not-literals
  (testing "the heap dump dir names an XDG base and a suffix, not a path"
    (let [d (get-in defaults/defaults [:forensics :heap-dump-dir])]
      (is (map? d) "a descriptor map, so LC1 resolves it from the environment")
      (is (= :state (:xdg d)))
      (is (= "hive-mcp/heapdumps" (:suffix d)))
      (is (= [".local" "state"] (:fallback d)))))

  (testing "the restart lock file names an XDG base and a port slot"
    (let [d (get-in defaults/defaults [:restart-lock :file])]
      (is (map? d))
      (is (= :data (:xdg d)))
      (is (true? (:port d)))
      (is (= "hive-mcp/restart-" (:suffix d)))
      (is (= ".lock" (:suffix-after-port d)))))

  (testing "no default anywhere holds a home directory"
    (let [flat (str (pr-str defaults/defaults))]
      (is (not (str/includes? flat "/home/"))
          (str "a default carries a literal home path: " flat))
      (is (not (str/includes? flat "~/"))
          (str "a default carries a tilde path: " flat))
      (is (not (str/includes? flat "/Users/"))
          (str "a default carries a macOS home path: " flat)))))

(deftest schema-rejects-what-the-script-could-not-use
  (testing "a heap size must be a size STRING with a unit or a byte count"
    (is (m/validate schema/LaunchConfig {:jvm {:xmx "10g"}}))
    (is (m/validate schema/LaunchConfig {:jvm {:xmx "1073741824"}}))
    (is (not (m/validate schema/LaunchConfig {:jvm {:xmx 10}}))
        "the script pastes a STRING into -J-Xmx; a number is not that")
    (is (not (m/validate schema/LaunchConfig {:jvm {:xmx "10x"}})))
    (is (not (m/validate schema/LaunchConfig {:jvm {:xmx ""}}))))

  (testing "the oomd preference is the script's own vocabulary"
    (doseq [p [:avoid :omit :none :off]]
      (is (m/validate schema/LaunchConfig {:oom {:oomd-preference p}})))
    (is (not (m/validate schema/LaunchConfig {:oom {:oomd-preference :kill}}))))

  (testing ":deps is deps.edn shaped, so a local.deps.edn entry is already valid"
    (is (m/validate schema/LaunchConfig
                    {:deps {'io.github.hive-agi/hive-carto
                            {:local/root "../hive-carto"}}}))
    (is (not (m/validate schema/LaunchConfig {:deps ["not" "a" "map"]}))))

  (testing "unknown keys pass: config.edn carries :addons, :memory and more"
    (is (m/validate schema/LaunchConfig {:jvm {:xmx "10g"}
                                         :addons {"hive.guard" {:mode :enforce}}}))))

(deftest path-walking-finds-keys-and-only-real-ones
  (testing "every declared key is reachable at its own path"
    (doseq [k [:jvm :profile :deps :workspace :presets :extra-paths :aliases
               :fast-boot? :forensics :oom :restart-lock :supervise :nrepl-port]]
      (is (schema/path-present? schema/LaunchConfig [k])
          (str "missing top-level key " k))))

  (testing "nested paths resolve, and a wrong one does not"
    (is (schema/path-present? schema/LaunchConfig [:jvm :xmx]))
    (is (schema/path-present? schema/LaunchConfig [:jvm :soft-max-heap]))
    (is (schema/path-present? schema/LaunchConfig [:forensics :heap-dump-dir]))
    (is (schema/path-present? schema/LaunchConfig [:oom :oomd-preference]))
    (is (schema/path-present? schema/LaunchConfig [:restart-lock :by]))
    (is (schema/path-present? schema/LaunchConfig [:supervise :window-s]))
    (is (not (schema/path-present? schema/LaunchConfig [:jvm :nope])))
    (is (not (schema/path-present? schema/LaunchConfig [:nope :xmx])))
    (is (not (schema/path-present? schema/LaunchConfig [:jvm :xmx :deeper])))
    (is (schema/path-present? schema/LaunchConfig [])
        "the empty path is the section itself")))

(deftest every-launch-key-is-optional
  (testing "an absent `:launch` key means the default, so all keys are optional"
    (doseq [k [:jvm :profile :deps :workspace :presets :extra-paths :aliases
               :fast-boot? :forensics :oom :restart-lock :supervise :nrepl-port]]
      (is (schema/optional? schema/LaunchConfig k)
          (str k " must be optional; a person configures only what differs")))))

(deftest defaults-are-empty-not-surprising
  (testing "the collection defaults start empty rather than pre-filled"
    (is (= {} (:deps defaults/defaults)))
    (is (= [] (:presets defaults/defaults)))
    (is (= [] (:extra-paths defaults/defaults)))
    (is (= {} (:aliases defaults/defaults)))
    (is (= [] (get-in defaults/defaults [:jvm :extra-opts]))))

  (testing "the launcher has no opinion on the nREPL port; the server does"
    (is (nil? (:nrepl-port defaults/defaults)))))