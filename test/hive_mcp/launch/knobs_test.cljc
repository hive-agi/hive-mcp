(ns hive-mcp.launch.knobs-test
  "The knob table must be COMPLETE and its parsers TOTAL.

   Three gates, matching the card:
     1. every knob's :path exists in LaunchConfig, so no knob can name a key
        the planner will never find;
     2. every knob has a parser, and every registered parser is reachable;
     3. parse-knob is total over junk — a property where test.check is
        available, plus a fixed table of 20 hostile inputs that runs
        everywhere (test.check's clojure-test integration is broken under
        babashka, so the property drives quick-check directly rather than
        through defspec).

   Plus the value-level behaviour each parser promises.

   Runs under `bb test:launch` and under the JVM suite."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.launch.knobs :as knobs]
            [hive-mcp.launch.schema :as schema]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def required-env-vars
  "Every environment variable bin/hive-mcp (or the oom-policy.sh it sources)
   reads today. The knob table must cover all of them, or a person's existing
   shell setup silently stops working when LC3 flips the launcher."
  ["HIVE_PROFILE"
   "HIVE_MCP_XMX"
   "HIVE_MCP_XMS"
   "HIVE_MCP_SOFT_MAX_HEAP"
   "MALLOC_ARENA_MAX"
   "HIVE_MCP_SUPERVISE"
   "HIVE_MCP_MAX_RESTARTS"
   "HIVE_MCP_RESTART_WINDOW"
   "HIVE_MCP_OOM_SCORE_ADJ"
   "HIVE_MCP_OOMD_PREFERENCE"
   "HIVE_MCP_COREDUMP"
   "HIVE_MCP_HEAP_DUMP_DIR"
   "HIVE_MCP_RESTART_LOCK"
   "HIVE_MCP_RESTART_LOCK_FILE"
   "HIVE_MCP_RESTART_BY"
   "HIVE_FAST_BOOT"
   "HIVE_MCP_NREPL_PORT"])

(def junk-inputs
  "Twenty inputs no parser may throw on: the empty string, whitespace, signs
   without digits, a float, an exponent, a 40-digit number, a size with no
   digits, a size with an unknown unit, a bare colon, quotes, a comma list, a
   shell fragment, a NUL byte, an accent, and a very long string."
  [""
   " "
   "\t\n"
   "-"
   "+"
   "--5"
   "1.5"
   "1e9"
   "0x10"
   "12abc"
   "99999999999999999999999999999999999999999"
   "g"
   "10x"
   "lots"
   ":"
   "\"quoted\""
   "a,b,c"
   "$(rm -rf /)"
   "nul\u0000byte"
   "über"
   (str/join (repeat 4096 "9"))])

(defn- value-or-nil-for-kind?
  "True when `v` is an acceptable answer for parser KIND: nil, or a value of
   the kind's own type. This is the whole promise of a total parser."
  [kind v]
  (or (nil? v)
      (case kind
        :int (int? v)
        :size (string? v)
        :bool (boolean? v)
        :keyword (keyword? v)
        :string (string? v)
        false)))

;; =============================================================================
;; Gate 1: every knob names a path the schema declares
;; =============================================================================

(deftest every-knob-path-exists-in-the-schema
  (testing "no knob can address a :launch key the planner will not find"
    (doseq [{:keys [env path]} knobs/knobs]
      (is (vector? path) (str env " must carry a vector :path"))
      (is (seq path) (str env " must carry a non-empty :path"))
      (is (every? keyword? path) (str env " :path must be keywords: " (pr-str path)))
      (is (schema/path-present? schema/LaunchConfig path)
          (str env " names " (pr-str path)
               ", which is not a key of LaunchConfig"))
      (is (some? (knobs/knob-for env)) (str env " must be findable by name")))))

(deftest every-required-env-var-has-a-knob
  (testing "the table covers every variable bin/hive-mcp reads today"
    (doseq [v required-env-vars]
      (is (some? (knobs/knob-for v))
          (str v " is read by bin/hive-mcp but missing from the knob table"))))

  (testing "and the table names no variable the script does not read"
    (doseq [{:keys [env]} knobs/knobs]
      (is (some #{env} required-env-vars)
          (str env " is in the knob table but is not a script knob"))))

  (testing "no knob is listed twice"
    (is (= (count knobs/knobs) (count (set (map :env knobs/knobs))))
        "a duplicated variable would apply twice, last writer winning")))

;; =============================================================================
;; Gate 2: every knob has a parser, and every parser is used
;; =============================================================================

(deftest every-knob-has-a-parser
  (testing "each knob's :parse names a registered parser function"
    (doseq [{:keys [env parse]} knobs/knobs]
      (is (keyword? parse) (str env " :parse must be a keyword KIND"))
      (is (contains? knobs/parsers parse)
          (str env " names parser " (pr-str parse) " which is not registered"))
      (is (fn? (get knobs/parsers parse)) (str env " parser must be callable")))))

(deftest no-registered-parser-is-dead
  (testing "every registered parser is reachable from the table"
    (let [used (set (map :parse knobs/knobs))]
      (doseq [k (keys knobs/parsers)]
        (is (contains? used k)
            (str "parser " k " is registered but no knob uses it"))))))

;; =============================================================================
;; Gate 3: totality
;; =============================================================================

(deftest parse-knob-never-throws-on-the-junk-table
  (testing "20 hostile inputs, every parser, no exception and a sane answer"
    (doseq [[kind _f] knobs/parsers
            s junk-inputs]
      (let [v (knobs/parse-knob kind s)]
        (is (value-or-nil-for-kind? kind v)
            (str "parser " kind " answered " (pr-str v) " for " (pr-str s))))))

  (testing "an unknown KIND answers nil rather than throwing"
    (is (nil? (knobs/parse-knob :nonesuch "whatever")))
    (is (nil? (knobs/parse-knob nil "whatever")))
    (is (nil? (knobs/parse-knob {:parse :nonesuch} "whatever")))))

(deftest parse-knob-is-total-over-arbitrary-strings
  (testing "property: for any string every parser answers a value of its kind
            or nil, and never throws"
    (let [result
          (tc/quick-check
           200
           (prop/for-all [s (gen/fmap str/join (gen/vector gen/char 0 12))]
             (every? (fn [[kind _f]]
                       (value-or-nil-for-kind? kind (knobs/parse-knob kind s)))
                     knobs/parsers)))]
      (is (:pass? result) (pr-str result))))

  (testing "property: a decimal integer round-trips through :int"
    (let [result
          (tc/quick-check
           100
           (prop/for-all [n gen/large-integer]
             (= n (knobs/parse-knob :int (str n)))))]
      (is (:pass? result) (pr-str result))))

  (testing "property: a generated size string round-trips through :size,
            spelled exactly as generated"
    (let [result
          (tc/quick-check
           100
           (prop/for-all [n (gen/large-integer* {:min 1})
                          unit (gen/elements ["k" "m" "g" "t"])]
             (= (str n unit) (knobs/parse-knob :size (str n unit)))))]
      (is (:pass? result) (pr-str result)))))

;; =============================================================================
;; The parsers' value-level promises
;; =============================================================================

(deftest int-parser
  (testing "signs, zero, and the script's own negative"
    (is (= -500 (knobs/parse-knob :int "-500")))
    (is (= 5 (knobs/parse-knob :int "5")))
    (is (= 5 (knobs/parse-knob :int "+5")))
    (is (= 0 (knobs/parse-knob :int "0")))
    (is (= 600 (knobs/parse-knob :int " 600 ")) "whitespace is tolerated"))

  (testing "junk answers nil"
    (doseq [s ["" " " "1.5" "1e9" "abc" "5x" "--1" "-" "+"]]
      (is (nil? (knobs/parse-knob :int s)) (pr-str s)))))

(deftest size-parser
  (testing "the script's three sizes, and the units the JVM accepts"
    (is (= "10g" (knobs/parse-knob :size "10g")))
    (is (= "2g" (knobs/parse-knob :size "2g")))
    (is (= "8g" (knobs/parse-knob :size "8g")))
    (is (= "512m" (knobs/parse-knob :size "512m")))
    (is (= "2048k" (knobs/parse-knob :size "2048k")))
    (is (= "4t" (knobs/parse-knob :size "4t")))
    (is (= "1073741824" (knobs/parse-knob :size "1073741824"))
        "a bare byte count is what -Xmx accepts too"))

  (testing "spelling is preserved, never converted to bytes"
    (is (= "10g" (knobs/parse-knob :size "10G")))
    (is (= "10g" (knobs/parse-knob :size "  10g  "))))

  (testing "junk answers nil: a size with no digits, or an unknown unit"
    (doseq [s ["g" "10x" "lots" "" "-10g" "10.5g"]]
      (is (nil? (knobs/parse-knob :size s)) (pr-str s)))))

(deftest bool-parser
  (testing "both vocabularies, case-insensitively"
    (doseq [s ["1" "true" "TRUE" "on" "yes" "y" " True "]]
      (is (true? (knobs/parse-knob :bool s)) (pr-str s)))
    (doseq [s ["0" "false" "FALSE" "off" "no" "n"]]
      (is (false? (knobs/parse-knob :bool s)) (pr-str s))))

  (testing "junk answers nil, which means keep the default"
    (doseq [s ["" " " "2" "maybe" "on!"]]
      (is (nil? (knobs/parse-knob :bool s)) (pr-str s)))))

(deftest keyword-parser
  (testing "the oomd vocabulary, with and without the colon"
    (doseq [[s want] [[":avoid" :avoid] ["avoid" :avoid] ["AVOID" :avoid]
                      [":omit" :omit] ["none" :none] ["off" :off]]]
      (is (= want (knobs/parse-knob :keyword s)) (pr-str s)))
    (is (= :k8s-headless (knobs/parse-knob :keyword "k8s-headless"))
        "a profile name keeps its dash and digits"))

  (testing "junk answers nil"
    (doseq [s ["" " " ":" "a b" "a,b" "\"x\""]]
      (is (nil? (knobs/parse-knob :keyword s)) (pr-str s)))))

(deftest string-parser
  (testing "a path comes back trimmed"
    (is (= "/var/tmp/dumps" (knobs/parse-knob :string "  /var/tmp/dumps ")))
    (is (= "ppid 42" (knobs/parse-knob :string "ppid 42"))))

  (testing "blank answers nil: an empty variable is unset, not an empty path"
    (doseq [s ["" " " "\t"]]
      (is (nil? (knobs/parse-knob :string s)) (pr-str s)))))

;; =============================================================================
;; read-knobs: the merged view a launcher uses
;; =============================================================================

(deftest read-knobs-applies-only-what-parses
  (testing "a good value lands at its path"
    (is (= {:launch {:jvm {:xmx "24g"}} :ignored []}
           (knobs/read-knobs {"HIVE_MCP_XMX" "24g"} {})))
    (is (= {:launch {:supervise {:enabled? false}} :ignored []}
           (knobs/read-knobs {"HIVE_MCP_SUPERVISE" "off"} {}))))

  (testing "a junk value is reported, and the base value stands"
    (let [{:keys [launch ignored]}
          (knobs/read-knobs {"HIVE_MCP_MAX_RESTARTS" "nope"}
                            {:supervise {:max-restarts 5}})]
      (is (= 5 (get-in launch [:supervise :max-restarts])))
      (is (= [["HIVE_MCP_MAX_RESTARTS" "nope"]] ignored))))

  (testing "the report sits BESIDE the launch map, never inside it"
    (let [{:keys [launch]} (knobs/read-knobs {"HIVE_MCP_MAX_RESTARTS" "nope"}
                                             {})]
      (is (not (contains? launch :ignored))
          "a stray :ignored key would be serialized into a :launch section")
      (is (m/validate schema/LaunchConfig launch)
          "and the result is still a valid :launch section")))

  (testing "an absent variable changes nothing"
    (is (= {:launch {:jvm {:xmx "10g"}} :ignored []}
           (knobs/read-knobs {} {:jvm {:xmx "10g"}}))))

  (testing "several knobs at once keep their own paths"
    (let [{:keys [launch ignored]}
          (knobs/read-knobs {"HIVE_MCP_XMX" "24g"
                             "HIVE_PROFILE" "k8s-headless"
                             "HIVE_MCP_OOMD_PREFERENCE" "omit"
                             "HIVE_MCP_NREPL_PORT" "7911"}
                            {})]
      (is (= [] ignored))
      (is (= "24g" (get-in launch [:jvm :xmx])))
      (is (= :k8s-headless (:profile launch)))
      (is (= :omit (get-in launch [:oom :oomd-preference])))
      (is (= 7911 (:nrepl-port launch))))))