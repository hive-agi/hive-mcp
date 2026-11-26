(ns hive-mcp.launch.knobs
  "Every environment variable bin/hive-mcp reads, as data: the knob table.

   A knob is {:env VAR :path [:key ...] :parse KIND}, with :path rooted at the
   :launch section. parse-knob is total and answers nil on junk. :size keeps
   the caller's spelling: the value goes straight into -J-Xmx<value>."
  (:require [clojure.string :as str]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Character predicates — clojure.string only, no regex literals
;; =============================================================================

(def ^:private digit-ring
  "The ASCII digit ring: str/index-of turns a character into its numeric
   value, so no character literal and no regex is needed."
  "0123456789")

(def ^:private letter-ring
  "The lower-case ASCII letters, for alpha-char?."
  "abcdefghijklmnopqrstuvwxyz")

(defn digit-value
  "The numeric value of the ASCII digit `c`, or nil when `c` is not one."
  [c]
  (str/index-of digit-ring (str c)))

(defn- digits-only?
  "True when `s` is one or more ASCII digits."
  [s]
  (and (string? s)
       (pos? (count s))
       (every? digit-value s)))

(defn- alpha-char?
  "True when `c` folds to an ASCII letter."
  [c]
  (str/includes? letter-ring (str/lower-case (str c))))

;; =============================================================================
;; Parsers — total, no exceptions, nil on junk
;; =============================================================================

(defn- parse-int*
  "A base-10 integer with an optional leading sign, or nil.

   Hand-parsed rather than through parse-long: parse-long's answer on junk
   (12abc, 1.5, a 30-digit number) is the JVM Long reader's behaviour, not
   something a launcher should inherit."
  [s]
  (when-let [s (some-> s str/trim not-empty)]
    (let [neg? (str/starts-with? s "-")
          pos-sign? (str/starts-with? s "+")
          body (if (or neg? pos-sign?) (subs s 1) s)]
      (when (digits-only? body)
        ;; The fold is guarded so a 40-digit value answers nil (refuse, keep
        ;; the default) instead of throwing long overflow. 9223372036854775807
        ;; is Long/MAX_VALUE written out, because a cljc file that must also
        ;; load under babashka cannot name the boxed class.
        (when (<= (count body) 19)
          (let [n (reduce (fn [acc c]
                            (let [v (+ (* acc 10) (digit-value c))]
                              (if (<= v 9223372036854775807) v (reduced nil))))
                          0
                          body)]
            (when (some? n)
              (if neg? (- n) n))))))))

(defn- size-ish?
  "True when `s` reads as a JVM size: digits, or digits then one of the
   k/m/g/t suffixes."
  [s]
  (let [n (count s)]
    (cond
      (zero? n) false
      (digits-only? s) true
      :else (and (< 1 n)
                 (contains? #{"k" "m" "g" "t"} (subs s (dec n)))
                 (digits-only? (subs s 0 (dec n)))))))

(defn- parse-size*
  "A JVM size string, trimmed and lower-cased, or nil.

   Accepts 10g, 512m, 2048k, 4t, and a bare byte count like 1073741824. The
   answer is the STRING, not a number of bytes (see the ns docstring)."
  [s]
  (when-let [s (some-> s str/trim not-empty)]
    (let [s (str/lower-case s)]
      (when (size-ish? s) s))))

(def true-words
  "Truthy spellings, matched case-insensitively.

   bin/hive-mcp tests a different spelling per knob (== 1 for
   HIVE_MCP_SUPERVISE, == 1 or == true for HIVE_FAST_BOOT, != off for
   HIVE_MCP_RESTART_LOCK). One shared vocabulary means a person can write on
   anywhere and be understood: a widening of what is ACCEPTED, never of what
   those words mean."
  #{"1" "true" "on" "yes" "y"})

(def false-words
  "Falsy spellings, matched case-insensitively."
  #{"0" "false" "off" "no" "n"})

(defn- parse-bool*
  "true or false from the vocabularies above; nil otherwise."
  [s]
  (let [s (some-> s str/trim str/lower-case not-empty)]
    (cond
      (contains? true-words s) true
      (contains? false-words s) false
      :else nil)))

(defn- keyword-body-char?
  "True for the characters a keyword body may hold: letters, digits, and the
   four separators keywords commonly use. Space, quotes and commas reject,
   because clojure.core/keyword would happily accept them."
  [c]
  (or (alpha-char? c)
      (boolean (digit-value c))
      (contains? #{"-" "_" "/" "."} (str c))))

(defn- parse-keyword*
  "A keyword from :avoid, avoid or AVOID; nil for junk."
  [s]
  (when-let [s (some-> s str/trim not-empty)]
    (let [s (if (str/starts-with? s ":") (subs s 1) s)]
      (when (and (not-empty s)
                 (every? keyword-body-char? s))
        (keyword (str/lower-case s))))))

(defn- parse-string*
  "The string itself, trimmed, or nil when blank.

   A blank variable is unset in the shell's eyes, and a launcher that treats
   an empty value as a path produces a config nobody asked for."
  [s]
  (when (string? s)
    (let [trimmed (str/trim s)]
      (when (pos? (count trimmed)) trimmed))))

(def parsers
  "Parser registry: :parse KIND -> (fn [string] value-or-nil).

   Every parser is TOTAL: no exception escapes on any string input, including
   empty, whitespace, unicode and multi-megabyte junk."
  {:int parse-int*
   :size parse-size*
   :bool parse-bool*
   :keyword parse-keyword*
   :string parse-string*})

(defn parse-knob
  "Parse one knob's raw environment value with its KIND. Total: nil on junk.

   Accepts a knob map ({:parse :int}) or a bare KIND (:int). An UNKNOWN KIND
   answers nil rather than throwing or guessing."
  [kind-or-knob s]
  (let [k (if (map? kind-or-knob) (:parse kind-or-knob) kind-or-knob)]
    (when-let [f (get parsers k)]
      (f s))))

;; =============================================================================
;; The table
;; =============================================================================

(def knobs
  "Every environment variable bin/hive-mcp (or the bin/oom-policy.sh it
   sources) reads, with its :launch path and parser KIND.

   Order follows the script: profile, fast boot, heap, forensics, OOM standing,
   restart lock, supervise.

   HIVE_MCP_CONFIG is deliberately ABSENT: it selects WHICH config.edn is
   read, so it is consumed before a :launch section exists and has no path
   inside one. LC3 applies it."
  [{:env "HIVE_PROFILE"
    :path [:profile]
    :parse :keyword}

   {:env "HIVE_FAST_BOOT"
    :path [:fast-boot?]
    :parse :bool}

   {:env "HIVE_MCP_XMX"
    :path [:jvm :xmx]
    :parse :size}

   {:env "HIVE_MCP_XMS"
    :path [:jvm :xms]
    :parse :size}

   {:env "HIVE_MCP_SOFT_MAX_HEAP"
    :path [:jvm :soft-max-heap]
    :parse :size}

   {:env "MALLOC_ARENA_MAX"
    :path [:jvm :malloc-arena-max]
    :parse :int}

   {:env "HIVE_MCP_COREDUMP"
    :path [:forensics :coredump?]
    :parse :bool}

   {:env "HIVE_MCP_HEAP_DUMP_DIR"
    :path [:forensics :heap-dump-dir]
    :parse :string}

   {:env "HIVE_MCP_OOM_SCORE_ADJ"
    :path [:oom :score-adj]
    :parse :int}

   {:env "HIVE_MCP_OOMD_PREFERENCE"
    :path [:oom :oomd-preference]
    :parse :keyword}

   {:env "HIVE_MCP_RESTART_LOCK"
    :path [:restart-lock :enabled?]
    :parse :bool}

   {:env "HIVE_MCP_RESTART_LOCK_FILE"
    :path [:restart-lock :file]
    :parse :string}

   {:env "HIVE_MCP_RESTART_BY"
    :path [:restart-lock :by]
    :parse :string
    :note "who claims the lock; recorded in the holder file and restarts.log"}

   {:env "HIVE_MCP_NREPL_PORT"
    :path [:nrepl-port]
    :parse :int
    :note "names the restart-lock file; the server reads the same variable"}

   {:env "HIVE_MCP_SUPERVISE"
    :path [:supervise :enabled?]
    :parse :bool}

   {:env "HIVE_MCP_MAX_RESTARTS"
    :path [:supervise :max-restarts]
    :parse :int}

   {:env "HIVE_MCP_RESTART_WINDOW"
    :path [:supervise :window-s]
    :parse :int}])

(defn knob-for
  "The knob map for environment variable `name` (exact and case-sensitive, as
   environment variables are), or nil."
  [name]
  (some (fn [k] (when (= name (:env k)) k)) knobs))

(defn read-knobs
  "Apply every knob present in `env` to `base`, answering the merged map.

   A knob whose value does not parse is IGNORED and leaves the base value
   standing — the same outcome as leaving the variable unset, which is what
   the shell's ${VAR:-default} does with an empty value.

   Answers {:launch merged :ignored [[variable raw-value] ...]}. The report is
   kept BESIDE the launch map, not inside it: a stray :ignored key inside a
   :launch section would be carried into anything that serializes it, and
   LaunchConfig is deliberately open, so validation would not catch it."
  [env base]
  (let [merged (reduce (fn [acc knob]
                         (let [var-name (:env knob)
                               raw (get env var-name)
                               v (when (some? raw) (parse-knob knob raw))]
                           (if (some? v)
                             (assoc-in acc (:path knob) v)
                             acc)))
                       base
                       knobs)
        ignored (into []
                      (keep (fn [knob]
                              (let [var-name (:env knob)
                                    raw (get env var-name)]
                                (when (and (some? raw)
                                           (nil? (parse-knob knob raw)))
                                  [var-name raw]))))
                      knobs)]
    {:launch merged
     :ignored ignored}))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> digit-value [:=> [:cat :any] [:maybe :int]])
(m/=> parse-knob [:=> [:cat [:or :keyword :map] [:maybe :string]] :any])
(m/=> knob-for [:=> [:cat :string] [:maybe :map]])
(m/=> read-knobs
      [:=> [:cat [:maybe :map] :map]
       [:map [:launch :map] [:ignored [:vector [:tuple :string :string]]]]])

(comment
  (parse-knob :int "-500")        ;=> -500
  (parse-knob :int "+7")          ;=> 7
  (parse-knob :int "-500x")       ;=> nil
  (parse-knob :size "10g")        ;=> "10g"
  (parse-knob :size "10")         ;=> "10"
  (parse-knob :size "lots")       ;=> nil
  (parse-knob :bool "on")         ;=> true
  (parse-knob :bool "0")          ;=> false
  (parse-knob :bool "maybe")      ;=> nil
  (parse-knob :keyword ":avoid")  ;=> :avoid
  (parse-knob :keyword "avoid me") ;=> nil
  (parse-knob :nonesuch "value")   ;=> nil
  (count knobs)                   ;=> 17
  (read-knobs {"HIVE_MCP_XMX" "24g" "HIVE_MCP_MAX_RESTARTS" "nope"} {})
  ;=> {:launch {:jvm {:xmx "24g"}}
  ;    :ignored [["HIVE_MCP_MAX_RESTARTS" "nope"]]}
  )