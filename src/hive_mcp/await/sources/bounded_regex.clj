(ns hive-mcp.await.sources.bounded-regex
  "Regex matching with a time bound, for regexes a CALLER supplies.

   java.util.regex backtracks and has no timeout, and a cancelled
   ScheduledFuture does not interrupt it, so one pathological pattern over a
   long input can pin a shared poll thread for good. Two layers:
     - check-pattern! refuses, at create time, a pattern with a nested
       quantifier ((a+)+, (a*)*, (a|aa)+ ...) or a backreference, the usual
       shapes of catastrophic backtracking;
     - find-in matches through a CharSequence whose charAt checks a deadline,
       so whatever slips through still stops (throwing :await/regex-timeout)."
  (:require [clojure.string :as str]
            [malli.core :as m])
  (:import (java.util.regex Pattern PatternSyntaxException)))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def max-pattern-chars 500)

(defn nested-quantifier?
  "True when `rx` has a quantified group whose body is itself quantified or
   holds an alternation, the shape of catastrophic backtracking. A
   conservative text check over the pattern (escapes skipped). Pure."
  [rx]
  (let [s (str/replace (str rx) #"\\." "_")
        s (str/replace s #"\[[^\]]*\]" "_")]
    (loop [s s]
      (let [s' (str/replace s #"\(([^()]*)\)([*+?]|\{\d+,?\d*\})?"
                            (fn [[_ body q]]
                              (cond
                                (and q (or (re-find #"[*+]|\{\d+,\d*\}" body) (str/includes? body "|")))
                                "\u0000"
                                (str/includes? body "\u0000") "\u0000"
                                ;; a quantified group, or a group holding a
                                ;; quantifier, is itself a quantified unit
                                (or q (re-find #"[*+]|\{\d+,\d*\}" body)) "_+"
                                :else "_")))]
        (cond
          (str/includes? s' "\u0000") true
          (= s s') false
          :else (recur s'))))))

(m/=> nested-quantifier? [:=> [:cat :any] :boolean])

(defn check-pattern!
  "Throw `refuse` (a fn of a message) unless `rx` is a usable bounded
   pattern; answer the compiled Pattern."
  [rx refuse]
  (when-not (and (string? rx) (not (str/blank? rx))) (refuse ":regex is required"))
  (when (> (count rx) max-pattern-chars) (refuse (str ":regex is longer than " max-pattern-chars)))
  (when (re-find #"\\[1-9]|\\k<" rx) (refuse ":regex may not use backreferences"))
  (when (nested-quantifier? rx)
    (refuse ":regex has a nested quantifier (e.g. (a+)+); it could backtrack without bound"))
  (try (Pattern/compile rx)
       (catch PatternSyntaxException e (refuse (str ":regex does not compile: " (.getDescription e))))))

(deftype DeadlineChars [^CharSequence s ^long deadline-ns]
  CharSequence
  (length [_] (.length s))
  (charAt [_ i]
    (when (> (System/nanoTime) deadline-ns)
      (throw (ex-info "await: regex match exceeded its time bound" {:error :await/regex-timeout})))
    (.charAt s i))
  (subSequence [_ a b] (DeadlineChars. (.subSequence s a b) deadline-ns))
  (toString [_] (str s)))

(defn find-in
  "The first match of `pattern` in `s` (a string), or nil, giving up with
   :await/regex-timeout once `budget-ms` is spent."
  [^Pattern pattern ^String s budget-ms]
  (let [m (.matcher pattern (DeadlineChars. s (+ (System/nanoTime) (* 1000000 (long budget-ms)))))]
    (when (.find m) (.group m))))
