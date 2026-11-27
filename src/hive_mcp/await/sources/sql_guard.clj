(ns hive-mcp.await.sources.sql-guard
  "Pure guard for the :sql-predicate await source: is this query ONE
   read-only SELECT, and what is it with its $n placeholders bound.

   This guard is the FIRST of three layers, not the only one. The adapter
   also runs every query in a READ ONLY transaction that it always rolls
   back, so a write that slips past this text check is still refused by
   Postgres. The guard exists to refuse loudly at create time, before
   anything is recorded, not to be the safety boundary.

   The query is split into code and literals ('..', E'..', \"..\") so a
   keyword inside a string never counts, and a placeholder inside a string
   is never bound. Refused outright: comments, dollar-quoted strings, a `;`
   anywhere but at the very end, a bare `?` (use $1, $2, ...), a first
   keyword other than SELECT or WITH, and any write or session keyword in
   the code.

   Placeholders are Postgres style, $1..$n, bound IN ORDER through a
   prepared statement: (bind sql params) rewrites each $k to a JDBC `?` and
   answers the parameter vector in placeholder order, so $2 may come before
   $1 or appear twice. A value is never spliced into the text."
  (:require [clojure.string :as str]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def max-sql-chars
  "The longest query text accepted."
  8000)

(def forbidden-words
  "Keywords that have no place in a read-only SELECT. Matched as whole
   words in the code, case-insensitively."
  #{"insert" "update" "delete" "merge" "truncate" "drop" "alter" "create"
    "grant" "revoke" "copy" "call" "do" "lock" "vacuum" "analyze" "cluster"
    "reindex" "into" "set" "reset" "listen" "notify" "unlisten" "refresh"
    "prepare" "execute" "deallocate" "discard" "checkpoint" "load" "import"
    "begin" "commit" "rollback" "savepoint"})

(def forbidden-function
  "Server functions with a side effect a READ ONLY transaction does not
   stop (signalling a backend, sleeping, reading server files, advisory
   locks, remote links, config). Matched on the code, case-insensitively."
  #"(?i)\b(pg_terminate_backend|pg_cancel_backend|pg_sleep\w*|pg_reload_conf|pg_rotate_logfile|pg_advisory\w*|pg_try_advisory\w*|pg_read_\w+|pg_ls_\w+|pg_stat_file|pg_file_\w+|pg_logical_\w+|pg_notify|pg_promote|pg_switch_wal|lo_\w+|dblink\w*|set_config|nextval|setval|query_to_xml\w*|xpath\w*)\s*\(")

(def Segment
  "One piece of a split query: code, or a literal kept verbatim."
  [:tuple [:enum :code :lit] :string])

(defn- refuse [message data]
  (throw (ex-info (str "await sql-predicate: " message)
                  (merge {:error :await/invalid-subscription :source :sql-predicate} data))))

(defn- lit-end
  "Index just past the literal that opens at `i` with quote `q`; `esc?`
   when backslash escapes apply (an E'..' string). Throws when unclosed."
  [^String s i q esc?]
  (let [n (count s)]
    (loop [j (inc i)]
      (cond
        (>= j n) (refuse "unterminated quoted text" {:at i})
        (and esc? (= \\ (.charAt s j))) (recur (+ j 2))
        (= q (.charAt s j)) (if (and (< (inc j) n) (= q (.charAt s (inc j))))
                              (recur (+ j 2))
                              (inc j))
        :else (recur (inc j))))))

(defn segments
  "`sql` split into [[:code s] [:lit s] ...]. Throws a refusal on a
   comment, a dollar-quoted string or an unterminated literal. Pure."
  [^String sql]
  (let [n (count sql)]
    (loop [i 0 start 0 out []]
      (if (>= i n)
        (cond-> out (< start n) (conj [:code (subs sql start n)]))
        (let [c   (.charAt sql i)
              nxt (when (< (inc i) n) (.charAt sql (inc i)))]
          (cond
            (or (and (= c \-) (= nxt \-)) (and (= c \/) (= nxt \*)))
            (refuse "comments are not accepted in the query" {:at i})

            (and (= c \$) (some? nxt) (not (Character/isDigit (char nxt))))
            (refuse "dollar-quoted text is not accepted; use $1, $2 ... placeholders" {:at i})

            (or (= c \') (= c \"))
            (let [e?  (and (= c \') (pos? i)
                           (#{\E \e} (.charAt sql (dec i)))
                           (or (= 1 i) (not (Character/isLetterOrDigit (.charAt sql (- i 2))))))
                  from (if e? (dec i) i)
                  end  (lit-end sql i c e?)]
              (recur end end (cond-> out
                               (< start from) (conj [:code (subs sql start from)])
                               true (conj [:lit (subs sql from end)]))))

            :else (recur (inc i) start out)))))))

(m/=> segments [:=> [:cat :string] [:vector Segment]])

(defn- code-text [segs]
  (str/join " " (keep (fn [[k s]] (when (= :code k) s)) segs)))

(defn- words [code]
  (map str/lower-case (re-seq #"[A-Za-z_][A-Za-z0-9_$]*" code)))

(defn check-select!
  "Throw a refusal unless `sql` is one read-only SELECT (see the namespace
   doc); answer its segments, with a single trailing `;` dropped. Pure."
  [sql]
  (when-not (and (string? sql) (not (str/blank? sql)))
    (refuse "a query is required" {}))
  (when (> (count sql) max-sql-chars)
    (refuse (str "the query is longer than " max-sql-chars " characters") {}))
  (let [segs  (segments (str/trim sql))
        segs  (if-let [[k s] (peek segs)]
                (if (and (= :code k) (str/ends-with? (str/trimr s) ";"))
                  (conj (pop segs) [:code (subs (str/trimr s) 0 (dec (count (str/trimr s))))])
                  segs)
                segs)
        code  (code-text segs)
        ws    (words code)
        bad   (sort (distinct (filter forbidden-words ws)))]
    (when (str/includes? code ";")
      (refuse "one statement only: `;` is accepted only at the end" {}))
    (when (str/includes? code "?")
      (refuse "a bare `?` is not accepted: bind values with $1, $2 ..." {}))
    (when-not (#{"select" "with"} (first ws))
      (refuse "only a SELECT (or WITH ... SELECT) is accepted" {:first (first ws)}))
    (when (seq bad)
      (refuse (str "write or session keywords are not accepted: " (str/join ", " bad))
              {:keywords (vec bad)}))
    (when-let [f (re-find forbidden-function code)]
      (refuse (str "server function " (second f) "() is not accepted in an await query")
              {:function (second f)}))
    segs))

(m/=> check-select! [:=> [:cat :any] [:vector Segment]])

(defn placeholder-order
  "The $k numbers of the code segments of `segs`, in the order they occur.
   Pure."
  [segs]
  (into [] (comp (filter (comp #{:code} first))
                 (mapcat (fn [[_ s]] (map (comp parse-long second) (re-seq #"\$(\d+)" s)))))
        segs))

(m/=> placeholder-order [:=> [:cat [:vector Segment]] [:vector :int]])

(defn check-params!
  "Throw a refusal unless `params` fit the placeholders of `segs`: every
   $k has a value (1 <= k <= count) and every value is used. Values must be
   strings, numbers, booleans or nil. Answers params as a vector. Pure."
  [segs params]
  (let [params (vec (or params []))
        order  (placeholder-order segs)
        used   (set order)
        n      (count params)]
    (when-let [bad (seq (remove #(<= 1 % n) order))]
      (refuse (str "placeholder $" (first bad) " has no value: " n " param(s) given")
              {:placeholders (vec (sort used)) :params n}))
    (when-let [unused (seq (remove used (range 1 (inc n))))]
      (refuse (str "param(s) " (str/join ", " (map #(str "$" %) unused)) " are never used in the query")
              {:unused (vec unused)}))
    (when-let [bad (seq (remove #(or (nil? %) (string? %) (number? %) (boolean? %)) params))]
      (refuse "params must be strings, numbers, booleans or null" {:bad (mapv pr-str bad)}))
    params))

(m/=> check-params! [:=> [:cat [:vector Segment] :any] [:vector :any]])

(defn bind
  "The JDBC form of `sql` with `params`: {:sql s :params [..]}, where each
   $k became `?` and :params holds the value of each `?` in order. Throws a
   refusal when the query or its params are not accepted. Pure."
  [sql params]
  (let [segs   (check-select! sql)
        params (check-params! segs params)
        order  (placeholder-order segs)
        text   (apply str (map (fn [[k s]] (if (= :code k) (str/replace s #"\$\d+" "?") s)) segs))]
    {:sql text :params (mapv #(nth params (dec %)) order)}))

(m/=> bind [:=> [:cat :any :any] [:map [:sql :string] [:params [:vector :any]]]])
