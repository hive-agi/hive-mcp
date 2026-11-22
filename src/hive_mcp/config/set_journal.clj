(ns hive-mcp.config.set-journal
  "Append-only EDNL journal of MCP `config set` attempts: one line per
   attempt, allowed or refused, at `default-path` unless a path is passed.

   A line carries :at :key :path :decision :rule :mode :matched
   :verified-caller-id :caller-id :verified? :session-id :agent-id and :value.
   The value is redacted before it is written, by the `redact` fn the caller
   passes: `(redact path value)`, which is the config tool's `redact-config`.
   It is a parameter so this namespace never requires the tool namespace
   that will require it. A key that did not parse has no path to redact at,
   so its value is never written (`non-canonical-placeholder`). A value is
   bounded to `max-value-chars` printed characters, and the key and every id
   to `max-id-chars`.

   Appends are serialised by one process-wide lock. Once the file passes
   `max-journal-bytes` it is moved to a timestamped name beside it
   (`rotated-file`); no journal file is ever deleted or replaced.

   `append!` never throws. A failed redaction or write is logged and answered
   with false, so a journal fault cannot block the write it records."
  (:require [clojure.java.io :as io]
            [hive-mcp.config.path :as config-path]
            [hive-mcp.context.request :as request]
            [malli.core :as m]
            [taoensso.timbre :as log]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def journal-file-name
  "File name of the journal, beside the process's config.edn."
  "config-set-journal.ednl")

(def Identity
  "Who asked for the write, as the request context knows it."
  [:map
   [:verified-caller-id [:maybe :string]]
   [:caller-id [:maybe :string]]
   [:verified? :boolean]
   [:session-id [:maybe :string]]])

(def Attempt
  "What `record` journals for one attempt: a write-policy Decision (or
   Classification) plus the value, the asserted agent id and the identity."
  [:map
   [:decision [:map [:key :string] [:decision :keyword] [:rule :keyword]]]
   [:value :any]
   [:agent-id {:optional true} [:maybe :string]]
   [:identity {:optional true} [:maybe Identity]]
   [:at {:optional true} [:maybe inst?]]])

(def Line
  "One journal line."
  [:map
   [:at inst?]
   [:key :string]
   [:path [:maybe [:vector :any]]]
   [:decision :keyword]
   [:rule :keyword]
   [:mode [:maybe :keyword]]
   [:matched :any]
   [:verified-caller-id [:maybe :string]]
   [:caller-id [:maybe :string]]
   [:verified? :boolean]
   [:session-id [:maybe :string]]
   [:agent-id [:maybe :string]]
   [:value :any]])

(def Redact
  "Redactor: (path value) -> the value safe to write."
  [:=> [:cat [:maybe [:vector :any]] :any] :any])

(defn default-path
  "The journal path: `journal-file-name` in the directory of the process's
   config.edn, so a test JVM running from its own config never writes the
   operator's journal."
  []
  (let [parent (.getParentFile (io/file config-path/config-path))]
    (str (io/file (or parent (io/file ".")) journal-file-name))))

(m/=> default-path [:=> [:cat] [:string {:min 1}]])

(defn request-identity
  "The identity of the request in flight, read from hive-mcp.context.request.
   Outside a request every id is nil and :verified? false."
  []
  (let [verified (request/current-verified-caller-id)]
    {:verified-caller-id verified
     :caller-id          (request/current-caller-id)
     :verified?          (some? verified)
     :session-id         (request/current-session-id)}))

(m/=> request-identity [:=> [:cat] Identity])

(def max-value-chars
  "Longest printed value a line keeps; a longer one is replaced by a
   truncated prefix and its length."
  2048)

(def max-journal-bytes
  "Size past which the journal is rotated to a timestamped name before an
   append."
  (* 16 1024 1024))

(def non-canonical-placeholder
  "What a line carries instead of the value when the key did not parse: with
   no path, no redactor can know which part of the value is secret."
  {:redacted :non-canonical-key})

(defn bound-value
  "`v` when it prints within `max-value-chars`, else {:truncated true :chars n
   :prefix s} with the first `max-value-chars` printed characters."
  [v]
  (let [s (pr-str v)]
    (if (<= (count s) max-value-chars)
      v
      {:truncated true :chars (count s) :prefix (subs s 0 max-value-chars)})))

(def max-id-chars
  "Longest key, caller id, session id or agent id a line keeps; a longer one
   keeps this many characters and a marker naming its full length."
  256)

(defn bound-str
  "`s` when it is nil or within `max-id-chars`, else its first
   `max-id-chars` characters followed by ...[<n> chars]."
  [s]
  (if (and (string? s) (> (count s) max-id-chars))
    (str (subs s 0 max-id-chars) "...[" (count s) " chars]")
    s))

(m/=> bound-str [:=> [:cat [:maybe :string]] [:maybe :string]])

(m/=> bound-value [:=> [:cat :any] :any])

(defn record
  "The journal line for one `attempt`. The value goes through `redact` at the
   decision's path, then `bound-value`. When the decision has no path (the
   key was not canonical), the value is `non-canonical-placeholder` and
   `redact` is not called. The key and every id go through `bound-str`, so no
   field the caller controls can make a line unbounded. Pure apart from
   `redact` and the clock when :at is absent."
  [redact {:keys [decision value agent-id identity at]}]
  (let [{:keys [key path rule mode matched]} decision]
    {:at                 (or at (java.util.Date.))
     :key                (bound-str key)
     :path               (when (vector? path)
                           (let [b (bound-value path)]
                             (if (vector? b) b [:truncated (:chars b)])))
     :decision           (:decision decision)
     :rule               rule
     :mode               mode
     :matched            matched
     :verified-caller-id (bound-str (:verified-caller-id identity))
     :caller-id          (bound-str (:caller-id identity))
     :verified?          (boolean (:verified? identity))
     :session-id         (bound-str (:session-id identity))
     :agent-id           (bound-str agent-id)
     :value              (if (vector? path)
                           (bound-value (redact path value))
                           non-canonical-placeholder)}))

(m/=> record [:=> [:cat Redact Attempt] Line])

(defonce ^:private write-lock
  ;; One lock per process for every journal path: an append is one write of
  ;; one line, never interleaved with another thread's.
  (Object.))

(def ^:private rotation-stamp
  (-> (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd'T'HHmmss'Z'")
      (.withZone java.time.ZoneOffset/UTC)))

(defn rotated-file
  "Where the journal at `f` is rotated to at instant `now`, attempt `n`:
   <stem>.<UTC basic ISO>.ednl beside it, with -<n> before the extension for
   n > 0 (the disambiguating suffix when that name is taken). A journal whose
   name has no .ednl extension gets .<stamp>[-n] appended."
  [^java.io.File f ^java.time.Instant now n]
  (let [nm     (.getName f)
        stamp  (str (.format ^java.time.format.DateTimeFormatter rotation-stamp now)
                    (when (pos? n) (str "-" n)))
        rotnm  (if (str/ends-with? nm ".ednl")
                 (str (subs nm 0 (- (count nm) 5)) "." stamp ".ednl")
                 (str nm "." stamp))]
    (io/file (or (.getParentFile f) (io/file ".")) rotnm)))

(m/=> rotated-file [:=> [:cat [:fn #(instance? java.io.File %)] inst? nat-int?]
                    [:fn #(instance? java.io.File %)]])

(defn rotate-if-full!
  "When `f` is past `max-journal-bytes`, move it to the first free
   `rotated-file` name for `now`. Never deletes and never replaces: the move
   refuses an existing target, so a taken name only moves on to the next
   suffix. Returns the rotated file, or nil when nothing was rotated. The
   caller holds the journal lock."
  ([f] (rotate-if-full! f (java.time.Instant/now)))
  ([^java.io.File f ^java.time.Instant now]
   (when (> (.length f) max-journal-bytes)
     (loop [n 0]
       (let [target (rotated-file f now n)
             moved  (try
                      (java.nio.file.Files/move (.toPath f) (.toPath ^java.io.File target)
                                                (make-array java.nio.file.CopyOption 0))
                      target
                      (catch java.nio.file.FileAlreadyExistsException _ nil))]
         (cond
           moved          moved
           (< n 10000)    (recur (inc n))
           :else          (throw (ex-info "config-set journal: no free rotation name"
                                          {:path (str f)}))))))))

(m/=> rotate-if-full!
      [:function
       [:=> [:cat [:fn #(instance? java.io.File %)]]
        [:maybe [:fn #(instance? java.io.File %)]]]
       [:=> [:cat [:fn #(instance? java.io.File %)] inst?]
        [:maybe [:fn #(instance? java.io.File %)]]]])

(defn append!
  "Append the line for `attempt` to the journal at `path` (default
   `default-path`), under one process-wide lock, first rotating the file to
   a timestamped name when it is past `max-journal-bytes`. True when the line
   reached the file; false, after a log line, when redaction or the write
   failed. Never throws."
  ([redact attempt] (append! (default-path) redact attempt))
  ([path redact attempt]
   (try
     (let [line (str (pr-str (record redact attempt)) "\n")
           f    (io/file path)]
       (locking write-lock
         (io/make-parents f)
         (rotate-if-full! f)
         (spit f line :append true))
       true)
     (catch Throwable t
       (log/error t "config-set journal: append failed; this attempt is not journalled"
                  {:path (str path) :key (get-in attempt [:decision :key])})
       false))))

(m/=> append!
      [:function
       [:=> [:cat Redact :any] :boolean]
       [:=> [:cat :any Redact :any] :boolean]])
