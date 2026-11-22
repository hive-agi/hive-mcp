(ns hive-mcp.config.set-journal
  "Append-only EDNL journal of MCP `config set` attempts: one line per
   attempt, allowed or refused, at `default-path` unless a path is passed.

   A line carries :at :key :path :decision :rule :mode :matched
   :verified-caller-id :caller-id :verified? :session-id :agent-id and :value.
   The value is redacted before it is written, by the `redact` fn the caller
   passes: `(redact path value)`, which is the config tool's `redact-config`.
   It is a parameter so this namespace never requires the tool namespace
   that will require it.

   `append!` never throws. A failed redaction or write is logged and answered
   with false, so a journal fault cannot block the write it records."
  (:require [clojure.java.io :as io]
            [hive-mcp.config.path :as config-path]
            [hive-mcp.context.request :as request]
            [malli.core :as m]
            [taoensso.timbre :as log]))
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

(defn record
  "The journal line for one `attempt`, with its value passed through
   `redact`. Pure apart from `redact` and the clock when :at is absent."
  [redact {:keys [decision value agent-id identity at]}]
  (let [{:keys [key path rule mode matched]} decision]
    {:at                 (or at (java.util.Date.))
     :key                key
     :path               path
     :decision           (:decision decision)
     :rule               rule
     :mode               mode
     :matched            matched
     :verified-caller-id (:verified-caller-id identity)
     :caller-id          (:caller-id identity)
     :verified?          (boolean (:verified? identity))
     :session-id         (:session-id identity)
     :agent-id           agent-id
     :value              (redact path value)}))

(m/=> record [:=> [:cat Redact Attempt] Line])

(defn append!
  "Append the line for `attempt` to the journal at `path` (default
   `default-path`). True when the line reached the file; false, after a log
   line, when redaction or the write failed. Never throws."
  ([redact attempt] (append! (default-path) redact attempt))
  ([path redact attempt]
   (try
     (let [line (record redact attempt)
           f    (io/file path)]
       (io/make-parents f)
       (spit f (str (pr-str line) "\n") :append true)
       true)
     (catch Throwable t
       (log/error t "config-set journal: append failed; this attempt is not journalled"
                  {:path (str path) :key (get-in attempt [:decision :key])})
       false))))

(m/=> append!
      [:function
       [:=> [:cat Redact :any] :boolean]
       [:=> [:cat :any Redact :any] :boolean]])
