(ns hive-mcp.await.sources.sql-predicate
  "The :sql-predicate await source: poll a read-only Postgres SELECT until a
   predicate over its result holds. This answers 'did the data land', which
   a log line cannot: a log line proves only that a job ran.

   Subscription:
     {:type :sql-predicate
      :poll-ms 300000                         ; >= 30 s, default 5 min
      :spec {:connection-ref \"pass:sisf/staging-pg-url\"  ; or \"env:NAME\"
             :query  \"select 1 from contributor.contributors
                      where id = $1 and address_complement = $2\"
             :params [42 \"Apto 3\"]          ; bound in a prepared statement
             :predicate {:kind \"exists\"}    ; see Predicate
             :row-cap 100                     ; <= 1000
             :statement-timeout-ms 5000}}     ; <= 30000
   plus a REQUIRED deadline on the awaiter (deadline or timeout_ms, <= 48 h).

   Predicates:
     {:kind exists}                        at least one row
     {:kind count :op \">=\" :value N}       row count compared with N
     {:kind scalar :op \"=\" :value v
      :column \"c\"?}                        the first row's column c (default
                                           its only/first column) compared
   An op is one of > >= < <= = == !=.

   Read-only by construction: the guard (sql-guard) refuses anything but one
   SELECT at create time, and the runner (adapters.jdbc) runs it in a READ
   ONLY transaction it always rolls back, under statement_timeout and a row
   cap. The connection is a secret REF; an inline URL or password is
   refused, so no credential is ever persisted in the registry.

   Ports (DIP): *runner* (ISqlRunner) and *secrets* (ISecretResolver) are
   read when the source OPENS; poll/*clock* and poll/*scheduler* likewise.
   A test binds stubs around create!."
  (:require [clojure.string :as str]
            [hive-mcp.await.adapters.secrets :as secrets]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.sources.boundary :as b]
            [hive-mcp.await.sources.poll :as poll]
            [hive-mcp.await.sources.sql-guard :as guard]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def timing
  "Poll interval bounds: at least 30 s, 5 min by default."
  {:min-poll-ms 30000 :default-poll-ms 300000})

(def default-row-cap 100)
(def max-row-cap 1000)
(def default-statement-timeout-ms 5000)
(def max-statement-timeout-ms 30000)
(def sample-rows
  "How many result rows a wake event carries."
  3)

(def Predicate
  [:or
   [:map [:kind [:= :exists]]]
   [:map [:kind [:= :count]] [:op :string] [:value number?]]
   [:map [:kind [:= :scalar]] [:op :string] [:value :any] [:column {:optional true} [:maybe :string]]]])

(def Parsed
  "A subscription spec as the source runs it."
  [:map
   [:connection-ref :string]
   [:sql :string]
   [:params [:vector :any]]
   [:predicate Predicate]
   [:row-cap pos-int?]
   [:statement-timeout-ms pos-int?]])

(defn- refuse [message & [data]]
  (throw (poll/refusal :sql-predicate message (or data {}))))

(defn- bounded [v default max-v what]
  (let [v (if (nil? v) default v)]
    (when-not (and (integer? v) (pos? v))
      (refuse (str what " must be a positive integer")))
    (when (> v max-v)
      (refuse (str what " " v " is over the maximum " max-v)))
    (long v)))

(defn parse-predicate
  "The Predicate `p` names, normalised, or a refusal. Pure."
  [p]
  (let [p    (poll/kw-keys (if (string? p) {:kind p} p))
        kind (some-> (:kind p) name str/lower-case keyword)
        op   (some-> (:op p) name str/trim)]
    (case kind
      :exists {:kind :exists}
      :count  (let [n (poll/->number (:value p))]
                (when-not (poll/comparator-of op) (refuse (str "count predicate: unknown op " (pr-str op))))
                (when-not n (refuse "count predicate: :value must be a number"))
                {:kind :count :op op :value n})
      :scalar (do (when-not (poll/comparator-of op) (refuse (str "scalar predicate: unknown op " (pr-str op))))
                  (when-not (contains? p :value) (refuse "scalar predicate: :value is required"))
                  (cond-> {:kind :scalar :op op :value (:value p)}
                    (some? (:column p)) (assoc :column (str (:column p)))))
      (refuse "predicate :kind must be exists, count or scalar" {:kind (:kind p)}))))

(m/=> parse-predicate [:=> [:cat :any] Predicate])

(defn parse
  "The Parsed spec of `subscription`, or a refusal. Pure."
  [subscription]
  (let [spec (poll/spec-of subscription)
        ref  (:connection-ref spec)
        {:keys [sql params]} (guard/bind (:query spec) (:params spec))]
    (when-not (secrets/ref-kind ref)
      (refuse (str ":connection-ref must name a secret (pass:<path> or env:<NAME>);"
                   " an inline URL or password is never accepted")))
    {:connection-ref       (str/trim ref)
     :sql                  sql
     :params               params
     :predicate            (parse-predicate (or (:predicate spec) {:kind "exists"}))
     :row-cap              (bounded (:row-cap spec) default-row-cap max-row-cap ":row-cap")
     :statement-timeout-ms (bounded (:statement-timeout-ms spec) default-statement-timeout-ms
                                    max-statement-timeout-ms ":statement-timeout-ms")}))

(m/=> parse [:=> [:cat :map] Parsed])

(defn observed
  "What `predicate` reads from `result` {:rows :truncated?}. Pure."
  [predicate {:keys [rows]}]
  (case (:kind predicate)
    :exists (boolean (seq rows))
    :count  (count rows)
    :scalar (let [row (first rows)]
              (if-let [c (:column predicate)]
                (get row (keyword c))
                (some-> row vals first)))))

(m/=> observed [:=> [:cat Predicate :map] :any])

(defn evaluate
  "{:match? bool :data map} for `result` under `parsed`. A count over a
   truncated result counts at most row-cap, which is said in :truncated?.
   Pure."
  [parsed result]
  (let [p   (:predicate parsed)
        obs (observed p result)
        ok? (case (:kind p)
              :exists obs
              (:count :scalar) (poll/compare-values (:op p) obs (:value p)))]
    {:match? (boolean ok?)
     :data   {:predicate  p
              :observed   obs
              :row-count  (count (:rows result))
              :truncated? (boolean (:truncated? result))
              :sample     (vec (take sample-rows (:rows result)))}}))

(m/=> evaluate [:=> [:cat Parsed :map] poll/ProbeResult])

;; =============================================================================
;; Ports and the source
;; =============================================================================

(def ^:dynamic *runner*
  "The ISqlRunner, read when the source opens. nil: the JDBC adapter,
   resolved late so this namespace loads without the driver."
  nil)

(def ^:dynamic *secrets*
  "The ISecretResolver, read when the source opens."
  secrets/default-resolver)

(defn- current-runner []
  (or *runner* @(requiring-resolve 'hive-mcp.await.adapters.jdbc/default-runner)))

(defn probe-fn
  "The zero-arg probe for `parsed` over `runner` and `resolver`: one query,
   evaluated. The secret is resolved on every probe and never retained."
  [parsed runner resolver]
  (fn probe []
    (let [url (b/resolve-secret! resolver (:connection-ref parsed))]
      (evaluate parsed (b/run-select! runner {:url url
                                              :sql (:sql parsed)
                                              :params (:params parsed)
                                              :row-cap (:row-cap parsed)
                                              :statement-timeout-ms (:statement-timeout-ms parsed)})))))

(defmethod ports/check-subscription :sql-predicate
  [_subscription awaiter]
  (poll/check! :sql-predicate timing awaiter parse))

(defmethod ports/open-source :sql-predicate
  [subscription awaiter deliver!]
  (let [parsed (parse subscription)]
    (poll/open-polling :sql-predicate timing awaiter deliver!
                       (probe-fn parsed (current-runner) *secrets*))))
