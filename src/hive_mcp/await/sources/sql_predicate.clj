(ns hive-mcp.await.sources.sql-predicate
  "The :sql-predicate await source: poll one read-only SQL query until a
   predicate over its result holds. This answers 'did the data land', which
   a log line cannot: a log line proves only that a job ran.

   Core holds only the source and its pure logic (predicates, bounds, the
   closed spec, the allowlist). The database is reached through the SQL
   adapter PORT (hive-mcp.await.sources.sql-port): the dialect (what counts
   as one read-only query, how $n binds) and the transport (connection,
   READ ONLY transaction, statement_timeout, row cap) are the adapter's.
   The Postgres adapter is the hive.await-sql addon. With no adapter
   registered, create is refused with :await/source-adapter-missing,
   naming the addon, before anything is recorded.

   Subscription:
     {:type :sql-predicate
      :poll-ms 300000                         ; >= 30 s, default 5 min
      :spec {:connection-ref \"k8s:sisf/staging-pg-ro/url\"  ; or vault:<row>
             :query  \"select 1 from contributor.contributors
                      where id = $1 and address_complement = $2\"
             :params [42 \"Apto 3\"]          ; bound by the adapter
             :predicate {:kind \"exists\"}    ; see Predicate
             :dialect \"postgres\"?           ; adapter id; default the only one
             :row-cap 100?                    ; <= 1000
             :statement-timeout-ms 5000?      ; <= 30000
             :sample-rows 0?}}                ; rows carried by the wake, <= 3
   plus a REQUIRED deadline on the awaiter (deadline or timeout_ms, <= 48 h).
   The spec is CLOSED: an unknown key is refused, never stored.

   The connection is a secret REF that the operator allowlisted
   (:await :sql :connection-refs); refs name a k8s Secret key (k8s:) or a
   hive vault row (vault:), never an inline DSN. :params are persisted in
   the registry as plain EDN: they are for ids, never for secrets.

   Predicates:
     {:kind exists}                        at least one row
     {:kind count :op \">=\" :value N}       row count compared with N
     {:kind scalar :op \"=\" :value v
      :column \"c\"?}                        the first row's column c (default
                                           its first column) compared

   The wake carries the predicate and what was observed; result ROWS only
   when :sample-rows asks for them (they may hold personal data, and a wake
   lands in the registry, the journal and the resumed prompt)."
  (:require [clojure.string :as str]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.sources.boundary :as b]
            [hive-mcp.await.sources.policy :as policy]
            [hive-mcp.await.sources.poll :as poll]
            [hive-mcp.await.sources.secret-refs :as refs]
            [hive-mcp.await.sources.sql-port :as port]
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
(def max-sample-rows 3)
(def max-query-chars 8000)

(def secret-schemes
  "The ref schemes a connection may use."
  #{"k8s" "vault"})

(def spec-keys
  #{:connection-ref :query :params :predicate :dialect :row-cap
    :statement-timeout-ms :sample-rows})

(def Predicate
  [:or
   [:map [:kind [:= :exists]]]
   [:map [:kind [:= :count]] [:op :string] [:value number?]]
   [:map [:kind [:= :scalar]] [:op :string] [:value :any] [:column {:optional true} [:maybe :string]]]])

(def Parsed
  "A subscription spec as the source runs it."
  [:map
   [:connection-ref :string]
   [:query :string]
   [:params [:vector :any]]
   [:dialect [:maybe :string]]
   [:predicate Predicate]
   [:row-cap pos-int?]
   [:statement-timeout-ms pos-int?]
   [:sample-rows :int]])

(defn- refuse [message & [data]]
  (throw (poll/refusal :sql-predicate message (or data {}))))

(defn- bounded [v default lo hi what]
  (let [v (if (nil? v) default v)]
    (when-not (and (integer? v) (<= lo v hi))
      (refuse (str what " must be an integer in " lo ".." hi)))
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
  "The Parsed spec of `subscription`, or a refusal. Pure over the policy:
   refs, bounds and the closed spec. The dialect check is the adapter's
   (see check!)."
  [subscription]
  (let [spec (poll/closed-spec :sql-predicate subscription spec-keys)
        ref  (:connection-ref spec)
        q    (:query spec)
        params (:params spec [])]
    (when-not (and (string? q) (not (str/blank? q))) (refuse ":query is required"))
    (when (> (count q) max-query-chars) (refuse (str ":query is longer than " max-query-chars)))
    (when-not (sequential? params) (refuse ":params must be an array"))
    (refs/check-ref! :sql-predicate ref secret-schemes)
    (policy/allowed! :sql-predicate [:sql :connection-refs] (str/trim ref) "connection ref")
    {:connection-ref       (str/trim ref)
     :query                q
     :params               (vec params)
     :dialect              (some-> (:dialect spec) name str/trim not-empty)
     :predicate            (parse-predicate (or (:predicate spec) {:kind "exists"}))
     :row-cap              (bounded (:row-cap spec) default-row-cap 1 max-row-cap ":row-cap")
     :statement-timeout-ms (bounded (:statement-timeout-ms spec) default-statement-timeout-ms
                                    100 max-statement-timeout-ms ":statement-timeout-ms")
     :sample-rows          (bounded (:sample-rows spec) 0 0 max-sample-rows ":sample-rows")}))

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

(defn- plain [v]
  (if (or (nil? v) (string? v) (number? v) (boolean? v)) v (str v)))

(defn evaluate
  "{:match? bool :data map} for `result` under `parsed`. A count over a
   truncated result counts at most row-cap, which is said in :truncated?.
   Rows are carried only when :sample-rows asks. Pure."
  [parsed result]
  (let [p   (:predicate parsed)
        obs (observed p result)
        ok? (case (:kind p)
              :exists obs
              (:count :scalar) (poll/compare-values (:op p) obs (:value p)))]
    {:match? (boolean ok?)
     :data   (cond-> {:predicate  p
                      :observed   (plain obs)
                      :row-count  (count (:rows result))
                      :truncated? (boolean (:truncated? result))}
               (pos? (:sample-rows parsed))
               (assoc :sample (mapv #(update-vals % plain)
                                    (take (:sample-rows parsed) (:rows result)))))}))

(m/=> evaluate [:=> [:cat Parsed :map] poll/ProbeResult])

;; =============================================================================
;; Ports and the source
;; =============================================================================

(def ^:dynamic *secrets*
  "The ISecretResolver, read when the source opens."
  refs/default-resolver)

(defn bound-query
  "`parsed`'s query bound by `adapter`'s dialect: {:sql :params}. Throws the
   adapter's refusal."
  [adapter parsed]
  ((:check-and-bind adapter) (:query parsed) (:params parsed)))

(defn probe-fn
  "The zero-arg probe: resolve the secret (bounded, never retained), run the
   bound query through `adapter`, evaluate."
  [parsed adapter resolver]
  (let [{:keys [sql params]} (bound-query adapter parsed)]
    (fn probe []
      (let [secret (b/resolve-secret! resolver (:connection-ref parsed))]
        (evaluate parsed ((:run-select! adapter)
                          {:secret secret :sql sql :params params
                           :row-cap (:row-cap parsed)
                           :statement-timeout-ms (:statement-timeout-ms parsed)}))))))

(defn check!
  "Refuse `awaiter` before anything is recorded: timing, the closed spec,
   the ref and its allowlist, a registered adapter, and the adapter's own
   dialect check of the query."
  [awaiter]
  (poll/check! :sql-predicate timing awaiter
               (fn [sub]
                 (let [parsed (parse sub)]
                   (bound-query (port/adapter-for (:dialect parsed)) parsed)
                   parsed))))

(defmethod ports/check-subscription :sql-predicate
  [_subscription awaiter]
  (check! awaiter))

(defmethod ports/open-source :sql-predicate
  [subscription awaiter deliver!]
  ;; the same refusal as create: at boot, a missing addon is reported as a
  ;; failed restore and the awaiter is kept on disk
  (let [parsed  (parse subscription)
        adapter (port/adapter-for (:dialect parsed))]
    (poll/open-polling :sql-predicate timing awaiter deliver!
                       (probe-fn parsed adapter *secrets*))))
