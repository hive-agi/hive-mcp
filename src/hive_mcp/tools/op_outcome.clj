(ns hive-mcp.tools.op-outcome
  "Pure classifier for whether one batch operation failed, and why.

   Both batch paths call `op-outcome`:
   `hive-mcp.tools.cli/make-batch-handler` on the raw handler result, and
   `hive-mcp.batch/enrich-op-result` on the parsed result data.

   A result is failed when any rule below matches. The first match, in this
   order, names the `:kind`:

   - :success-false  a map whose :success is false
   - :body-failure   a text envelope whose :text is a JSON or EDN map with
                     :success false
   - :is-error       :isError true
   - :ok-false       :ok false
   - :status-failure a :status of invalid or error (keyword or string), on
                     the map itself or on its text body
   - :error-key      a non-nil :error on a map with no :success key, on the
                     map itself or on its text body (an explicit :success
                     true wins over a stray :error)
   - :null-id        opts {:null-id-key K} given, and the map holds K
                     with a nil value

   Anything else, including nil, strings, vectors and JSON arrays, is not
   failed. No dispatch, no extensions, no logging.

   SPDX-License-Identifier: AGPL-3.0-or-later"
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [malli.core :as m]
            [hive-mcp.resilience.swallow :as swallow]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn text-body
  "The map a text envelope's :text holds, read as JSON, else as EDN (hive
   handlers that `pr-str` their reply), else nil. Never throws."
  [result]
  (let [t (when (map? result) (:text result))]
    (when (and (string? t) (str/starts-with? (str/triml t) "{"))
      (let [body (or (swallow/attempt-ex (json/read-str t :key-fn keyword))
                     (swallow/attempt-ex (edn/read-string {:default (fn [_tag v] v)} t)))]
        (when (map? body) body)))))

(defn json-failure-body
  "The parsed body (JSON or EDN) of a text envelope when it is a map whose
   :success is false, else nil."
  [result]
  (let [body (text-body result)]
    (when (false? (:success body))
      body)))

(def failed-statuses
  "The :status values that mean the handler refused or failed the op."
  #{"invalid" "error"})

(defn- error-only?
  "True when map `m` carries a non-nil :error and no :success key."
  [m]
  (and (map? m) (some? (:error m)) (not (contains? m :success))))

(defn- failed-status
  "The failed :status of map `m` as a string, or nil."
  [m]
  (let [st (when (map? m) (:status m))]
    (when (or (keyword? st) (string? st))
      (failed-statuses (str/lower-case (name st))))))

(defn- reported-reason
  "The first failure reason `m` states, as a string: the head of a
   sequential :errors, then :error, :reason, :message. Nil when none."
  [m]
  (let [errs (:errors m)]
    (some-> (or (when (sequential? errs) (first errs))
                (when (and (map? errs) (seq errs))
                  (let [[k v] (first errs)]
                    (str (if (keyword? k) (name k) k) ": "
                         (if (sequential? v) (first v) v))))
                (:error m)
                (:reason m)
                (:message m))
            str)))

(defn- failure-kind
  "The first rule `result` matches, or nil."
  [result body status-body null-id-key]
  (when (map? result)
    (cond
      (false? (:success result))         :success-false
      body                               :body-failure
      (true? (:isError result))          :is-error
      (false? (:ok result))              :ok-false
      (or (failed-status result)
          (failed-status status-body))   :status-failure
      (error-only? result)               :error-key
      (error-only? status-body)          :error-key
      (and null-id-key
           (contains? result null-id-key)
           (nil? (get result null-id-key))) :null-id)))

(defn- failure-message
  "Human-readable reason for a failed `result` of `kind`."
  [kind result body status-body]
  (case kind
    :success-false (or (reported-reason result)
                       "tool reported failure (:success false)")
    :body-failure  (or (reported-reason body)
                       "tool reported failure (body :success false)")
    (:is-error
     :ok-false)    (or (reported-reason result)
                       (some-> (:text result) str)
                       "tool reported failure (:isError/:ok false)")
    :status-failure (let [m (if (failed-status result) result status-body)]
                      (or (reported-reason m)
                          (str "tool reported :status " (failed-status m))))
    :error-key     (str (:error (if (error-only? result) result status-body)))
    :null-id       "creation tool returned nil id, degraded backend?"))

(defn op-outcome
  "Classify a handler result. Returns {:failed? false}, or
   {:failed? true :kind KIND :message STRING} with KIND as in the ns doc.
   opts: {:null-id-key K} arms the :null-id rule for creation tools."
  ([result]
   (op-outcome result nil))
  ([result {:keys [null-id-key]}]
   (let [parsed (text-body result)
         body   (when (false? (:success parsed)) parsed)
         kind   (failure-kind result body parsed null-id-key)]
     (if kind
       {:failed? true :kind kind :message (failure-message kind result body parsed)}
       {:failed? false}))))

(defn op-failed?
  "True when `result` is failed under `op-outcome`."
  ([result] (op-failed? result nil))
  ([result opts] (:failed? (op-outcome result opts))))

;; =============================================================================
;; Contracts
;; =============================================================================

(def FailureKind
  "The rule a failed result matched, in the order the ns doc lists them."
  [:enum :success-false :body-failure :is-error :ok-false :status-failure :error-key :null-id])

(def Outcome
  "What `op-outcome` answers: not failed, or failed with a kind and a reason."
  [:or
   [:map {:closed true} [:failed? [:= false]]]
   [:map {:closed true}
    [:failed? [:= true]]
    [:kind FailureKind]
    [:message :string]]])

(def OutcomeOpts
  "op-outcome options; :null-id-key arms the :null-id rule."
  [:maybe [:map [:null-id-key {:optional true} :any]]])

(m/=> text-body [:=> [:cat :any] [:maybe :map]])

(m/=> json-failure-body [:=> [:cat :any] [:maybe [:map [:success [:= false]]]]])

(m/=> op-outcome [:function
                  [:=> [:cat :any] Outcome]
                  [:=> [:cat :any OutcomeOpts] Outcome]])

(m/=> op-failed? [:function
                  [:=> [:cat :any] :boolean]
                  [:=> [:cat :any OutcomeOpts] :boolean]])
