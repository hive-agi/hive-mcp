(ns hive-mcp.tools.op-outcome
  "Pure classifier for whether one batch operation failed, and why.

   Both batch paths call `op-outcome`:
   `hive-mcp.tools.cli/make-batch-handler` on the raw handler result, and
   `hive-mcp.batch/enrich-op-result` on the parsed result data.

   A result is failed when any rule below matches. The first match, in this
   order, names the `:kind`:

   - :success-false  a map whose :success is false
   - :body-failure   a text envelope whose :text is a JSON object with
                     :success false
   - :is-error       :isError true
   - :ok-false       :ok false
   - :error-key      a non-nil :error on a map with no :success key (an
                     explicit :success true wins over a stray :error)
   - :null-id        opts {:null-id-key K} given, and the map holds K
                     with a nil value

   Anything else, including nil, strings, vectors and JSON arrays, is not
   failed. No dispatch, no extensions, no logging.

   SPDX-License-Identifier: AGPL-3.0-or-later"
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn json-failure-body
  "The parsed JSON object in a text envelope's :text when it is a map whose
   :success is false, else nil."
  [result]
  (let [t (when (map? result) (:text result))]
    (when (and (string? t) (str/starts-with? (str/triml t) "{"))
      (let [body (try (json/read-str t :key-fn keyword) (catch Exception _ nil))]
        (when (and (map? body) (false? (:success body)))
          body)))))

(defn- reported-reason
  "The first failure reason `m` states, as a string: the head of a
   sequential :errors, then :error, :reason, :message. Nil when none."
  [m]
  (let [errs (:errors m)]
    (some-> (or (when (sequential? errs) (first errs))
                (:error m)
                (:reason m)
                (:message m))
            str)))

(defn- failure-kind
  "The first rule `result` matches, or nil."
  [result body null-id-key]
  (when (map? result)
    (cond
      (false? (:success result))         :success-false
      body                               :body-failure
      (true? (:isError result))          :is-error
      (false? (:ok result))              :ok-false
      (and (some? (:error result))
           (not (contains? result :success))) :error-key
      (and null-id-key
           (contains? result null-id-key)
           (nil? (get result null-id-key))) :null-id)))

(defn- failure-message
  "Human-readable reason for a failed `result` of `kind`."
  [kind result body]
  (case kind
    :success-false (or (reported-reason result)
                       "tool reported failure (:success false)")
    :body-failure  (or (reported-reason body)
                       "tool reported failure (body :success false)")
    (:is-error
     :ok-false)    (or (reported-reason result)
                       (some-> (:text result) str)
                       "tool reported failure (:isError/:ok false)")
    :error-key     (str (:error result))
    :null-id       "creation tool returned nil id, degraded backend?"))

(defn op-outcome
  "Classify a handler result. Returns {:failed? false}, or
   {:failed? true :kind KIND :message STRING} with KIND as in the ns doc.
   opts: {:null-id-key K} arms the :null-id rule for creation tools."
  ([result]
   (op-outcome result nil))
  ([result {:keys [null-id-key]}]
   (let [body (json-failure-body result)
         kind (failure-kind result body null-id-key)]
     (if kind
       {:failed? true :kind kind :message (failure-message kind result body)}
       {:failed? false}))))

(defn op-failed?
  "True when `result` is failed under `op-outcome`."
  ([result] (op-failed? result nil))
  ([result opts] (:failed? (op-outcome result opts))))
