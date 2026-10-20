(ns hive-mcp.resilience.protocol
  "L0 contract — resilience shared kernel.

   Both `hive-mcp.vectordb` and `hive-milvus.store` consume this; the
   `ErrorClass` ADT is the published language that crosses the
   boundary so neither side has to know the other's error vocabulary.

   The ADT is closed; new variants force a compile-time decision
   everywhere the classifier feeds policy. This is the design that
   prevents the silent 1804 misclassification — a `:schema-mismatch`
   variant cannot be ignored by a `:transient`-only retry policy.

   Reload-safety: `defonce`-guarded for `IErrorClassifier`."
  (:require [hive-dsl.adt :as adt]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(adt/defadt ErrorClass
  "Closed sum type of error classes observed at vector-store boundaries.

   - `:err/transient`        — IO drop / connection reset / 5xx. Heal loop
                               can rebuild the client; retry is sound.
   - `:err/schema-mismatch`  — vector dim ≠ collection dim, missing field,
                               malformed row. Caller bug — never retry.
   - `:err/auth`             — 401/403, expired token. Retry is futile.
   - `:err/validation`       — caller-side input rejected (oversize doc,
                               unknown type). Caller fixes input, not us.
   - `:err/unknown`          — uncategorized. Conservative retry budget,
                               then surface."
  [:err/transient        {:message string?}]
  [:err/schema-mismatch  {:message string? :details map?}]
  [:err/auth             {:message string?}]
  [:err/validation       {:message string?}]
  [:err/unknown          {:message string?}])

;;; ---------------------------------------------------------------------------
;;; Malli shape of an ErrorClass value (the ADT's runtime representation)
;;; ---------------------------------------------------------------------------

(def ErrorClassVariant
  "The closed set of `ErrorClass` variants."
  [:enum :err/transient :err/schema-mismatch :err/auth :err/validation :err/unknown])

(def ErrorClassValue
  "Any `ErrorClass` ADT value. The payload key set depends on the variant
   (see `defadt ErrorClass` above), so the map is open beyond :adt/*."
  [:map
   [:adt/variant ErrorClassVariant]
   [:adt/type [:= :ErrorClass]]
   [:message :string]])

(defonce ^:private -ierrorclassifier-defined? (atom false))

(when (compare-and-set! -ierrorclassifier-defined? false true)
  (defprotocol IErrorClassifier
    "Map a Throwable to a closed `ErrorClass` ADT value."

    (classify [this ex]
      "Inspect `ex` (and its cause chain) and return an `ErrorClass`
       variant. MUST be total — never throws, never returns nil. An
       unrecognized exception classifies as `:err/unknown`.")))

(defn schema-mismatch?
  "True if `err-class` is the `:err/schema-mismatch` variant. Helper
   so policy.clj and resilience.clj can short-circuit without
   importing `hive-dsl.adt` directly."
  [err-class]
  (= :err/schema-mismatch (adt/adt-variant err-class)))

;; accepts any value — adt-variant of a non-ADT value answers nil, hence false
(m/=> schema-mismatch? [:=> [:cat :any] :boolean])

(defn transient?
  "True if `err-class` is the `:err/transient` variant."
  [err-class]
  (= :err/transient (adt/adt-variant err-class)))

(m/=> transient? [:=> [:cat :any] :boolean])
