(ns hive-mcp.addons.metrics
  "Addon lifecycle and addon tool dispatch timing, recorded through the
   hive-ttracking.metrics port, which is bound late by symbol.

   Keys (closed label vocabulary: addon ids, tool names, command names):
     [:addon/load <addon-id-or-init-ns> <phase>]   constructor resolve / build / init-ns load
     [:addon/init <addon-id>]                      addons.core/init-addon!
     [:addon/tool <addon-id> <tool or \"tool cmd\">] an addon-contributed handler call
     [:addon/invoke <tool-name>]                   the :tools/invoke port an addon calls

   With no port on the classpath every function here is a pass-through: the
   measured fn runs unchanged and nothing is recorded. A recorder failure never
   fails the measured call."
  (:require [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private record-sym
  'hive-ttracking.metrics/record!)

(defonce ^:private cache
  ^{:doc "nil (unresolved), ::absent, or the resolved record! var."}
  (atom nil))

(defn- resolve-recorder
  []
  (let [hit @cache]
    (cond
      (identical? hit ::absent) nil
      (some? hit)               hit
      :else (let [f (try (requiring-resolve record-sym) (catch Throwable _ nil))]
              (reset! cache (or f ::absent))
              f))))

(def ^:dynamic *recorder*
  "0-arg fn answering the record fn (k elapsed-ns outcome), or nil when no
   metrics port is present. Read per call; a test binds it to a stub."
  resolve-recorder)

(defn reset-cache!
  "Forget whether the metrics port resolved, so the next call asks again.
   Returns nil."
  []
  (reset! cache nil)
  nil)

(defn outcome
  "Pure. :error for an MCP error answer ({:isError true}) or a failed
   lifecycle answer ({:success? false}); :ok for anything else."
  [v]
  (if (and (map? v)
           (or (true? (:isError v)) (false? (:success? v))))
    :error
    :ok))

(defn- emit!
  [rec k t0 oc]
  (try (rec k (- (System/nanoTime) t0) oc)
       (catch Throwable _ nil)))

(defn timed-call
  "(F) measured under op key K. Records outcome :error when F throws (the
   throwable is rethrown) or answers what `outcome` calls an error, else :ok.
   Returns F's value. Without a recorder, just (F)."
  [k f]
  (if-let [rec (*recorder*)]
    (let [t0 (System/nanoTime)
          v  (try (f)
                  (catch Throwable t
                    (emit! rec k t0 :error)
                    (throw t)))]
      (emit! rec k t0 (outcome v))
      v)
    (f)))

(defn wrap-handler
  "HANDLER whose every call is measured under op key K (see `timed-call`).
   Answers HANDLER itself, unwrapped, when no recorder is present. A fn
   handler's metadata is carried onto the wrapper."
  [k handler]
  (if (*recorder*)
    (let [w (fn
              ([] (timed-call k #(handler)))
              ([a] (timed-call k #(handler a)))
              ([a b] (timed-call k #(handler a b)))
              ([a b & more] (timed-call k #(apply handler a b more))))]
      (if (and (instance? clojure.lang.IObj handler) (not (var? handler)) (meta handler))
        (with-meta w (meta handler))
        w))
    handler))

(defn label
  "Pure. X as a short label string: a keyword's name with its namespace, a
   symbol or string as-is, nil as \"-\"."
  [x]
  (cond
    (nil? x)     "-"
    (keyword? x) (subs (str x) 1)
    :else        (str x)))

;; Contracts

(def OpKey
  "An op key: [area op] or [area op sub]."
  [:vector {:min 2 :max 3} :any])

(m/=> reset-cache! [:=> [:cat] :nil])
(m/=> outcome [:=> [:cat :any] [:enum :ok :error]])
(m/=> timed-call [:=> [:cat OpKey ifn?] :any])
(m/=> wrap-handler [:=> [:cat OpKey ifn?] ifn?])
(m/=> label [:=> [:cat :any] :string])
