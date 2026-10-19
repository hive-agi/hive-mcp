(ns hive-mcp.events.context

  "Pure context manipulation helpers for the event system.

   Context shape: {:coeffects {...} :effects {...} :queue [...] :stack [...]}

   Zero-dependency utilities used by interceptors and handlers to read
   and write the :coeffects and :effects maps."
  (:require [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn get-coeffect
  "Get a coeffect value from context."
  ([context]
   (:coeffects context))
  ([context key]
   (get-in context [:coeffects key]))
  ([context key not-found]
   (get-in context [:coeffects key] not-found)))

(m/=> get-coeffect [:function [:=> [:cat :map] [:maybe :map]] [:=> [:cat :map :keyword] :any] [:=> [:cat :map :keyword :any] :any]])

(defn assoc-coeffect
  "Associate a coeffect value in context."
  [context key value]
  (assoc-in context [:coeffects key] value))

(m/=> assoc-coeffect [:=> [:cat :map :keyword :any] :map])

(defn update-coeffect
  "Update a coeffect value in context."
  [context key f & args]
  (apply update-in context [:coeffects key] f args))

(m/=> update-coeffect [:=> [:cat :map :keyword ifn? [:* :any]] :map])

(defn get-effect
  "Get an effect value from context."
  ([context]
   (:effects context))
  ([context key]
   (get-in context [:effects key]))
  ([context key not-found]
   (get-in context [:effects key] not-found)))

(m/=> get-effect [:function [:=> [:cat :map] [:maybe :map]] [:=> [:cat :map :keyword] :any] [:=> [:cat :map :keyword :any] :any]])

(defn assoc-effect
  "Associate an effect value in context."
  [context key value]
  (assoc-in context [:effects key] value))

(m/=> assoc-effect [:=> [:cat :map :keyword :any] :map])

(defn update-effect
  "Update an effect value in context."
  [context key f & args]
  (apply update-in context [:effects key] f args))

(m/=> update-effect [:=> [:cat :map :keyword ifn? [:* :any]] :map])
