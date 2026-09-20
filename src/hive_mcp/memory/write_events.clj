(ns hive-mcp.memory.write-events
  "DEPRECATED facade. The write-event vocabulary is kernel code and lives in
   `hive-mcp.events.write-events`; this name call-throughs to it and dies with
   the hive-memory extraction.

   Every fn delegates through the kernel VAR (`(defn f [x] (we/f x))`), never
   `(def f we/f)`: a def-alias captures the fn value at load, so a hot reload
   or `with-redefs` of the kernel var would not reach callers of this name.
   See memory 20260919135346-7838bb58."
  (:require [hive-mcp.events.write-events :as we]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def op->channel-type
  "Write op -> channel event :type. Closed set.
   See `hive-mcp.events.write-events/op->channel-type`."
  we/op->channel-type)

(def channel-type->op
  "Channel event :type -> write op.
   See `hive-mcp.events.write-events/channel-type->op`."
  we/channel-type->op)

(def channel-types
  "Every channel event :type a write can produce, in op order.
   See `hive-mcp.events.write-events/channel-types`."
  we/channel-types)

(defn ->event
  "See `hive-mcp.events.write-events/->event`."
  [op payload]
  (we/->event op payload))

(defn event->write
  "See `hive-mcp.events.write-events/event->write`."
  [event]
  (we/event->write event))

(defn register-listener!
  "See `hive-mcp.events.write-events/register-listener!`."
  [key f]
  (we/register-listener! key f))

(defn unregister-listener!
  "See `hive-mcp.events.write-events/unregister-listener!`."
  [key]
  (we/unregister-listener! key))

(defn listener-keys
  "See `hive-mcp.events.write-events/listener-keys`."
  []
  (we/listener-keys))

(defn notify!
  "See `hive-mcp.events.write-events/notify!`."
  [op payload]
  (we/notify! op payload))
