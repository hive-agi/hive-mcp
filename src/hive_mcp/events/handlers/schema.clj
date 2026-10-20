(ns hive-mcp.events.handlers.schema
  "Value-object schemas shared by the event handler modules under
   hive-mcp.events.handlers: the argument list every handler is called with,
   the :log effect the handlers emit, and the registry a registration answers.

   Event and coeffect shapes themselves belong to hive-mcp.events.schemas;
   this namespace only composes them for the handler contracts."
  (:require [hive-mcp.events.schemas :as es]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def HandlerArgs
  "What an event handler is called with: the coeffects map (a direct call may
   pass nil or {}) and the event vector [event-id payload?]."
  [:cat [:maybe es/Coeffects] es/Event])

(def LogEffect
  "The :log effect the handlers in this tree emit: a level and a message."
  [:map
   [:level [:enum :debug :info :warn :error]]
   [:message :string]])

(def HandlerRegistry
  "What hive-mcp.events.core/reg-event answers, and so what a registration
   that ends in one answers: the live registry, event id -> entry map."
  [:map-of :keyword :map])
