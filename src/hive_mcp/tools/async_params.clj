(ns hive-mcp.tools.async-params
  "The `async` and `async-timeout-ms` params every tool's schema declares.

   Every tool's middleware chain runs `wrap-handler-async`, and an MCP client
   strips an UNDECLARED boolean false in transit, so a tool that does not
   declare `async` can never receive `async:false`. Both surfaces that
   advertise tools apply `with-async-params`: the server's own table
   (`server.routes/make-tool`) and the registry surface external loaders
   read (`tools.registry/get-advertised-tools`, `get-consolidated-tools`)."
  (:require [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private async-result-note
  (str "A queued call returns {:queued true :task-id ...} at once; its result "
       "arrives in a ---TOOLRESULT--- block on a later hive call."))

(def ^:private async-opt-out-property
  {"async" {:type "boolean"
            :description (str "Set false to force synchronous execution and get the "
                              "full result in-band (default: this tool's commands may "
                              "queue and return {:queued true :task-id ...}). "
                              "Set true to force queueing. " async-result-note)}})

(def ^:private async-opt-in-property
  {"async" {:type "boolean"
            :description (str "Set true to run in the background so the call does not "
                              "block the agent loop. " async-result-note)}})

(def ^:private async-timeout-property
  {"async-timeout-ms" {:type "integer"
                       :description "With async:true, cancel the queued call after this many ms."}})

(def DefaultAsyncCommands
  "A tool's :default-async-commands: any collection of command names, only
   ever tested for emptiness here, or nil."
  [:maybe [:or [:set :any] [:sequential :any]]])

(def AsyncProps
  "The two schema properties `async-props` answers, string keyed."
  [:map {:closed true}
   ["async" [:map [:type [:= "boolean"]] [:description [:string {:min 1}]]]]
   ["async-timeout-ms" [:map [:type [:= "integer"]] [:description [:string {:min 1}]]]]])

(def ToolDef
  "A tool map as `with-async-params` reads it: its default-async command set
   and the :properties of its :inputSchema, both optional."
  [:maybe [:map
           [:default-async-commands {:optional true} DefaultAsyncCommands]
           [:inputSchema {:optional true}
            [:maybe [:map [:properties {:optional true} [:maybe :map]]]]]]])

(def AsyncToolDef
  "A tool map whose :inputSchema :properties declare both async params (the
   tool's own `async`, when it had one, is kept as it was)."
  [:map [:inputSchema [:map [:properties [:map ["async" :any] ["async-timeout-ms" :any]]]]]])

(defn async-props
  "Schema properties for a tool whose default-async command set is
   DEFAULT-ASYNC-COMMANDS: opt-out wording when it queues anything by default,
   opt-in wording otherwise."
  [default-async-commands]
  (merge (if (seq default-async-commands)
           async-opt-out-property
           async-opt-in-property)
         async-timeout-property))

(m/=> async-props [:=> [:cat DefaultAsyncCommands] AsyncProps])

(defn with-async-params
  "TOOL with the async params merged under its :inputSchema properties. A tool
   that declares its own `async` keeps it. Idempotent."
  [{:keys [default-async-commands] :as tool}]
  (update-in tool [:inputSchema :properties]
             #(merge (async-props default-async-commands) %)))

(m/=> with-async-params [:=> [:cat ToolDef] AsyncToolDef])

(defn async-flag
  "The caller's `async` value as a boolean, or nil when unset. A client that
   was not told the param's type may send it as the string \"false\", which
   is truthy; read it as the boolean it spells."
  [v]
  (cond
    (nil? v)            nil
    (boolean? v)        v
    (string? v)         (case (.toLowerCase ^String (.trim ^String v))
                          ("false" "0" "no" "") false
                          true)
    :else               (boolean v)))

;; any wire value is read: nil stays unset, everything else is a boolean
(m/=> async-flag [:=> [:cat :any] [:maybe :boolean]])
