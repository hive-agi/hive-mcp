(ns hive-mcp.tools.kg.batch
  "Wave 3 / T13 Phase 3 — Batchable runner for the consolidated `kg` tool.

   Same shape as `hive-mcp.tools.memory.batch`: wraps
   `hive-mcp.batch/make-default-runner` with a kg-specific
   `:resolve-handler` over `hive-mcp.tools.kg/handlers`.

   The consolidated kg ns is resolved lazily via `requiring-resolve` so
   this batch ns loads even when deeper kg deps (datahike / datalevin
   backends) aren't available. Contract tests bind their own resolver.

   Purely additive. Public MCP entry-point `handle-kg` is untouched."
  (:require [clojure.string :as str]
            [hive-mcp.batch :as batch]
            [hive-mcp.batch.protocol :as proto]
            [hive-mcp.dns.result :refer [rescue]]
            [hive-mcp.tools.cli :as cli]
            [hive-mcp.dispatch.handler :as dispatch]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- lazy-canonical-handlers
  []
  (or (rescue nil
              (when-let [v (requiring-resolve
                            'hive-mcp.tools.kg/handlers)]
                @v))
      {}))

(defn- resolve-kg-handler
  [handlers tool-name]
  (when (and tool-name (not (str/blank? (str tool-name))))
    (let [canonical (str/replace (str tool-name) #"\." " ")
          path      (cli/parse-command canonical)
          resolved  (cli/resolve-handler handlers path)]
      (when (dispatch/handler? (:handler resolved))
        (:handler resolved)))))

(defn- default-emit-fx
  [fx-id fx-data]
  (rescue nil
          (when-let [get-fx (requiring-resolve 'hive.events.fx/get-fx)]
            (when-let [h (get-fx fx-id)]
              (h fx-data)))))

(defrecord KGBatchRunner [handlers delegate]
  proto/Batchable
  (batch-execute [_this ops opts]
    (proto/batch-execute delegate ops opts))
  (batch-schema [_this]
    (proto/batch-schema delegate)))

(def KGRunner
  "A `KGBatchRunner` (satisfies `Batchable`)."
  [:fn {:gen/elements [(->KGBatchRunner {} nil)]
        :error/message "must be a hive-mcp.tools.kg.batch.KGBatchRunner"}
   #(instance? KGBatchRunner %)])

(def RunnerOpts
  "`make-kg-runner` opts: a handler tree and an fx emitter, both optional."
  [:maybe
   [:map
    [:handlers {:optional true} [:maybe [:map-of {:gen/max 0} :any :any]]]
    [:emit-fx {:optional true} [:maybe [:fn {:gen/elements [(fn [_ _] nil)]} ifn?]]]]])

(defn make-kg-runner
  "Factory — build a `KGBatchRunner` satisfying `Batchable`."
  ([] (make-kg-runner {}))
  ([{:keys [handlers emit-fx]}]
   (let [hs       (or handlers (lazy-canonical-handlers))
         resolver (partial resolve-kg-handler hs)
         delegate (batch/make-default-runner
                   {:resolve-handler resolver
                    :emit-fx         (or emit-fx default-emit-fx)})]
     (->KGBatchRunner hs delegate))))

(m/=> make-kg-runner [:function [:=> [:cat] KGRunner] [:=> [:cat RunnerOpts] KGRunner]])

;; =============================================================================
;; Public convenience shim — mirrors `hive-mcp.tools.multi/run-multi`
;; =============================================================================

(def ^:private default-runner
  "Lazy shared Batchable runner. Built on first use."
  (delay (make-kg-runner)))

(defn run-batch
  "Execute a vector of kg ops through the `Batchable` protocol.

   `ops` is a vector of `{:id :tool :command ...}` maps where `:tool`
   is a kg command (e.g. `\"edge\"`, `\"traverse\"`, `\"impact\"`).
   Returns the standard Batchable result shape."
  ([ops] (run-batch ops {}))
  ([ops opts]
   (proto/batch-execute @default-runner ops opts)))

(m/=> run-batch
  [:function
   [:=> [:cat [:sequential batch/RawOp]] batch/RunResult]
   [:=> [:cat [:sequential batch/RawOp] [:maybe [:map [:dry-run? {:optional true} [:maybe :boolean]]]]]
    batch/RunResult]])
