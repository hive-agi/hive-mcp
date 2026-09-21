(ns hive-mcp.spi.kanban.registry
  "The DIP swap point for the kanban ports, plus a facade over the registered
   providers.

   A provider registers itself with `register!`; consumers call the facade fns
   (`list-tasks`, `get-task`, `transition!`, `create-task!`) and never name a
   provider namespace. With nothing registered every facade fn answers from
   the Noops, so a consumer loads and runs with no provider on the classpath:
   reads are empty, writes are a {:err ...} value.

   The facade resolves the provider on EVERY call, so a later registration
   (an addon at initialize!, a test fixture) is what the next call answers
   from. A captured value would freeze the seam.

   HOST-LOCAL: the kernel's copy of hive-spi.kanban.registry.
   See hive-mcp.spi.kanban."
  (:require [hive-mcp.spi.kanban :as kanban]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Degraded defaults
;; =============================================================================

(defn- unavailable
  [port]
  {:error   :kanban/provider-unavailable
   :message (str "no provider registered for " port)})

(defrecord NoopKanbanRead []
  kanban/IKanbanRead
  ;; An empty board, not an envelope: list results are iterated by callers.
  (list-tasks [_ _query] [])
  (get-task [_ _id] nil))

(defrecord NoopKanbanWrite []
  kanban/IKanbanWrite
  (transition! [_ _request] {:err (unavailable :IKanbanWrite)})
  (create-task! [_ _request] {:err (unavailable :IKanbanWrite)}))

(def noops
  "Port key -> Noop instance. The registry's fallback table."
  {:IKanbanRead  (->NoopKanbanRead)
   :IKanbanWrite (->NoopKanbanWrite)})

(def port->protocol
  "Port key -> the protocol an implementation must satisfy."
  {:IKanbanRead  kanban/IKanbanRead
   :IKanbanWrite kanban/IKanbanWrite})

(defn noop?
  "True when `impl` is a degraded default for either kanban port."
  [impl]
  (contains? (set (vals noops)) impl))

;; =============================================================================
;; Registry
;; =============================================================================

(defonce ^:private -providers (atom {}))

(defonce ^:private -warned (atom #{}))

(defn register!
  "Install `impl` as the provider for `port`. Returns `impl`.
   Throws when `impl` does not satisfy the port's protocol — a wiring error
   must fail loudly at startup, unlike a missing provider which degrades."
  [port impl]
  (if-let [proto (get port->protocol port)]
    (if (satisfies? proto impl)
      (do (swap! -providers assoc port impl)
          (swap! -warned disj port)
          impl)
      (throw (ex-info "impl does not satisfy the port protocol"
                      {:port port :impl (type impl)})))
    (throw (ex-info "unknown kanban port" {:port port :known (keys port->protocol)}))))

(defn unregister!
  "Drop the provider for `port`, reverting it to its Noop default."
  [port]
  (swap! -providers dissoc port)
  nil)

(defn reset-providers!
  "Drop every provider. Test-support."
  []
  (reset! -providers {})
  (reset! -warned #{})
  nil)

(defn registered?
  "True when a non-Noop provider is installed for `port`."
  [port]
  (contains? @-providers port))

(defn provider
  "The active provider for `port` — a registered impl, else the Noop default.
   Warns once per port on first degraded use."
  [port]
  (or (get @-providers port)
      (do (when-not (contains? @-warned port)
            (swap! -warned conj port)
            (log/warn "kanban port degraded, no provider registered:" port))
          (get noops port))))

;; =============================================================================
;; Facade
;; =============================================================================

(defn list-tasks
  "See hive-mcp.spi.kanban/IKanbanRead."
  [query]
  (kanban/list-tasks (provider :IKanbanRead) query))

(defn get-task
  "See hive-mcp.spi.kanban/IKanbanRead."
  [id]
  (kanban/get-task (provider :IKanbanRead) id))

(defn transition!
  "See hive-mcp.spi.kanban/IKanbanWrite."
  [request]
  (kanban/transition! (provider :IKanbanWrite) request))

(defn create-task!
  "See hive-mcp.spi.kanban/IKanbanWrite."
  [request]
  (kanban/create-task! (provider :IKanbanWrite) request))
