(ns hive-mcp.server.request-offload
  "Moves MCP request handlers off jsonrpc4clj's read loop.

   jsonrpc4clj 1.0.2 (ChanServer) calls every request method on ONE
   thread-loop and waits for its return value before it reads the next
   request or notification. A tool call that blocks (a long ingest verb, a KG
   flush, a stuck embedding) therefore holds every later message on that
   connection, $/cancelRequest included. Measured 2026-10-23: a stub blocked
   on a promise kept a second, instant request unanswered for the full 2 s
   probe window.

   jsonrpc4clj wraps whatever a method returns in a promesa promise. A method
   that hands back a CompletableFuture frees the read loop at once, and the
   response is written when the future completes, under the request's own
   JSON-RPC id. This namespace wraps the request methods so that they do this:

   - The wrapper is installed in the SDK method table, but it offloads only when
     the context carries an executor under ::executor. The stdio context gets
     one. The HTTP transport's context does not, so its synchronous
     IRpcDispatch is unaffected.
   - \"initialize\" is never wrapped. It runs on the read loop, so nothing read
     after it can start before it has answered.
   - At most `request-concurrency` handlers run at once (a fixed pool). Excess
     requests queue in the executor, and the read loop keeps reading.
   - $/cancelRequest and MCP notifications/cancelled cancel the future. That
     interrupts the worker running the handler and answers with
     RequestCancelled. The handler also sees the
     :jsonrpc4clj.server/req-cancelled? atom flip.

   Composition root: hive-mcp.system.layer5 (:hive/mcp-stdio)."
  (:require [jsonrpc4clj.server :as jsonrpc-server]
            [promesa.core :as p]
            [malli.core :as m]
            [taoensso.timbre :as log])
  (:import [java.util.concurrent CompletableFuture ExecutorService Future
            LinkedBlockingQueue ThreadFactory ThreadPoolExecutor TimeUnit
            CancellationException]
           [java.util.concurrent.atomic AtomicLong]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-concurrency
  "Handlers one connection may run at once when [:server :request-concurrency]
   is unset."
  8)

(def sync-methods
  "Request methods that stay on the read loop. initialize has to answer before
   anything read after it starts. tools/list is a cheap table read, and
   hive-mcp.server.registration owns its method by var (tools-list-installed?
   compares against that var), so it is left alone."
  #{"initialize" "tools/list"})

(def Concurrency [:int {:min 1 :max 1024}])

(defn concurrency
  "Pure. The pool size: CONFIGURED when it is a positive int (at most 1024),
   otherwise `default-concurrency`."
  [configured]
  (if (and (int? configured) (<= 1 configured 1024))
    configured
    default-concurrency))

(m/=> concurrency [:=> [:cat :any] Concurrency])

(defn- thread-factory ^ThreadFactory [prefix]
  (let [n (AtomicLong.)]
    (reify ThreadFactory
      (newThread [_ r]
        (doto (Thread. ^Runnable r (str prefix "-" (.incrementAndGet n)))
          (.setDaemon true))))))

(defn executor
  "A fixed pool of N daemon threads with an unbounded queue: at most N handlers
   run at once, and the rest wait their turn without blocking the read loop."
  ^ExecutorService [n]
  (ThreadPoolExecutor. (int n) (int n) 60 TimeUnit/SECONDS
                       (LinkedBlockingQueue.)
                       (thread-factory "mcp-request")))

(m/=> executor [:=> [:cat Concurrency] [:fn #(instance? ExecutorService %)]])

(defn submit
  "Run (F) on EXEC. Returns a CompletableFuture with F's value or throwable.
   Cancelling the future interrupts the worker running F."
  ^CompletableFuture [^ExecutorService exec f]
  (let [cf   (CompletableFuture.)
        task (.submit exec ^Runnable
                      (fn []
                        (when-not (.isDone cf)
                          (try (.complete cf (f))
                               (catch Throwable t
                                 (.completeExceptionally cf t))))))]
    (.whenComplete cf (reify java.util.function.BiConsumer
                        (accept [_ _ e]
                          (when (instance? CancellationException e)
                            (.cancel ^Future task true)))))
    cf))

(m/=> submit [:=> [:cat [:fn #(instance? ExecutorService %)] ifn?] [:fn #(instance? CompletableFuture %)]])

;; ── Method-table wrapping ─────────────────────────────────────────────────

(defn offloaded?
  "True when F is a wrapper this namespace installed."
  [f]
  (boolean (::orig (meta f))))

(m/=> offloaded? [:=> [:cat :any] :boolean])

(defn wrap-method
  "Wrap the request-method fn ORIG: with ::executor in the context, the call
   runs on that executor and the wrapper returns its CompletableFuture.
   Without one, ORIG runs inline as before. Dynamic bindings, including the
   read loop's discarded *out*, travel with the call. Idempotent."
  [orig]
  (if (offloaded? orig)
    orig
    (with-meta
      (fn [method context params]
        (if-let [exec (::executor context)]
          (submit exec (bound-fn [] (orig method context params)))
          (orig method context params)))
      {::orig orig})))

(m/=> wrap-method [:=> [:cat ifn?] ifn?])

(defn offload!
  "Wrap the receive-request method for METHOD in place. Returns true when it
   is wrapped afterwards, nil when there is no such method."
  [method]
  (when-let [orig (get (methods jsonrpc-server/receive-request) method)]
    (.addMethod ^clojure.lang.MultiFn jsonrpc-server/receive-request method (wrap-method orig))
    true))

(m/=> offload! [:=> [:cat :any] [:maybe [:= true]]])

(declare receive-cancelled)

(defn install!
  "Wrap every registered request method except :default and `sync-methods`,
   and install the MCP notifications/cancelled handler. Idempotent. Returns
   the wrapped method names."
  []
  (let [names (->> (keys (methods jsonrpc-server/receive-request))
                   (remove #(or (= :default %) (contains? sync-methods %)))
                   sort
                   vec)]
    (doseq [mname names] (offload! mname))
    (.addMethod ^clojure.lang.MultiFn jsonrpc-server/receive-notification
                "notifications/cancelled" #'receive-cancelled)
    names))

(m/=> install! [:=> [:cat] [:vector :any]])

;; ── Cancellation ───────────────────────────────────────────────────────────

(defn cancel-pending!
  "Cancel SERVER's pending received request with ID, the same way
   $/cancelRequest does. Returns true when one was pending."
  [server id]
  (if-let [pending (some-> server :pending-received-requests* deref (get id))]
    (do (p/cancel! pending) true)
    false))

(m/=> cancel-pending! [:=> [:cat :any :any] :boolean])

(defn receive-cancelled
  "MCP notifications/cancelled {:requestId id}: cancel that request if it is
   still running. The SDK's own method was a no-op."
  [_method context params]
  (when-let [id (:requestId params)]
    (when (cancel-pending! (:server context) id)
      (log/debug "MCP request cancelled by client" id)))
  nil)

(m/=> receive-cancelled [:=> [:cat :any :any [:maybe map?]] :nil])

;; ── Lifecycle ──────────────────────────────────────────────────────────────

(defn attach
  "CONTEXT with a fresh bounded executor of size (concurrency CONFIGURED).
   Calls `install!` first."
  [context configured]
  (let [n (concurrency configured)]
    (install!)
    (log/info "MCP requests offloaded from the read loop, concurrency" n)
    (assoc context ::executor (executor n) ::concurrency n)))

(m/=> attach [:=> [:cat [:maybe map?] :any] map?])

(defn detach!
  "Shut the executor in CONTEXT down, interrupting the handlers still running."
  [context]
  (some-> ^ExecutorService (::executor context) .shutdownNow)
  nil)

(m/=> detach! [:=> [:cat [:maybe map?]] :nil])
