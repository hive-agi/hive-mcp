(ns hive-mcp.await.sources.boundary
  "Wake bounded context: the I/O boundary every polling await source talks
   through, as protocols (DIP). A source never calls the clock, a thread
   pool, a database, a process or an HTTP client directly. It holds an
   adapter that implements one of these protocols. The production adapters
   live in hive-mcp.await.adapters.*, and a test passes stubs with `binding`
   around create!/open-source (no with-redefs).

   Every protocol is deliberately narrow: it does one thing at the boundary
   and answers plain data, so a source's decisions stay pure.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol IPollScheduler
  (schedule-repeating! [scheduler f interval-ms]
    "Run the zero-arg `f` now and then every `interval-ms` until cancelled.
     Answer a zero-arg cancel fn. `f` never throws (the poll engine catches)."))

(defprotocol IClock
  (now-ms [clock] "The current time, epoch ms."))

(defprotocol ISecretResolver
  (resolve-secret! [resolver ref]
    "The secret value named by `ref` (a string <scheme>:<id>, e.g.
     \"k8s:sisf/staging-pg/url\" or \"vault:<row-id>\"), or throw ex-info
     {:error :await/secret-missing}. The value is never logged, never
     journaled and never put in an event. See await.sources.secret-refs."))

;; The SQL adapter port is data, not a protocol, so an addon can contribute
;; one without a compile-time dependency on hive-mcp: see
;; hive-mcp.await.sources.sql-port.

(defprotocol IProcessRunner
  (run-process! [runner argv opts]
    "Run `argv` (a vector of strings, NO shell) with opts
     {:timeout-ms n :max-bytes n}. Answer {:exit n :out s :err s
     :timed-out? bool :truncated? bool}. Throw only when it cannot start."))

(defprotocol IHttpGet
  (http-get! [client url query-params opts]
    "GET `url` with `query-params` (a map of strings) and opts
     {:timeout-ms n}. Answer {:status n :body s}. Throw on transport
     failure."))
