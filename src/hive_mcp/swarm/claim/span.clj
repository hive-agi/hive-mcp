(ns hive-mcp.swarm.claim.span
  "What a swarm agent claims, narrowed from a file to a span of one.

   Compat shim: moved to hive-agent.swarm.claim.span in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.claim.span" sym))

(defn- impl!
  "impl, after making sure the host adapters fill the swarm port slots.
   The adapters are resolved late: a static require would close a load
   cycle through the events adapter. A failure here (for instance a call
   made while that cycle is still loading) leaves the ports on their
   noops until a later call installs them."
  [sym]
  (try ((requiring-resolve 'hive-mcp.swarm.adapters.boot/ensure!))
       (catch Throwable _ nil))
  (impl sym))

(def Span @(impl 'Span))

(defn conflicts
  {:arglists '([callers-fn held wanted slave-id])}
  [& args]
  (apply (impl! 'conflicts) args))

(defn conflicts?
  {:arglists '([callers-fn a b])}
  [& args]
  (apply (impl! 'conflicts?) args))

(defn explain
  {:arglists '([{:keys [file qn held-by reason caller]}])}
  [& args]
  (apply (impl! 'explain) args))

(defn file-span
  {:arglists '([file])}
  [& args]
  (apply (impl! 'file-span) args))

(defn form-span
  {:arglists '([file qn] [file qn mode])}
  [& args]
  (apply (impl! 'form-span) args))

(defn key-of
  {:arglists '([s])}
  [& args]
  (apply (impl! 'key-of) args))

(def mode-rank @(impl 'mode-rank))

(def modes @(impl 'modes))

(defn overlap
  {:arglists '([callers-fn a b])}
  [& args]
  (apply (impl! 'overlap) args))

(defn span
  {:arglists '([x])}
  [& args]
  (apply (impl! 'span) args))

(defn widen
  {:arglists '([a b])}
  [& args]
  (apply (impl! 'widen) args))
