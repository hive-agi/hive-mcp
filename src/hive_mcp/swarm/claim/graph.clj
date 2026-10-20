(ns hive-mcp.swarm.claim.graph
  "The `callers-fn` that `hive-mcp.swarm.claim.span` asks for, backed by carto.

   Compat shim: moved to hive-agent.swarm.claim.graph in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.claim.graph" sym))

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

(def Callers
  "Qualified names of the forms that call a qn, as `callers` answers them."
  [:vector :string])

(def CallersFn
  "What `callers-fn` hands out: qn -> its callers."
  [:=> [:cat :string] Callers])

(defn available?
  {:arglists '([])}
  [& args]
  (apply (impl! 'available?) args))

(m/=> available? [:=> [:cat] :boolean])

(defn callers
  {:arglists '([scope qn])}
  [& args]
  (apply (impl! 'callers) args))

;; scope is a carto scope id, nil for the default index; [] on any failure
(m/=> callers [:=> [:cat [:maybe :string] :string] Callers])

(defn callers-fn
  {:arglists '([scope])}
  [& args]
  (apply (impl! 'callers-fn) args))

;; nil, not a fn answering [], when carto is absent: span/overlap tells them apart
(m/=> callers-fn [:=> [:cat [:maybe :string]] [:maybe CallersFn]])

(defn reset-cache!
  {:arglists '([])}
  [& args]
  (apply (impl! 'reset-cache!) args))

;; answers the emptied cache, i.e. what reset! returns
(m/=> reset-cache! [:=> [:cat] [:map-of :any :any]])
