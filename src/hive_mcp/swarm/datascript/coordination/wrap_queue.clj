(ns hive-mcp.swarm.datascript.coordination.wrap-queue
  "Ling wrap notification queue for coordinator permeation.

   Compat shim: moved to hive-agent.swarm.datascript.coordination.wrap-queue in
   hive-agent. Every public var delegates there; see hive-mcp.swarm.delegate for
   behavior without the addon."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [hive-mcp.swarm.datascript.schema :as s]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-datascript.swarm.coordination.wrap-queue" sym))

(defn add-wrap-notification! {:arglists '([wrap-id {:keys [agent-id session-id project-id created-ids stats parent-session-id depth]}])} [& args] (apply (impl 'add-wrap-notification!) args))

;; Contracts transcribe hive-datascript.swarm.coordination.wrap-queue, where
;; the behaviour lives.
(m/=> add-wrap-notification!
      [:=> [:cat :string [:maybe [:map [:agent-id {:optional true} [:maybe :string]]
                                  [:session-id {:optional true} [:maybe :string]]
                                  [:project-id {:optional true} [:maybe :string]]
                                  [:created-ids {:optional true} [:maybe s/StringColl]]
                                  [:stats {:optional true} [:maybe :map]]
                                  [:parent-session-id {:optional true} [:maybe :string]]
                                  [:depth {:optional true} [:maybe :int]]]]]
       s/TxReport])
(defn get-unprocessed-wraps {:arglists '([])} [& args] (apply (impl 'get-unprocessed-wraps) args))

(m/=> get-unprocessed-wraps [:=> [:cat] [:sequential s/WrapRow]])
(defn get-unprocessed-wraps-for-hierarchy {:arglists '([project-id-prefix])} [& args] (apply (impl 'get-unprocessed-wraps-for-hierarchy) args))

(m/=> get-unprocessed-wraps-for-hierarchy [:=> [:cat [:maybe :string]] [:sequential s/WrapRow]])
(defn get-unprocessed-wraps-for-project {:arglists '([project-id])} [& args] (apply (impl 'get-unprocessed-wraps-for-project) args))

(m/=> get-unprocessed-wraps-for-project [:=> [:cat [:maybe :string]] [:sequential s/WrapRow]])
(defn get-unprocessed-wraps-for-session {:arglists '([parent-session-id])} [& args] (apply (impl 'get-unprocessed-wraps-for-session) args))

;; nil for a nil session, else that session's wraps.
(m/=> get-unprocessed-wraps-for-session [:=> [:cat [:maybe :string]] [:maybe [:sequential s/WrapRow]]])
(defn mark-wrap-processed! {:arglists '([wrap-id])} [& args] (apply (impl 'mark-wrap-processed!) args))

(m/=> mark-wrap-processed! [:=> [:cat s/EntityId] s/MaybeTxReport])
