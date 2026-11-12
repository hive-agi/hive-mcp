(ns hive-mcp.protocols.lifecycle
  "IShutdownHook / ISweepable / IResourceOwner / IShutdownBudget — `def`
   aliases of hive-spi.lifecycle.ports.

   The protocols themselves live in hive-spi so addons can implement them
   without compile-depending on hive-mcp. Every historical
   hive-mcp.protocols.lifecycle/* qualified name still resolves here.
   `satisfies?` must be called on the ports vars, never on these aliases:
   an alias holds a snapshot, so an impl added later via extend-protocol
   would be invisible to it."
  (:require [hive-spi.lifecycle.ports :as ports]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Protocols are `def` aliases; METHODS are defn delegates. A method alias
;; captures the fn at load time and misses any impl registered later by
;; map-based `extend` (kanban 20260917114708-1178ca7a).

(def IShutdownHook ports/IShutdownHook)
(defn shutdown-priority "Delegates to `ports/shutdown-priority`." [hook]
  (ports/shutdown-priority hook))
(defn shutdown-name "Delegates to `ports/shutdown-name`." [hook]
  (ports/shutdown-name hook))
(defn shutdown! "Delegates to `ports/shutdown!`." [hook ctx]
  (ports/shutdown! hook ctx))

(def ISweepable ports/ISweepable)
(defn sweep-interval-s "Delegates to `ports/sweep-interval-s`." [sweeper]
  (ports/sweep-interval-s sweeper))
(defn sweep-name "Delegates to `ports/sweep-name`." [sweeper]
  (ports/sweep-name sweeper))
(defn sweep! "Delegates to `ports/sweep!`." [sweeper ctx]
  (ports/sweep! sweeper ctx))

(def IResourceOwner ports/IResourceOwner)
(defn owner-id "Delegates to `ports/owner-id`." [owner]
  (ports/owner-id owner))
(defn owned-resources "Delegates to `ports/owned-resources`." [owner]
  (ports/owned-resources owner))
(defn release-all! "Delegates to `ports/release-all!`." [owner]
  (ports/release-all! owner))

(def IShutdownBudget ports/IShutdownBudget)
(defn shutdown-timeout-ms "Delegates to `ports/shutdown-timeout-ms`." [budget]
  (ports/shutdown-timeout-ms budget))

;; Port-method delegates: the participant input is loose (protocol dispatch
;; rejects a non-implementer); answers are the ports' documented types.
(m/=> shutdown-priority [:=> [:cat :any] :int])
(m/=> shutdown-name [:=> [:cat :any] :string])
(m/=> shutdown! [:=> [:cat :any [:map]] :any])
(m/=> sweep-interval-s [:=> [:cat :any] :int])
(m/=> sweep-name [:=> [:cat :any] :string])
(m/=> sweep! [:=> [:cat :any [:map]] :any])
(m/=> owner-id [:=> [:cat :any] :any])
(m/=> owned-resources [:=> [:cat :any] [:or [:map] [:sequential :any]]])
(m/=> release-all! [:=> [:cat :any] :any])
(m/=> shutdown-timeout-ms [:=> [:cat :any] :int])
