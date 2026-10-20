(ns hive-mcp.swarm.ledger.default
  "Process-wide default swarm ledger, opened lazily on first append.

   Compat shim: moved to hive-agent.swarm.ledger.default in hive-agent.
   Every public var delegates there; see hive-mcp.swarm.delegate for behavior
   without the addon."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [hive-spi.swarm.ledger :as ledger]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.ledger.default" sym))

(defn append! {:arglists '([event])} [& args] (apply (impl 'append!) args))

;; The swarm ledger port (hive-spi.swarm.ledger/ILedgerStore). The generator
;; is a recording reify, so the schema holds without the datalevin backend.
(def LedgerStore
  [:fn {:gen/schema :nil
        :gen/fmap (fn [_] (reify ledger/ILedgerStore
                            (append! [_ e] (assoc e :seq 1))
                            (close! [_] nil)))}
   (fn [x] (satisfies? ledger/ILedgerStore x))])

;; Contracts transcribe hive-agent.swarm.ledger.default, where the behaviour
;; lives. append! never throws: nil when the ledger is unavailable or the
;; append failed.
(m/=> append! [:=> [:cat :map] [:maybe :map]])
(defn reset-store! {:arglists '([])} [& args] (apply (impl 'reset-store!) args))

;; Ends on (reset! opened? false).
(m/=> reset-store! [:=> [:cat] [:= false]])
(defn set-store! {:arglists '([s])} [& args] (apply (impl 'set-store!) args))

;; Ends on (reset! opened? true).
(m/=> set-store! [:=> [:cat [:maybe LedgerStore]] [:= true]])
(defn store {:arglists '([])} [& args] (apply (impl 'store) args))

(m/=> store [:=> [:cat] [:maybe LedgerStore]])
