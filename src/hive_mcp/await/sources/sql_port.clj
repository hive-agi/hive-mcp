(ns hive-mcp.await.sources.sql-port
  "The SQL adapter port of the :sql-predicate await source (DIP).

   Core owns the port and the source; it has NO database driver and no SQL
   dialect. An addon contributes an adapter at its IAddon initialize! and
   retracts it at shutdown!. The Postgres one is the hive.await-sql addon
   (repo hive-await-sql). With none registered, an :sql-predicate await is
   refused at create, before anything is recorded, naming the addon.

   An adapter is plain data, so the addon needs no compile-time dependency
   on hive-mcp (it registers through requiring-resolve):

     {:id             \"postgres\"            ; registry key; one per dialect
      :addon          \"hive.await-sql\"      ; who registered it
      :check-and-bind (fn [sql params] ...)  ; DIALECT: refuse anything that
                                             ; is not one read-only query, or
                                             ; answer {:sql s :params [..]} in
                                             ; the adapter's own placeholder form
      :run-select!    (fn [request] ...)}    ; TRANSPORT: run one bound query
                                             ; read-only; answer {:rows [..]
                                             ; :truncated? bool}; throw on failure

   run-select! gets {:secret s :sql s :params [..] :row-cap n
   :statement-timeout-ms n}. :secret is the resolved connection secret; an
   adapter must never put it, nor a bound value, into an error message.

   Selection: the subscription's :dialect names an adapter id; without one,
   the only registered adapter is used, and with several the request must
   name one. *adapter* overrides the registry (the test seam)."
  (:require [clojure.string :as str]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-addon
  "The addon that contributes the Postgres adapter."
  "hive.await-sql")

(def Adapter
  [:map
   [:id [:string {:min 1}]]
   [:addon {:optional true} [:maybe :string]]
   [:check-and-bind [:fn ifn?]]
   [:run-select! [:fn ifn?]]])

(defonce ^:private adapters (atom {}))

(def ^:dynamic *adapter*
  "An adapter that overrides the registry, or nil (the test seam)."
  nil)

(defn register-adapter!
  "Register `adapter` (see Adapter) under its :id, replacing an earlier one.
   Answers the id."
  [adapter]
  (when-let [e (m/explain Adapter adapter)]
    (throw (ex-info "await sql-port: invalid adapter"
                    {:error :await/invalid-adapter :explain (pr-str (mapv :path (:errors e)))})))
  (swap! adapters assoc (:id adapter) adapter)
  (:id adapter))

(m/=> register-adapter! [:=> [:cat Adapter] :string])

(defn unregister-adapter!
  "Retract the adapter `id`. Answers the ids still registered."
  [id]
  (vec (sort (keys (swap! adapters dissoc (str id))))))

(m/=> unregister-adapter! [:=> [:cat :any] [:vector :string]])

(defn registered
  "The ids of every registered adapter."
  []
  (vec (sort (keys @adapters))))

(m/=> registered [:=> [:cat] [:vector :string]])

(defn missing
  "The refusal for a missing adapter. `dialect` is the id asked for, or nil."
  [dialect]
  (ex-info (str "await: source sql-predicate needs addon " default-addon
                " (no SQL adapter registered"
                (when dialect (str " for dialect " (pr-str dialect))) ")")
           {:error :await/source-adapter-missing :source :sql-predicate
            :port :sql-runner :dialect dialect :registered (registered)
            :hint (str "load addon " default-addon)}))

(defn adapter-for
  "The adapter for `dialect` (an id, or nil for the only one), or throw the
   :await/source-adapter-missing refusal."
  [dialect]
  (or *adapter*
      (let [all @adapters
            d   (some-> dialect name str/trim not-empty)]
        (cond
          d                (or (get all d) (throw (missing d)))
          (= 1 (count all)) (val (first all))
          (empty? all)     (throw (missing nil))
          :else (throw (ex-info (str "await sql-predicate: several SQL adapters are registered ("
                                     (str/join ", " (sort (keys all))) "); name one with :dialect")
                                {:error :await/invalid-subscription :source :sql-predicate
                                 :registered (vec (sort (keys all)))}))))))

(m/=> adapter-for [:=> [:cat :any] Adapter])
