(ns hive-mcp.knowledge-graph.tx-hooks
  "Addon seam over every KG write: hooks that see a transaction's tx-data
   before it reaches the store, and may rewrite it.

   A hook is (fn [tx-data] tx-data'): it returns the tx-data to transact, or
   throws to refuse the whole transaction. Hooks run in id order, on the
   caller's thread, so a hook can read the request context of the call that
   wrote. With no hook registered, tx-data passes through untouched.

   Both connection/transact! and connection/transact-sync! apply them, so a
   hook sees edges written through any path (add-edge(s)!, batch, events,
   promotion), not only through one function."
  (:require [taoensso.timbre :as log]))

(defonce ^:private hooks (atom (sorted-map)))

(defn register!
  "Register (or replace) hook `f` under keyword `id`. `f` may be a var, so a
   re-evaluated hook applies at once. Returns id."
  [id f]
  {:pre [(keyword? id) (ifn? f)]}
  (swap! hooks assoc id f)
  (log/info "kg tx-hook registered:" id)
  id)

(defn unregister!
  "Remove hook `id`. Safe when absent. Returns id."
  [id]
  (swap! hooks dissoc id)
  id)

(defn registered
  "Registered hook ids, in run order."
  []
  (vec (keys @hooks)))

(defn apply-hooks
  "`tx-data` through every registered hook, in id order. A hook that throws
   refuses the transaction: the exception reaches the writer."
  [tx-data]
  (reduce (fn [tx [_ f]] (f tx)) tx-data @hooks))
