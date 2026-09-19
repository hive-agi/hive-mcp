(ns hive-mcp.spi.catchup-registry
  "The injection point for catchup blocks.

   A catchup answer is composed from BLOCKS. Every contributor, the host's
   own domains and addons alike, registers one:

     {:block/id    kw           the key its value lands under
      :block/fn    (fn [ctx])   returns the block value (a map)
      :block/order int          lower runs first}

   The host never names a contributor: it calls `compose` with a context
   and receives every registered block's value keyed by id.

   HOST-LOCAL: the kernel's copy of the contribution registry. It moves to
   hive-spi (ns hive-spi.catchup.registry) once that library releases it;
   until then the kernel cannot require an unreleased coordinate."
  (:require [hive-spi.slot :as slot]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Block
  "malli schema for a registered block."
  [:map
   [:block/id :keyword]
   [:block/fn fn?]
   [:block/order :int]])

(defn block?
  "True iff X has the Block shape."
  [x]
  (and (map? x)
       (keyword? (:block/id x))
       (fn? (:block/fn x))
       (integer? (:block/order x))))

(defonce ^:private block-slot
  (slot/multi-slot {:validate block?}))

(defn register-block!
  "Install BLOCK under its :block/id, replacing any block already there.
   Returns BLOCK. Throws when BLOCK does not have the Block shape."
  [block]
  (slot/reg-put! block-slot (:block/id block) block))

(defn unregister-block!
  "Remove the block under ID. No-op when absent. Returns nil."
  [id]
  (slot/reg-remove! block-slot id))

(defn registered-blocks
  "Every registered block, sorted by :block/order then :block/id."
  []
  (->> (vals (slot/reg-snapshot block-slot))
       (sort-by (juxt :block/order :block/id))
       vec))

(defn registered?
  "True iff a block is installed under ID."
  [id]
  (some? (slot/reg-get block-slot id)))

(defn reset-registry!
  "Remove every registered block. Returns nil."
  []
  (slot/reg-clear! block-slot))

(defn compose
  "Run every registered block against CTX in :block/order.

   Returns {:blocks {id value} :failed {id message}}. A block that throws
   lands in :failed with the exception message and never aborts the others;
   a block that returns nil lands in :blocks as nil."
  [ctx]
  (reduce (fn [acc {:block/keys [id fn]}]
            (try
              (assoc-in acc [:blocks id] (fn ctx))
              (catch Throwable t
                (assoc-in acc [:failed id] (or (ex-message t) (str (class t)))))))
          {:blocks {} :failed {}}
          (registered-blocks)))
