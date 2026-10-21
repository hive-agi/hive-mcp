(ns hive-mcp.spi.catchup-registry
  "DEPRECATED shim over hive-spi.catchup.registry. Require that instead.

   The catchup block registry lived here, host-local, until hive-spi released
   it (1.4.0). Core now composes catchup from the library registry. This shim
   stays for one release because addons (hive-git's :forge-inbox,
   hive-knowledge's host catchup) still soft-resolve
   `hive-mcp.spi.catchup-registry/register-block!`. It keeps NO block table of
   its own: every fn forwards to the library, so a block registered here is a
   block catchup composes.

   `compose` keeps the 2-arity (ctx, run) the host used for its block cache;
   hive-spi's compose has only the 1-arity, so the runner arity is built here
   over the library's `registered-blocks`."
  (:require [hive-spi.catchup.registry :as registry]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Block
  "malli schema for a registered block.

   :block/cache (optional) opts the block into the host's shared cache:
   true for the default policy, or a policy map
   {:fresh-ms n :max-age-ms n :drop-tags #{tag} :key-by [ctx-key ...]}."
  [:map
   [:block/id :keyword]
   [:block/fn fn?]
   [:block/order :int]
   [:block/cache {:optional true} [:or :boolean :map]]])

(defn block?
  "See hive-spi.catchup.registry/block?."
  [x]
  (registry/block? x))

(defn register-block!
  "See hive-spi.catchup.registry/register-block!."
  [block]
  (registry/register-block! block))

(defn unregister-block!
  "See hive-spi.catchup.registry/unregister-block!."
  [id]
  (registry/unregister-block! id))

(defn registered-blocks
  "See hive-spi.catchup.registry/registered-blocks."
  []
  (registry/registered-blocks))

(defn registered?
  "See hive-spi.catchup.registry/registered?."
  [id]
  (registry/registered? id))

(defn reset-registry!
  "See hive-spi.catchup.registry/reset-registry!."
  []
  (registry/reset-registry!))

(defn compose
  "Run every registered block against CTX in :block/order.

   Returns {:blocks {id value} :failed {id message}}. A block that throws
   lands in :failed with the exception message and never aborts the others.
   RUN (fn [block ctx]) is how one block is executed; the default calls its
   :block/fn."
  ([ctx]
   (registry/compose ctx))
  ([ctx run]
   (reduce (fn [acc {:block/keys [id] :as block}]
             (try
               (assoc-in acc [:blocks id] (run block ctx))
               (catch Throwable t
                 (assoc-in acc [:failed id] (or (ex-message t) (str (class t)))))))
           {:blocks {} :failed {}}
           (registry/registered-blocks))))

;; =============================================================================
;; Contracts
;; =============================================================================

(def BlockValue
  "A registered block, as `register-block!` accepts and `registered-blocks`
   answers it: `Block` with a generating :block/fn."
  [:map
   [:block/id :keyword]
   [:block/fn [:fn {:error/message "should be a fn"
                    :gen/schema :nil
                    :gen/fmap (fn [_] (fn [_ctx] {}))}
               fn?]]
   [:block/order :int]
   [:block/cache {:optional true} [:or :boolean :map]]])

(def Runner
  "How `compose` runs one block: (fn [block ctx])."
  [:fn {:error/message "should be callable"
        :gen/schema :nil
        :gen/fmap (fn [_] (fn [block ctx] ((:block/fn block) ctx)))}
   ifn?])

(def Composed
  [:map {:closed true}
   [:blocks [:map-of :keyword :any]]
   [:failed [:map-of :keyword :string]]])

(m/=> block? [:=> [:cat :any] :boolean])
(m/=> register-block! [:=> [:cat BlockValue] BlockValue])
(m/=> unregister-block! [:=> [:cat :any] :nil])
(m/=> registered-blocks [:=> [:cat] [:vector BlockValue]])
(m/=> registered? [:=> [:cat :any] :boolean])
(m/=> reset-registry! [:=> [:cat] :nil])
(m/=> compose [:function
               [:=> [:cat :any] Composed]
               [:=> [:cat :any Runner] Composed]])
