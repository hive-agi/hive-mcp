(ns hive-mcp.tools.catchup.block-cache
  "Process-wide cache for the parts of catchup that are not the memory bundle:
   contributed blocks (hive-mcp.spi.catchup-registry), status providers
   (:catchup/status-providers) and the context-store refs of a bundle.

   The bundle has its own cache (`bundle-cache`). Before this namespace, every
   catchup still re-ran the kanban block (five board scans) and the carto
   status provider, each 2-3.5 s on the live store, once per agent. Ten agents
   catching up on one directory paid that ten times.

   An entry is keyed by the caller and served under a POLICY:

     {:fresh-ms   n     age below which the value is served as is
      :max-age-ms n     age below which a stale value is served while ONE
                        background refresh runs; at or past it, cold
      :drop-tags  #{s}} a memory write carrying any of these tags drops the
                        entry before `write-events/notify!` returns

   A cold entry is computed under single-flight, so a herd of concurrent
   callers shares one computation. A value is only served for the memory
   store instance it was computed against (weak reference, as in
   `bundle-cache`). A computation that throws is never stored.

   `now-ms` is the clock; tests pin it with with-redefs."
  (:require [hive-mcp.events.write-events :as write-events]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.catchup.bundle-cache :as bundle-cache]
            [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.dns.result :refer [rescue rescue-log]]
            [clojure.tools.logging :as log])
  (:import [java.lang.ref WeakReference]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Knobs
;; =============================================================================

(def default-block-policy
  "Policy for a block that declares :block/cache without its own numbers."
  {:fresh-ms (* 30 1000) :max-age-ms (* 5 60 1000)})

(def status-policy
  "Policy for every :catchup/status-providers entry. A provider takes only the
   project-id, so its value is the same for every caller on that project."
  {:fresh-ms (* 60 1000) :max-age-ms (* 10 60 1000)})

(defn now-ms
  "Clock. with-redefs this in tests."
  []
  (System/currentTimeMillis))

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private entries    (atom {}))
(defonce ^:private refreshing (atom #{}))
(defonce ^:private counters   (atom {:hit 0 :stale 0 :miss 0 :dropped 0}))

(defn- count! [k] (swap! counters update k (fnil inc 0)))

(defn stats
  "Diagnostic snapshot: counters plus entry count."
  []
  (assoc @counters
         :entries (count @entries)
         :refreshing (count @refreshing)))

(defn reset-cache!
  "Drop every entry and the counters. For tests."
  []
  (reset! entries {})
  (reset! refreshing #{})
  (reset! counters {:hit 0 :stale 0 :miss 0 :dropped 0})
  nil)

;; =============================================================================
;; Core
;; =============================================================================

(defn- current-store []
  (rescue nil (when (mem-proto/store-set?) (mem-proto/get-store))))

(defn- same-store? [entry store]
  (identical? store (some-> ^WeakReference (:store-ref entry) .get)))

(defn- compute-and-store!
  [key policy compute-fn]
  (let [store (current-store)
        v     (compute-fn)]
    (swap! entries assoc key {:value     v
                              :stored-at (now-ms)
                              :store-ref (WeakReference. store)
                              :drop-tags (set (:drop-tags policy))})
    v))

(defn- refresh-in-background!
  [key policy compute-fn]
  (let [[old new] (swap-vals! refreshing conj key)]
    (when (not= old new)
      (future
        (try
          (bundle-cache/single-flight [::entry key] #(compute-and-store! key policy compute-fn))
          (catch Throwable t
            (log/warn t "catchup block-cache: background refresh failed for" key))
          (finally
            (swap! refreshing disj key)))))))

(declare ensure-subscribed!)

(defn cached
  "The value for `key`, computing it with `compute-fn` (0-arity) when the cache
   holds nothing usable for the current store. See the ns doc for `policy`.

     fresh  (age < :fresh-ms)                  -> cached value
     stale  (:fresh-ms <= age < :max-age-ms)   -> cached value; refresh in background
     cold   (absent, other store, too old)     -> compute under single-flight"
  [key policy compute-fn]
  (ensure-subscribed!)
  (let [{:keys [fresh-ms max-age-ms]} policy
        store (current-store)
        hit   (let [e (get @entries key)]
                (when (and e (same-store? e store)) e))
        age   (when hit (- (now-ms) (:stored-at hit)))]
    (cond
      (and hit (< age fresh-ms))
      (do (count! :hit) (:value hit))

      (and hit (< age max-age-ms))
      (do (count! :stale)
          (refresh-in-background! key policy compute-fn)
          (:value hit))

      :else
      (do (count! :miss)
          (bundle-cache/single-flight [::entry key] #(compute-and-store! key policy compute-fn))))))

;; =============================================================================
;; Blocks and status providers
;; =============================================================================

(defn block-policy
  "The cache policy BLOCK declares, or nil when it opts out. A block opts in
   with :block/cache true (default policy) or a policy map."
  [block]
  (let [c (:block/cache block)]
    (cond
      (true? c) default-block-policy
      (map? c)  (merge default-block-policy c)
      :else     nil)))

(defn block-key
  "Cache key for BLOCK under CTX: the block id plus the ctx values named by
   the policy's :key-by (default [:project-id :directory]). :caller-id belongs
   in :key-by only for a block whose value depends on who asks."
  [block policy ctx]
  (into [:block (:block/id block)]
        (map #(get ctx %))
        (or (:key-by policy) [:project-id :directory])))

(defn run-block
  "Run BLOCK against CTX, through the cache when the block opts in."
  [block ctx]
  (if-let [policy (block-policy block)]
    (cached (block-key block policy ctx) policy #((:block/fn block) ctx))
    ((:block/fn block) ctx)))

(defn run-status-provider
  "Run status provider K (fn [project-id]) for PROJECT-ID through the cache."
  [k provider-fn project-id]
  (cached [:status k project-id] status-policy #(provider-fn project-id)))

;; =============================================================================
;; Context-store refs
;; =============================================================================

(defonce ^:private shared-refs
  ^{:doc "{scope -> {:bundle <identical bundle value> :refs {category ctx-id}}}"}
  (atom {}))

(defn shared-context-refs
  "Context-store refs for BUNDLE under SCOPE. When BUNDLE is the identical
   value the previous caller stored (a bundle-cache hit) and every ref is
   still live, those refs are returned; otherwise `put-fn` (0-arity, returns
   {category ctx-id}) writes a fresh set. N agents served one cached bundle
   share one set of context entries instead of writing N copies."
  [scope bundle put-fn]
  (let [{prev-bundle :bundle prev-refs :refs} (get @shared-refs scope)]
    (if (and (some? bundle)
             (identical? prev-bundle bundle)
             (seq prev-refs)
             (every? #(some? (context-store/context-get %)) (vals prev-refs)))
      (do (count! :refs-hit) prev-refs)
      (let [refs (put-fn)]
        (when (seq refs)
          (swap! shared-refs assoc scope {:bundle bundle :refs refs}))
        refs))))

;; =============================================================================
;; Invalidation
;; =============================================================================

(defn invalidate!
  "Apply one write {:op ... :tags ...}: drop every entry whose :drop-tags meet
   the write's tags. Returns nil."
  [{:keys [tags]}]
  (let [tags (set tags)]
    (when (seq tags)
      (let [[old new] (swap-vals! entries
                                  (fn [m]
                                    (into {}
                                          (remove (fn [[_ e]] (some tags (:drop-tags e))))
                                          m)))
            n (- (count old) (count new))]
        (when (pos? n)
          (swap! counters update :dropped + n)))))
  nil)

(defn evict-stale!
  "Drop entries older than the longest max-age any policy uses. Returns the
   number evicted."
  []
  (let [limit (max (:max-age-ms default-block-policy) (:max-age-ms status-policy))
        now   (now-ms)
        [old new] (swap-vals! entries
                              #(into {} (filter (fn [[_ e]] (< (- now (:stored-at e)) limit))) %))]
    (- (count old) (count new))))

(defn ensure-subscribed!
  "Register `invalidate!` as a write-events listener. Idempotent; never throws."
  []
  (rescue-log "catchup block-cache: register listener" nil
    (write-events/register-listener! ::invalidate invalidate!))
  nil)
