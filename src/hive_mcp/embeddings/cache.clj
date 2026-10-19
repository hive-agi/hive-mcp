(ns hive-mcp.embeddings.cache
  "In-process LRU+TTL cache for embedding vectors.

   Keyed by [collection-name text-hash] → {:vec [...] :expires-at ms}.
   Backed by java.util.LinkedHashMap in access-order. Synchronized
   via locking — embedding calls are already slow (100ms+), so the
   coarse lock is free.

   Eviction: LRU when size exceeds cap, lazy TTL check on lookup."
  (:import [java.util LinkedHashMap])
  (:require [malli.core :as m]
            [hive-mcp.embeddings.protocol :as proto]))

(def ^:private default-cap 2048)
(def ^:private default-ttl-ms (* 5 60 1000))

(defn- make-lru
  ^LinkedHashMap [cap]
  (proxy [LinkedHashMap] [16 0.75 true]
    (removeEldestEntry [_entry]
      (> (.size ^LinkedHashMap this) cap))))

(defonce ^:private state
  (atom {:map      (make-lru default-cap)
         :cap      default-cap
         :ttl-ms   default-ttl-ms
         :hits     0
         :misses   0
         :evicts   0}))

(defn- text-hash
  [^String text]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")
        bs (.digest md (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" %) bs))))

(def CacheStats
  "What `stats` reports: occupancy, configuration and hit accounting."
  [:map
   [:size [:int {:min 0}]]
   [:cap pos-int?]
   [:ttl-ms :int]
   [:hits [:int {:min 0}]]
   [:misses [:int {:min 0}]]
   [:evicts [:int {:min 0}]]
   [:hit-ratio [:double {:min 0.0 :max 1.0}]]])

(def ResetState
  "The cache state right after `clear!` or `configure!`: counters at zero."
  [:map
   [:cap pos-int?]
   [:ttl-ms :int]
   [:hits [:= 0]]
   [:misses [:= 0]]
   [:evicts [:= 0]]])

(defn cache-key
  "Cache identity. With a `space-id` (hive-mcp.embeddings.space/space-id) the
   key is collection + space + text hash, so a vector from one model, digest or
   serializer revision can never answer for another. Without one it is the
   legacy collection + text hash."
  ([collection-name text]
   (str collection-name ":" (text-hash text)))
  ([collection-name space-id text]
   (if space-id
     (str collection-name ":" space-id ":" (text-hash text))
     (cache-key collection-name text))))

;; `str` tolerates a nil collection or space id, so both are admitted.
(m/=> cache-key [:function
                 [:=> [:cat [:maybe :string] :string] :string]
                 [:=> [:cat [:maybe :string] [:maybe :string] :string] :string]])

(defn lookup
  "Return cached embedding vector or nil. Lazy-evicts expired entries.
   The 3-arity scopes the lookup to an embedding space id."
  ([collection-name text] (lookup collection-name nil text))
  ([collection-name space-id text]
   (let [k (cache-key collection-name space-id text)
         {:keys [^LinkedHashMap map ttl-ms]} @state]
     (locking map
       (if-let [entry (.get map k)]
         (if (< (System/currentTimeMillis) (:expires-at entry))
           (do (swap! state update :hits inc)
               (:vec entry))
           (do (.remove map k)
               (swap! state (fn [s] (-> s (update :evicts inc) (update :misses inc))))
               nil))
         (do (swap! state update :misses inc)
             nil))))))

(m/=> lookup [:function
              [:=> [:cat [:maybe :string] :string] [:maybe proto/Vector]]
              [:=> [:cat [:maybe :string] [:maybe :string] :string] [:maybe proto/Vector]]])

(defn store!
  "Insert a fresh embedding into the cache. The 4-arity scopes the entry to an
   embedding space id."
  ([collection-name text embedding] (store! collection-name nil text embedding))
  ([collection-name space-id text embedding]
   (let [k (cache-key collection-name space-id text)
         {:keys [^LinkedHashMap map ttl-ms]} @state
         expires (+ (System/currentTimeMillis) ttl-ms)]
     (locking map
       (.put map k {:vec embedding :expires-at expires}))
     embedding)))

(m/=> store! [:function
              [:=> [:cat [:maybe :string] :string proto/Vector] proto/Vector]
              [:=> [:cat [:maybe :string] [:maybe :string] :string proto/Vector] proto/Vector]])

(defn stats
  []
  (let [{:keys [^LinkedHashMap map hits misses evicts cap ttl-ms]} @state
        total (+ hits misses)]
    {:size    (.size map)
     :cap     cap
     :ttl-ms  ttl-ms
     :hits    hits
     :misses  misses
     :evicts  evicts
     :hit-ratio (if (pos? total) (double (/ hits total)) 0.0)}))

(m/=> stats [:=> [:cat] CacheStats])

(defn clear!
  []
  (let [^LinkedHashMap m (:map @state)]
    (locking m (.clear m))
    (swap! state assoc :hits 0 :misses 0 :evicts 0)))

(m/=> clear! [:=> [:cat] ResetState])

(defn configure!
  "Rebuild cache with new cap/ttl. Clears existing entries."
  [{:keys [cap ttl-ms]}]
  (let [cap' (or cap default-cap)
        ttl' (or ttl-ms default-ttl-ms)]
    (swap! state assoc
           :map (make-lru cap')
           :cap cap'
           :ttl-ms ttl'
           :hits 0 :misses 0 :evicts 0)))

(m/=> configure! [:=> [:cat [:map
                             [:cap {:optional true} [:maybe pos-int?]]
                             [:ttl-ms {:optional true} [:maybe :int]]]]
                  ResetState])
