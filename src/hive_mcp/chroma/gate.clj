(ns hive-mcp.chroma.gate
  "Concurrency gates for ChromaDB reads/writes and the shared embed port.

   ChromaDB uses SQLite internally: read (4 permits) and write (1 permit)
   derefs retain their hive-weave gates. Embeddings use the ONE process-wide
   provider gate from embeddings.shared-gate, never a Chroma-only pool.
   Do not nest with-embedding-gate around a decorated provider call."
  (:require [hive-weave.gate :as g]
            [hive-mcp.embeddings.shared-gate :as shared]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Gate Instances
;; =============================================================================

(defonce read-gate
  (g/gate {:permits 4 :timeout-ms 15000 :name "chroma-read"}))

(defonce write-gate
  (g/gate {:permits 1 :timeout-ms 30000 :name "chroma-write"}))

;; =============================================================================
;; Convenience API — drop-in replacements for bare @ derefs
;; =============================================================================

(def Pending
  "A deref-able vendor result (promise, future, deferred). Generates a
   realized promise."
  [:fn {:gen/schema :any :gen/fmap #(doto (promise) (deliver %))}
   #(instance? clojure.lang.IBlockingDeref %)])

(def GateStats
  "Diagnostic snapshot of the read, write and embed gates."
  [:map
   [:read :map]
   [:write :map]
   [:embed [:map
            [:name :string]
            [:permits :int]
            [:available :int]
            [:queue-length [:int {:min 0}]]]]])

(defn deref-read
  "Deref a Chroma read promise with concurrency gate + timeout.
   Replaces: @(chroma/query ...) or @(chroma/get ...)"
  ([promise]
   (g/deref-gate read-gate promise))
  ([promise timeout-ms]
   (g/deref-gate read-gate promise timeout-ms)))

(m/=> deref-read [:function
                  [:=> [:cat Pending] :any]
                  [:=> [:cat Pending [:int {:min 0}]] :any]])

(defn deref-write
  "Deref a Chroma write promise with exclusive gate + timeout.
   Replaces: @(chroma/add ...) or @(chroma/delete ...)"
  ([promise]
   (g/deref-gate write-gate promise))
  ([promise timeout-ms]
   (g/deref-gate write-gate promise timeout-ms)))

(m/=> deref-write [:function
                   [:=> [:cat Pending] :any]
                   [:=> [:cat Pending [:int {:min 0}]] :any]])

(defmacro with-embedding-gate
  "Legacy macro for callers outside the EmbeddingProvider port. Never wrap a
   decorated provider call with it: admission there already owns the permit."
  [& body]
  `(shared/with-permit shared/process-gate shared/*lane* (fn [] ~@body)))

;; =============================================================================
;; Diagnostics
;; =============================================================================

(defn gate-stats
  "Current state of Chroma read/write and the one shared embedding provider gate."
  []
  (let [embed shared/process-gate
        active @(:active embed)
        waiting @(:waiting embed)]
    {:read (g/gate-stats read-gate)
     :write (g/gate-stats write-gate)
     :embed {:name "embedding-provider"
             :permits (:permits embed)
             :available (- (:permits embed) active)
             :queue-length (reduce + (vals waiting))}}))

(m/=> gate-stats [:=> [:cat] GateStats])
