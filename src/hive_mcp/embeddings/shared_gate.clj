(ns hive-mcp.embeddings.shared-gate
  "One admission gate for the EmbeddingProvider port. Priority is chosen at
   request ingress, not inferred from provider type. The permit covers the
   actual provider HTTP call, including a full batch."
  (:require [hive-mcp.embeddings.protocol :as proto]))

(def ^:dynamic *lane*
  "Interactive memory adds bind :interactive; all other work uses :batch."
  :batch)

(defn next-lane
  "Pure admission policy; always serve a waiting interactive call first."
  [interactive batch]
  (cond (pos? interactive) :interactive
        (pos? batch) :batch
        :else nil))

(defn new-gate
  "Gate capacity and maximum wait in milliseconds. Admission is shared across
   all providers wrapped by the process-wide gate."
  [permits timeout-ms]
  (when-not (and (pos-int? permits) (pos-int? timeout-ms))
    (throw (ex-info "Embedding gate requires positive permits and timeout"
                    {:permits permits :timeout-ms timeout-ms})))
  {:permits permits :timeout-ms timeout-ms
   :lock (Object.)
   :active (atom 0)
   :waiting (atom {:interactive 0 :batch 0})})

(defonce process-gate (new-gate 1 12000))

(defn await-waiters
  "Test/diagnostic helper: wait until at least n calls are queued."
  [gate lane n timeout-ms]
  (let [end (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (if (>= (get @(:waiting gate) lane 0) n)
        true
        (if (< (System/currentTimeMillis) end)
          (do (Thread/sleep 2) (recur))
          false)))))

(defn try-acquire!
  "Admit a waiter only when capacity is free and no higher-priority lane waits.
   Waits at most `timeout-ms`; true when admitted, false when the wait ran out.
   A departing waiter wakes the others, including when it times out. A caller
   with its own deadline passes what that deadline has left, so a saturated
   gate cannot make the caller outlive it."
  [gate lane timeout-ms]
  (let [lock (:lock gate)
        end (+ (System/currentTimeMillis) (long timeout-ms))]
    (locking lock
      (swap! (:waiting gate) update lane inc)
      (try
        (loop []
          (let [remaining (- end (System/currentTimeMillis))
                waiting @(:waiting gate)]
            (cond
              (and (< @(:active gate) (:permits gate))
                   (= lane (next-lane (:interactive waiting) (:batch waiting))))
              (do (swap! (:active gate) inc) true)

              (<= remaining 0)
              false

              :else
              (do (.wait ^Object lock (long remaining)) (recur)))))
        (finally
          (swap! (:waiting gate) update lane dec)
          (.notifyAll ^Object lock))))))

(defn- acquire!
  "Admit within the gate's own timeout or throw :embedder/gate-timeout."
  [gate lane]
  (when-not (try-acquire! gate lane (:timeout-ms gate))
    (throw (ex-info "Embedding gate timed out"
                    {:error :embedder/gate-timeout :lane lane}))))

(defn release!
  "Return one permit taken by try-acquire! and wake every waiter."
  [gate]
  (locking (:lock gate)
    (swap! (:active gate) dec)
    (.notifyAll ^Object (:lock gate))))

(defn with-permit
  "Run an embedding call under the shared admission gate. Always release on
   timeout, 403, or any other provider exception."
  [gate lane call]
  (acquire! gate lane)
  (try (call)
       (finally (release! gate))))

(defprotocol AdmissionOwner
  "Marker for a provider that takes the shared permit itself, per attempt and
   bounded by its own deadline (the resilient failover chain). Decorating it
   would hold an outer permit while its inner attempts wait for another one:
   on a one-permit gate that is a self-deadlock until the deadline.")

(defrecord GatedProvider [delegate gate]
  proto/EmbeddingProvider
  (embed-text [_ text]
    (with-permit gate *lane* #(proto/embed-text delegate text)))
  (embed-batch [_ texts]
    (with-permit gate *lane* #(proto/embed-batch delegate texts)))
  (embedding-dimension [_]
    (proto/embedding-dimension delegate)))

(defn gated-provider
  "Decorate an EmbeddingProvider once at the registry/active-provider
   boundary; repeated decoration is idempotent to prevent nested permits.
   An AdmissionOwner is returned as is: it admits each attempt itself."
  ([provider] (gated-provider provider process-gate))
  ([provider gate]
   (cond
     (nil? provider) (throw (ex-info "Cannot gate a nil embedding provider" {}))
     (instance? GatedProvider provider) provider
     (satisfies? AdmissionOwner provider) provider
     :else (->GatedProvider provider gate))))

(defn gate-of
  "The gate admitting `provider`: its decorator's gate, else the process gate."
  [provider]
  (if (instance? GatedProvider provider) (:gate provider) process-gate))

(defn ungated
  "The undecorated provider, for a caller that already holds its permit."
  [provider]
  (if (instance? GatedProvider provider) (:delegate provider) provider))
