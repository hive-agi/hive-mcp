(ns hive-mcp.embeddings.shared-gate
  "One admission gate for the EmbeddingProvider port. Priority is chosen at
   request ingress, not inferred from provider type. The permit covers the
   actual provider HTTP call, including a full batch."
  (:require [hive-mcp.embeddings.protocol :as proto]
            [malli.core :as m]
            [hive-mcp.protocols.store-metrics :as metrics]))

(def ^:dynamic *lane*
  "Admission lane for gated embedding calls. MCP tool ingress binds
   :interactive (server.routes.middleware/wrap-handler-context); the binding
   is conveyed to futures and pools, so background work started from a
   request rebinds :batch (`batch-fn`, wrap-handler-async). Unbound: :batch."
  :batch)

(defn current-lane
  "The lane a gated call on this thread is admitted in."
  []
  *lane*)

(defn next-lane
  "Pure admission policy; always serve a waiting interactive call first."
  [interactive batch]
  (cond (pos? interactive) :interactive
        (pos? batch) :batch
        :else nil))

(def Lane
  "An admission lane."
  [:enum :interactive :batch])

;; `*lane*` is only ever bound to a Lane (ingress middleware, `batch-fn`,
;; `with-lane` callers); unbound it is :batch.
(m/=> current-lane [:=> [:cat] Lane])

(m/=> next-lane [:=> [:cat :int :int] [:maybe Lane]])

(defn new-gate
  "Gate capacity and maximum wait in milliseconds. Admission is shared across
   all providers wrapped by the process-wide gate. `:interactive-reserve` adds
   permits only the :interactive lane may take, so a held batch call can never
   leave an interactive caller without one."
  ([permits timeout-ms] (new-gate permits timeout-ms {}))
  ([permits timeout-ms {:keys [interactive-reserve] :or {interactive-reserve 0}}]
   (when-not (and (pos-int? permits) (pos-int? timeout-ms) (nat-int? interactive-reserve))
     (throw (ex-info "Embedding gate requires positive permits and timeout"
                     {:permits permits :timeout-ms timeout-ms
                      :interactive-reserve interactive-reserve})))
   {:permits permits :timeout-ms timeout-ms
    :interactive-reserve interactive-reserve
    :lock (Object.)
    :active (atom 0)
    :waiting (atom {:interactive 0 :batch 0})}))

(def Gate
  "A gate built by `new-gate`: capacity, maximum wait, its monitor and the
   live active/waiting counters. Generates fresh gates."
  [:and
   [:fn {:gen/schema [:tuple [:int {:min 1 :max 4}] [:int {:min 1 :max 50}]]
         :gen/fmap (fn [[p t]] (new-gate p t))}
    map?]
   [:map
    [:permits pos-int?]
    [:timeout-ms pos-int?]
    [:interactive-reserve {:optional true} nat-int?]
    [:lock some?]
    [:active [:fn #(instance? clojure.lang.IAtom %)]]
    [:waiting [:fn #(instance? clojure.lang.IAtom %)]]]])

;; Inputs admit anything: the body validates them and throws ex-info itself.
(m/=> new-gate [:function
                [:=> [:cat :any :any] Gate]
                [:=> [:cat :any :any [:map [:interactive-reserve {:optional true} :any]]] Gate]])

(defn lane-capacity
  "Permits `lane` may hold in `gate`: every lane shares `:permits`, and only
   :interactive may also take the reserve."
  [gate lane]
  (+ (:permits gate)
     (if (= :interactive lane) (get gate :interactive-reserve 0) 0)))

(m/=> lane-capacity [:=> [:cat Gate Lane] pos-int?])

(def default-permits
  "Process gate capacity when [:embeddings :gate :permits] is unset."
  1)

(def default-interactive-reserve
  "Process gate permits reserved for the :interactive lane when
   [:embeddings :gate :interactive-reserve] is unset."
  1)

(def default-timeout-ms
  "Longest a caller waits for a process-gate permit."
  12000)

(defn permits-from-config
  "Process gate capacity from a config map: [:embeddings :gate :permits] when
   it is a positive integer, else `default-permits`."
  [cfg]
  (let [n (get-in cfg [:embeddings :gate :permits])]
    (if (pos-int? n) n default-permits)))

;; Any config shape is admitted: get-in yields nil on a missing or malformed
;; path and the body falls back to `default-permits`.
(m/=> permits-from-config [:=> [:cat :any] pos-int?])

(defn reserve-from-config
  "Interactive reserve from a config map: [:embeddings :gate
   :interactive-reserve] when it is a natural number, else
   `default-interactive-reserve`. 0 restores plain one-lane capacity."
  [cfg]
  (let [n (get-in cfg [:embeddings :gate :interactive-reserve])]
    (if (nat-int? n) n default-interactive-reserve)))

(m/=> reserve-from-config [:=> [:cat :any] nat-int?])

(defn- global-config
  "The global config, resolved lazily so this namespace does not load the
   config subsystem; nil on any failure."
  []
  (try ((requiring-resolve 'hive-mcp.config.core/get-global-config))
       (catch Throwable _ nil)))

(defonce process-gate
  (let [cfg (global-config)]
    (new-gate (permits-from-config cfg) default-timeout-ms
              {:interactive-reserve (reserve-from-config cfg)})))
(defn batch-fn
  "Wrap `f` so it runs in the :batch lane on whatever thread executes it.
   Background work started from a request thread (futures and pools convey
   bindings) uses this so it never competes with the interactive lane."
  [f]
  (fn [& args] (binding [*lane* :batch] (apply f args))))

;; `f` may be any callable; the wrapper is a variadic fn applying it.
(m/=> batch-fn [:=> [:cat ifn?] ifn?])

(defmacro with-lane
  "Evaluate body with `*lane*` bound to `lane`."
  [lane & body]
  `(binding [*lane* ~lane] ~@body))

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

(m/=> await-waiters [:=> [:cat Gate Lane :int :int] :boolean])

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
              (and (< @(:active gate) (lane-capacity gate lane))
                   (= lane (next-lane (:interactive waiting) (:batch waiting))))
              (do (swap! (:active gate) inc) true)

              (<= remaining 0)
              false

              :else
              (do (.wait ^Object lock (long remaining)) (recur)))))
        (finally
          (swap! (:waiting gate) update lane dec)
          (.notifyAll ^Object lock))))))

(m/=> try-acquire! [:=> [:cat Gate Lane :int] :boolean])

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

;; `locking` returns the value of .notifyAll, which is nil.
(m/=> release! [:=> [:cat Gate] :nil])

(defn with-permit
  "Run an embedding call under the shared admission gate. Always release on
   timeout, 403, or any other provider exception."
  [gate lane call]
  (acquire! gate lane)
  (try (call)
       (finally (release! gate))))

;; The result is whatever the wrapped provider call returns (a vector, a batch
;; of vectors), so the output is open by design.
(m/=> with-permit [:=> [:cat Gate Lane [:=> [:cat] :any]] :any])

(defprotocol AdmissionOwner
  "Marker for a provider that takes the shared permit itself, per attempt and
   bounded by its own deadline (the resilient failover chain). Decorating it
   would hold an outer permit while its inner attempts wait for another one:
   on a one-permit gate that is a self-deadlock until the deadline.")

(defrecord GatedProvider [delegate gate]
  proto/EmbeddingProvider
  (embed-text [_ text]
    (with-permit gate *lane*
      #(metrics/timed (metrics/embed-key delegate :embed-text)
                      (proto/embed-text delegate text))))
  (embed-batch [_ texts]
    (with-permit gate *lane*
      #(metrics/timed (metrics/embed-key delegate :embed-batch)
                      (proto/embed-batch delegate texts))))
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

;; A nil provider is admitted at the boundary; the body rejects it itself.
(m/=> gated-provider [:function
                      [:=> [:cat [:maybe proto/Provider]] proto/Provider]
                      [:=> [:cat [:maybe proto/Provider] Gate] proto/Provider]])

(defn gate-of
  "The gate admitting `provider`: its decorator's gate, else the process gate."
  [provider]
  (if (instance? GatedProvider provider) (:gate provider) process-gate))

(m/=> gate-of [:=> [:cat [:maybe proto/Provider]] Gate])

(defn ungated
  "The undecorated provider, for a caller that already holds its permit."
  [provider]
  (if (instance? GatedProvider provider) (:delegate provider) provider))

(m/=> ungated [:=> [:cat [:maybe proto/Provider]] [:maybe proto/Provider]])
