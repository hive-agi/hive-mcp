(ns hive-mcp.protocols.store-metrics
  "Latency/frequency metering for the storage ports: memory, KG, vector
   collections and embedding providers.

   Observations reach `hive-ttracking.metrics/record!`, bound late. When that
   namespace is absent, `enabled?` is false, `wrap-store` returns its target
   unchanged and `timed` only evaluates its body.

   Op keys: [:store/<kind> <backend> <method>] and [:embed/<provider> <op>],
   each element a keyword derived from a class name or a protocol method.

   A metered store is a reify built once per [kind, target class]: it
   implements exactly the port protocols the target satisfies, forwards every
   method and records it. It exposes only those protocols (plus
   hive-spi IStoreDecorator for memory stores): keyword lookups and
   `instance?` checks on the concrete record do not see through it. `unwrap`
   returns the target."
  (:require [clojure.string :as str]
            [hive-spi.memory.decorate :as decorate]
            [hive-spi.memory.ports]
            [hive-spi.kg.protocol]
            [malli.core :as m])
  (:import [java.util.concurrent ConcurrentHashMap]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; Late-bound recorder
;;; ---------------------------------------------------------------------------

(defonce ^:private recorder (volatile! ::unresolved))

(defn- resolve-recorder
  "The hive-ttracking.metrics/record! var, or nil when it is not loadable."
  []
  (try (requiring-resolve 'hive-ttracking.metrics/record!)
       (catch Throwable _ nil)))

(defn- current-recorder
  "The recorder (a fn of k elapsed-ns outcome) or nil. Resolves once."
  []
  (let [r @recorder]
    (if (identical? r ::unresolved)
      (let [v (resolve-recorder)]
        (vreset! recorder v)
        v)
      r)))

(defn set-recorder!
  "Install F, a fn of [k elapsed-ns outcome], as the recorder (nil disables
   metering). Returns F. Composition and test seam."
  [f]
  (vreset! recorder f)
  f)

(defn rebind!
  "Drop the cached recorder so the next call resolves
   hive-ttracking.metrics/record! again. Returns nil."
  []
  (vreset! recorder ::unresolved)
  nil)

(defn enabled?
  "True when a recorder is bound."
  []
  (some? (current-recorder)))

(defn observe!
  "Record one observation through the recorder. Never throws. Returns nil."
  [k elapsed-ns outcome]
  (when-let [f (current-recorder)]
    (try (f k elapsed-ns outcome)
         (catch Throwable _ nil)))
  nil)

(defmacro timed
  "Evaluate body; record [k elapsed-ns outcome], outcome :error when body
   throws (rethrown), else :ok. Returns body's value."
  [k & body]
  `(let [t0# (System/nanoTime)]
     (try
       (let [r# (do ~@body)]
         (observe! ~k (- (System/nanoTime) t0#) :ok)
         r#)
       (catch Throwable t#
         (observe! ~k (- (System/nanoTime) t0#) :error)
         (throw t#)))))

;;; ---------------------------------------------------------------------------
;;; Labels
;;; ---------------------------------------------------------------------------

(defn label-of-name
  "Keyword label for a class simple name: kebab-case with a trailing
   Store/Provider/Embedder (and a preceding Memory/KG/Vector/Collection)
   stripped. \"DatahikeKGStore\" -> :datahike."
  [simple-name]
  (let [s    (str simple-name)
        base (str/replace s #"(Memory|KG|Vector|Collection)?(Store|Provider|Embedder|Embeddings)$" "")
        base (if (str/blank? base) s base)]
    (-> base
        (str/replace #"([a-z0-9])([A-Z])" "$1-$2")
        str/lower-case
        keyword)))

(defonce ^:private ^ConcurrentHashMap class-labels (ConcurrentHashMap.))

(defn label-of
  "Bounded keyword label for X, cached by class. Anonymous reify classes
   label as :reify."
  [x]
  (let [^Class c (class x)]
    (or (.get class-labels c)
        (let [l (if (str/includes? (.getName c) "$reify")
                  :reify
                  (label-of-name (.getSimpleName c)))]
          (.putIfAbsent class-labels c l)
          l))))

;;; ---------------------------------------------------------------------------
;;; Metered wrappers
;;; ---------------------------------------------------------------------------

(defprotocol IMetered
  (metered-target [this] "The wrapped target."))

(defn metered?
  "True when X is a metered wrapper."
  [x]
  (satisfies? IMetered x))

(defn unwrap
  "X with its metered wrapper removed."
  [x]
  (if (metered? x) (metered-target x) x))

(defn- protocols-in
  "Protocol maps interned in namespace NS-SYM."
  [ns-sym]
  (->> (ns-publics ns-sym)
       vals
       (map deref)
       (filter #(and (map? %) (:on-interface %) (contains? % :sigs)))
       (sort-by #(str (:var %)))))

(def ^:private kind->protocols
  {:memory (delay (protocols-in 'hive-spi.memory.ports))
   :kg     (delay (protocols-in 'hive-spi.kg.protocol))})

(defn- var-sym [^clojure.lang.Var v]
  (symbol (str (.-ns v)) (str (.-sym v))))

(defn- method-impls
  "reify method bodies for protocol P. KEY-SYMS maps a method keyword to the
   local holding its op key."
  [p key-syms inner]
  (let [pns (str (.-ns ^clojure.lang.Var (:var p)))]
    (for [[mk {:keys [name arglists]}] (:sigs p)
          arglist arglists
          :let [args (vec (repeatedly (dec (count arglist)) #(gensym "a")))
                f    (symbol pns (str name))]]
      `(~name [~'_ ~@args]
              (timed ~(key-syms mk) (~f ~inner ~@args))))))

(defn- build-ctor
  "Compile (fn [inner backend] -> reify) implementing PROTOCOLS under AREA."
  [area protocols decorator?]
  (let [inner    (gensym "inner")
        backend  (gensym "backend")
        mks      (distinct (mapcat (comp keys :sigs) protocols))
        key-syms (zipmap mks (map #(gensym (str "k-" (name %))) mks))]
    (binding [*ns* (the-ns 'hive-mcp.protocols.store-metrics)]
      (eval
       `(fn [~inner ~backend]
          (let [~@(mapcat (fn [mk] [(key-syms mk) [area backend (keyword (name mk))]]) mks)]
            (reify
              IMetered
              (metered-target [~'_] ~inner)
              ~@(when decorator?
                  [`decorate/IStoreDecorator `(decorate/inner-store [~'_] ~inner)])
              ~@(mapcat (fn [p]
                          (cons (var-sym (:var p)) (method-impls p key-syms inner)))
                        protocols))))))))

(def ^:private ^ConcurrentHashMap ctors
  "Compiled wrapper constructors by [kind target-class]; rebuilt on reload so
   wrappers implement the current IMetered."
  (ConcurrentHashMap.))

(defn- ctor-for
  [kind protocols area decorator? target]
  (let [ck [kind (class target)]]
    (or (.get ctors ck)
        (let [ps (filterv #(satisfies? % target) protocols)
              c  (build-ctor area ps decorator?)]
          (.putIfAbsent ctors ck c)
          (.get ctors ck)))))

(defn wrap-store
  "TARGET wrapped so every method of the port protocols of KIND (:memory or
   :kg, or the protocol maps given as :protocols) records
   [:store/<kind> <backend> <method>]. Returns TARGET unchanged when metering
   is disabled, TARGET is nil or already metered, or wrapping fails.
   opts: :backend label (default `label-of` TARGET), :protocols."
  ([kind target] (wrap-store kind target {}))
  ([kind target {:keys [backend protocols]}]
   (if (or (nil? target) (not (enabled?)) (metered? target))
     target
     (try
       (let [ps   (or protocols (some-> (kind->protocols kind) deref))
             area (keyword "store" (name kind))
             ctor (ctor-for kind ps area (= kind :memory) target)]
         (ctor target (or backend (label-of target))))
       (catch Throwable _ target)))))

;;; ---------------------------------------------------------------------------
;;; Memory registry decorator
;;; ---------------------------------------------------------------------------

(def memory-decorator
  "hive-spi memory-store decorator, innermost (order -1000): the recorded time
   is the backend's own."
  {:id       ::metrics
   :order    -1000
   :decorate (fn [_key store] (wrap-store :memory store))})

(defn install-memory-decorator!
  "Add `memory-decorator` to the hive-spi chain, re-decorating registered
   stores, when metering is enabled. Never throws. True when added."
  []
  (try
    (boolean
     (when (enabled?)
       (when-let [add! (requiring-resolve 'hive-spi.memory.registry/add-decorator!)]
         (add! memory-decorator)
         true)))
    (catch Throwable _ false)))

;;; ---------------------------------------------------------------------------
;;; KG facade timing
;;; ---------------------------------------------------------------------------

(defn kg-key
  "Op key for KG METHOD (keyword) on STORE."
  [store method]
  [:store/kg (label-of store) method])

(defmacro kg-timed
  "Evaluate body as KG METHOD on STORE, recorded when metering is enabled."
  [store method & body]
  `(if (enabled?)
     (timed (kg-key ~store ~method) ~@body)
     (do ~@body)))

;;; ---------------------------------------------------------------------------
;;; Embedding timing
;;; ---------------------------------------------------------------------------

(defonce ^:private ^ConcurrentHashMap embed-keys (ConcurrentHashMap.))

(defn embed-key
  "Op key [:embed/<provider> OP] for PROVIDER, cached per class and op."
  [provider op]
  (let [ck [(class provider) op]]
    (or (.get embed-keys ck)
        (let [k [(keyword "embed" (name (label-of provider))) op]]
          (.putIfAbsent embed-keys ck k)
          k))))

(def ^:private OpKey
  "An op key: [area op] or [area op sub]."
  [:vector {:min 2 :max 3} [:or :keyword :string]])

(m/=> set-recorder! [:=> [:cat [:maybe ifn?]] [:maybe ifn?]])
(m/=> rebind! [:=> [:cat] :nil])
(m/=> enabled? [:=> [:cat] :boolean])
(m/=> observe! [:=> [:cat :any :int [:enum :ok :error]] :nil])
(m/=> label-of-name [:=> [:cat :any] :keyword])
(m/=> label-of [:=> [:cat :any] :keyword])
(m/=> metered? [:=> [:cat :any] :boolean])
(m/=> unwrap [:=> [:cat :any] :any])
;; The target is any port implementation; the answer is it or its wrapper.
(m/=> wrap-store [:function
                  [:=> [:cat :keyword :any] :any]
                  [:=> [:cat :keyword :any [:maybe :map]] :any]])
(m/=> install-memory-decorator! [:=> [:cat] :boolean])
(m/=> kg-key [:=> [:cat :any :keyword] OpKey])
(m/=> embed-key [:=> [:cat :any :keyword] OpKey])
