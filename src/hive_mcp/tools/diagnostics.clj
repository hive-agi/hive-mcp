(ns hive-mcp.tools.diagnostics
  "Error-diagnostic helpers for MCP tool responses: classify Clojure's
   protocol dispatch failure (missing method impl, class never extended,
   or real protocol-class poisoning) and produce an accurate operator hint."
  (:require [clojure.string :as str]))

(def ^:private protocol-poison-pattern
  "Regex for Clojure's protocol dispatch failure, the IllegalArgumentException
   \"No implementation of method: ... of protocol: ... found for class: ...\".

   The message alone does NOT prove poisoning. The same text is thrown when
   an adapter never extended the protocol, or extended it under the wrong
   method key (hive-milvus extended :create-collection where the port's key
   is :-create-collection, 2026-10-08), and a restart cures neither. Real
   poisoning is the narrower case: the protocol var was redefined (typically
   via :reload-all) so a long-lived instance's class is a stale Class object
   that shares its name with the one the protocol now knows. See
   `classify-dispatch-failure`."
  #"No implementation of method: (\S+) of protocol: #'(\S+) found for class: (\S+)")

(def ^:private known-poisonable-protocols
  "Protocols we've actually seen poisoned in production hive-mcp sessions
   (lifecycle classes whose deftype/defrecord instances are long-lived).
   Used to lift the recovery hint from \"likely\" to \"almost certainly\"."
  #{"iapetos.collector/Collector"
    "milvus-clj.client/IMilvusCore"
    "hive-weave.pool/IBindingConveyor"})

(defn- resolve-protocol
  "The protocol map named by `protocol-name` (\"ns/Proto\"), or nil when the
   var is not loaded or does not hold a protocol."
  [protocol-name]
  (let [p (try (some-> protocol-name symbol find-var deref)
               (catch Exception _ nil))]
    (when (and (map? p) (contains? p :on-interface)) p)))

(defn- loaded-class
  "The Class currently loadable under `class-name`, or nil. Goes through a
   DynamicClassLoader so classes defined at the REPL or by eval (whose newest
   definition lives in its shared in-memory cache) resolve to the newest one."
  [class-name]
  (try (Class/forName class-name false
                      (clojure.lang.DynamicClassLoader. (clojure.lang.RT/baseLoader)))
       (catch Throwable _ nil)))

(defn- named-impls
  "Impl maps the protocol holds for any Class named `class-name`."
  [proto class-name]
  (keep (fn [[k impl]]
          (when (and (class? k) (= class-name (.getName ^Class k))) impl))
        (:impls proto)))

(defn- inherited-impl
  "The impl map `c` inherits from a superclass or interface, or nil."
  [proto ^Class c]
  (some #(get (:impls proto) %) (concat (supers c) [Object])))

(defn- implements-stale-interface?
  "True when `c` implements an interface named like the protocol's but not
   the protocol's current interface: defprotocol was re-evaluated after the
   deftype/defrecord was compiled."
  [proto ^Class c]
  (let [^Class iface (:on-interface proto)]
    (boolean (some #(and (= (.getName ^Class %) (.getName iface))
                         (not (identical? % iface)))
                   (supers c)))))

(defn classify-dispatch-failure
  "Classify a protocol dispatch failure against the live protocol. Returns

     {:diagnosis :missing-method :impl-keys [...]}  the class extends the
        protocol, but its impl map has no entry for the method (wrong key)
     {:diagnosis :stale-class}   poisoning: the class loadable under this name
        dispatches fine, or implements a superseded protocol interface, so the
        failing instance belongs to a stale Class object of the same name
     {:diagnosis :not-extended}  nothing extends the protocol for this class
     {:diagnosis :unknown}       the protocol is not resolvable in this JVM

   `method` is the method name as Clojure prints it (\"-m\" or \":-m\").
   Reads protocol state; no side effects."
  [protocol-name method class-name]
  (if-let [proto (resolve-protocol protocol-name)]
    (let [method-key (keyword (str/replace (str method) #"^:" ""))
          current    (loaded-class class-name)
          impls      (seq (concat (named-impls proto class-name)
                                  (some->> current (inherited-impl proto) vector)))
          impl-keys  (vec (sort (distinct (mapcat keys impls))))]
      (cond
        (and current (implements-stale-interface? proto current))
        {:diagnosis :stale-class}

        (and current (instance? Class current)
             (.isAssignableFrom ^Class (:on-interface proto) ^Class current))
        {:diagnosis :stale-class}

        (and impls (not (some #{method-key} impl-keys)))
        {:diagnosis :missing-method :impl-keys impl-keys}

        ;; An impl under this name carries the method, yet dispatch failed:
        ;; only a different Class object of the same name explains that.
        impls
        {:diagnosis :stale-class}

        :else
        {:diagnosis :not-extended}))
    {:diagnosis :unknown}))

(defn detect-protocol-poisoning
  "Inspect a Throwable or message string for Clojure's protocol dispatch
   failure (\"No implementation of method ...\"). Returns

     {:poisoned? true|false
      :diagnosis :stale-class|:missing-method|:not-extended|:unknown
      :impl-keys [...]            ; only for :missing-method
      :protocol \"<ns>/<Proto>\"
      :method \":<m>\"
      :class \"<TypeImpl>\"
      :known-poisonable? true|false}

   when matched, else nil. :poisoned? is true ONLY for :stale-class, the one
   shape a restart repairs. No logging or side effects."
  [x]
  (let [msg (cond
              (string? x)             x
              (instance? Throwable x) (or (.getMessage ^Throwable x) "")
              :else                   (str x))]
    (when-let [[_ method protocol kls] (re-find protocol-poison-pattern msg)]
      (let [verdict (classify-dispatch-failure protocol method kls)]
        (merge verdict
               {:poisoned?         (= :stale-class (:diagnosis verdict))
                :method            method
                :protocol          protocol
                :class             kls
                :known-poisonable? (contains? known-poisonable-protocols protocol)})))))

(defn recovery-hint
  "Produce an actionable error message for a protocol dispatch failure.

   Returns nil when the input doesn't match the dispatch-failure pattern, so
   callers can use it as a soft enrichment:
     (or (recovery-hint e) (.getMessage e))

   Only a :stale-class diagnosis says \"poisoned\" and advises a JVM restart;
   a missing method or a class that never extended the protocol is a code
   defect in the adapter, which no amount of JVM recycling repairs."
  [x]
  (when-let [{:keys [protocol method class known-poisonable? diagnosis impl-keys]}
             (detect-protocol-poisoning x)]
    (let [header (str "  protocol: " protocol "\n"
                      "  method:   " method "\n"
                      "  class:    " class "\n")]
      (case diagnosis
        :stale-class
        (str "Protocol dispatch poisoned (server restart required).\n"
             header
             (if known-poisonable?
               "  This is a known-poisonable lifecycle protocol: :reload-all on a\n"
               "  Likely root cause: :reload-all was run on a ns whose dep tree\n")
             "  source ns rebinds defprotocol/deftype vars, so existing instances\n"
             "  belong to an older Class object of the same name, which the\n"
             "  current protocol does not dispatch on.\n"
             "  Recovery: restart the JVM (no in-process recovery exists).")

        :missing-method
        (str "Adapter " class " does not implement " method " of " protocol ".\n"
             header
             "  Its extend keys are " (pr-str impl-keys) ".\n"
             "  The class extends the protocol, but under none of those keys is\n"
             "  this method: the extend map uses the wrong key (check a leading\n"
             "  '-'), or the method was added to the protocol after the adapter.\n"
             "  This is a defect in the adapter's code, not JVM state: fix the\n"
             "  extend map.")

        :not-extended
        (str "Class " class " does not extend protocol " protocol ".\n"
             header
             "  No impl is registered for this class. Check that the adapter's\n"
             "  namespace was loaded and that it extends the protocol for this\n"
             "  class. This is a code or load-order defect, not JVM state.")

        (str "Protocol method has no implementation for this class.\n"
             header
             "  The protocol is not loaded in this JVM, so the cause (a missing\n"
             "  extend vs. a stale class after a reload) could not be determined.")))))
