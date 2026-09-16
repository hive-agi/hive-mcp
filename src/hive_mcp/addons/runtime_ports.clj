(ns hive-mcp.addons.runtime-ports
  "Composition-root adapters injected into IAddon config under :runtime/ports.

   This namespace is the only place where the generic addon port vocabulary is
   bound to hive-mcp implementations. Adapters resolve lazily to keep manifest
   parsing free of startup-order and require-cycle hazards.")

(defn- resolve!
  [sym]
  (or (requiring-resolve sym)
      (throw (ex-info (str "Runtime port target unavailable: " sym)
                      {:error :addon/runtime-port-target-unavailable
                       :symbol sym}))))

(defn- call
  [sym & args]
  (apply (resolve! sym) args))

(defn invoke-registered-tool!
  "Invoke a registered callable tool from a fresh catalog, including hidden tools."
  [catalog tool-name arguments]
  (let [spec (some #(when (= tool-name (:name %)) %) (catalog))]
    (when-not (and spec (ifn? (:handler spec)))
      (throw (ex-info "Host tool unavailable." {:error :addon/tool-unavailable})))
    ((:handler spec) arguments)))

(defn shared-jvm?
  "True when an Integrant system is up in THIS JVM: the process that serves
   every agent. An addon must not run code under analysis here (instrumenting a
   var or load-file'ing a patch hits every other agent), so it routes that work
   to a worker JVM instead. Only the host can answer this; an addon that probed
   for hive-mcp namespaces itself would be coupled to one host.

   Resolves the system var only if `hive-mcp.server.core` is ALREADY loaded: a
   dev, test or worker JVM must not load the server (and its store components)
   just to be told it is not the server. Arity 1 injects the var lookup."
  ([] (shared-jvm? #(some-> (find-ns 'hive-mcp.server.core) (ns-resolve 'system))))
  ([system-var-fn]
   (boolean (some-> (system-var-fn) deref))))

(defn runtime-ports
  "Return a fresh map of host-neutral function ports for addon injection."
  []
  {:workflow/engine
   (fn [] (call 'hive-workflows.mcp/engine))

   :tools/invoke
   (partial invoke-registered-tool!
            #(call 'hive-mcp.tools.registry/get-advertised-tools))

   :memory/store
   (fn [slot]
     (get (call 'hive-mcp.protocols.memory/registered-stores) slot))

   :embedding/embed-batch
   (fn [memory-type texts]
     (when-let [provider (:provider
                          (call 'hive-mcp.embeddings.service/resolve-provider-for-type
                                memory-type))]
       (call 'hive-mcp.embeddings.protocol/embed-batch provider texts)))

   :embedding/provider
   (fn []
     (call 'hive-mcp.embeddings.active/get-embedding-provider))

   :embedding/configured?
   (fn []
     (call 'hive-mcp.embeddings.active/embedding-configured?))

   :kg/register-schema!
   (fn [schema]
     (call 'hive-mcp.knowledge-graph.schema/register-kg-schema! schema))

   :kg/infer-scope
   (fn [path]
     (call 'hive-mcp.knowledge-graph.scope/infer-scope-from-path path))

   :kg/resolve-project-id
   (fn [project-id]
     (call 'hive-mcp.knowledge-graph.scope/resolve-project-id project-id))

   :kg/query
   (fn
     ([slot query]
      (call 'hive-mcp.knowledge-graph.slots/query slot query))
     ([slot query inputs]
      (call 'hive-mcp.knowledge-graph.slots/query slot query inputs)))

   :host/shared-jvm?
   (fn [] (shared-jvm?))

   :extension/get
   (fn [k]
     (call 'hive-mcp.extensions.registry/get-extension k))

   :extension/keys
   (fn []
     (call 'hive-mcp.extensions.registry/registered-keys))

   :extension/register!
   (fn [k value]
     (call 'hive-mcp.extensions.registry/register! k value))

   :extension/contribute-commands!
   (fn [tool-name addon-id commands]
     (call 'hive-mcp.extensions.registry/contribute-commands!
           tool-name addon-id commands))

   :extension/retract-contributions!
   (fn [addon-id]
     (call 'hive-mcp.extensions.registry/retract-all-by-addon! addon-id))})
