(ns hive-mcp.hot.lifecycle.handlers-host
  "A live hive-mcp as an IHotHost, driven through the same `hot` handlers an
   operator calls and observed through the tool table clients are served.

   Boundary stratum (CPPB): the only namespace in the lifecycle harness that
   touches a running host. Run it inside a disposable instance (bin/instance2.sh),
   never on the live server: every op mutates the host.

   Two open tables keep it closed for modification (OCP): `perform!` maps an
   op kind to the handler call, and `outcome-of` maps a kind's report to an
   Outcome. A new op kind is one method in each."
  (:require [clojure.data.json :as json]
            [hive-addon.protocol :as addon]
            [hive-mcp.addons.core :as addons]
            [hive-mcp.hot.lifecycle.port :as port]
            [hive-mcp.server.core :as server]
            [hive-mcp.tools.consolidated.hot :as hot]
            [clojure.java.io :as io]))

;; ── collect: read the host ───────────────────────────────────────────────

(defn- report
  "A hot handler's reply ({:type \"text\" :text json}) as a keyword map."
  [reply]
  (let [text (:text reply)]
    (if (string? text)
      (try (json/read-str text :key-fn keyword)
           (catch Exception _ {:ok? false :unparsed text}))
      {:ok? false :reply reply})))

(defn advertised-tools
  "The tool table this host serves, in table order."
  []
  (vec (keys @(:tools @server/server-context-atom))))

(defn phases
  "{addon-id phase} from the lifecycle manager."
  []
  (into (sorted-map)
        (map (fn [[id {:keys [phase]}]] [(name id) (keyword phase)]))
        (:addons (report (hot/handle-lifecycle {})))))

(defn addon-tools
  "The tool names ADDON-ID contributes, read from the mounted instance."
  [addon-id]
  (when-let [a (:addon (addons/get-addon-entry addon-id))]
    (into (sorted-set) (map :name) (addon/tools a))))

(defn- present? [addon-id] (contains? (phases) addon-id))

(defn- checkout-dir
  "The project dir (the nearest one holding a deps.edn) at or above DIR, or nil."
  [dir]
  (loop [f (some-> dir io/file .getCanonicalFile)]
    (cond
      (nil? f)                          nil
      (.exists (io/file f "deps.edn"))  (str f)
      :else                             (recur (.getParentFile f)))))

(defn inject-paths
  "{addon-id checkout-dir}: where a path-based inject can plug each mounted
   addon back in from, read off the classpath. Only addons with source on disk
   (:local/root) have one; a jar-backed addon is absent from the map, and an
   inject of it is refused with that reason. hive-addon's source resolver is
   reached per call (Capture-by-Var); an image without it yields {}."
  []
  (if-let [spec-source (try (requiring-resolve 'hive-addon.hot.source/spec-source)
                            (catch Exception _ nil))]
    (into (sorted-map)
          (keep (fn [s]
                  (when-let [dir (checkout-dir (:hot/source-dir (spec-source s)))]
                    [(:addon/id s) dir])))
          (hot/effective-specs))
    (sorted-map)))

(defn- hot!
  "Call the `hot` tool's dispatcher with PARAMS, as an operator would, and
   read its reply. Routing by :command reaches a verb added later (e.g. eject)
   without a new require here; an image without the verb answers an error,
   which the outcome reads as a refusal and the report keeps."
  [params]
  (report (hot/handle-hot params)))

;; ── act: op kind -> handler call, report -> outcome ──────────────────────

(defmulti perform!
  "Run OP against the live host. Returns the handler's report map."
  (fn [_host op] (:op/kind op)))

(defmethod perform! :core-reload [_ _]
  (report (hot/handle-core-reload {})))

(defmethod perform! :addon-reload [_ {:op/keys [addon]}]
  (report (hot/handle-reload {:addon addon})))

(defmethod perform! :evict [_ {:op/keys [addon]}]
  (report (hot/handle-evict {:addon addon})))

(defmethod perform! :activate [_ {:op/keys [addon]}]
  (report (hot/handle-activate {:addon addon})))

(defmethod perform! :call [{:keys [probe-calls]} {:op/keys [addon]}]
  (let [before (get (phases) addon)
        {:keys [tool args]} (get probe-calls addon)
        handler (some-> @(:tools @server/server-context-atom) (get tool) :handler)
        reply (when handler
                (try (handler args)
                     (catch Throwable t {:isError true :thrown (ex-message t)})))]
    {:before before
     :after (get (phases) addon)
     :error? (or (nil? handler) (boolean (:isError reply)) (contains? reply :thrown))
     :reply reply}))

(defmethod perform! :pin [_ {:op/keys [addon policy force?]}]
  (hot! {:command "pin" :addon addon :policy (name policy) :force (true? force?)}))

(defmethod perform! :eject [_ {:op/keys [addon cascade?]}]
  (hot! {:command "eject" :addon addon :cascade (true? cascade?)}))

(defmethod perform! :unmount [_ {:op/keys [addon cascade?]}]
  (hot! {:command "unmount" :addon addon :cascade (true? cascade?)}))

(defmethod perform! :inject [{:keys [inject-paths]} {:op/keys [addon]}]
  (let [before (present? addon)
        dir    (get inject-paths addon)
        r      (if dir
                 (hot! {:command "inject" :path dir})
                 {:ok? false
                  :errors [(str "no inject path for " addon "; known: " (vec (keys inject-paths)))]})]
    (assoc r ::present-before? before ::present-after? (present? addon))))

(defmulti outcome-of
  "[op report] -> Outcome."
  (fn [op _report] (:op/kind op)))

(defn- ok-applied [{:keys [ok?]}] (if ok? :applied :refused))

(defmethod outcome-of :core-reload [_ r] (ok-applied r))
(defmethod outcome-of :addon-reload [_ r] (ok-applied r))

(defmethod outcome-of :evict [_ {:keys [evicted?]}]
  (if evicted? :applied :refused))

(defmethod outcome-of :activate [_ {:keys [ok? activated already-active?]}]
  (cond (not ok?)        :refused
        already-active?  :noop
        (seq activated)  :applied
        :else            :noop))

(defmethod outcome-of :call [_ {:keys [before after error?]}]
  (cond error?                                        :refused
        (and (= :dormant before) (= :active after))   :applied
        :else                                         :noop))

(defmethod outcome-of :pin [_ {:keys [lifecycle]}]
  (cond (not (map? lifecycle))  :refused
        (:refused lifecycle)    :refused
        :else                   :applied))

(defmethod outcome-of :eject [_ r] (ok-applied r))

;; unmount answers plug-out!'s Result projected: :ok? false with :reason on a
;; refusal, so it reads exactly as eject.
(defmethod outcome-of :unmount [_ r] (ok-applied r))

(defmethod outcome-of :inject [_ {:keys [ok?] ::keys [present-before? present-after?]}]
  (cond present-before?            :noop
        (and ok? present-after?)   :applied
        :else                      :refused))

;; ── the host ─────────────────────────────────────────────────────────────

(defrecord HandlersHost [probe-calls inject-paths last-report]
  port/IHotHost
  (apply-op! [this op]
    (let [r (perform! this op)]
      (reset! last-report r)
      (outcome-of op r)))
  (observe [_]
    {:obs/tools (advertised-tools) :obs/phases (phases)}))

(defn handlers-host
  "A HandlersHost. PROBE-CALLS is {addon-id {:tool name :args map}}: the call
   a :call op makes to exercise that addon through its advertised tool.
   INJECT-PATHS is {addon-id dir}: where an :inject op plugs that addon in
   from (the live spec's :spec/inject-paths)."
  ([probe-calls] (handlers-host probe-calls {}))
  ([probe-calls inject-paths]
   (->HandlersHost probe-calls inject-paths (atom nil))))

(defn live-spec
  "The HostSpec of the running host, read from what is mounted now. Call it
   on a freshly booted instance, before any op. :spec/inject-paths says where
   each addon with source on disk can be injected back from; :spec/deps is
   left at its default (none), so a live eject is never expected to be
   blocked by a dependent."
  []
  (let [ps        (phases)
        per-addon (into (sorted-map) (map (fn [id] [id (or (addon-tools id) #{})])) (keys ps))
        addon-all (into #{} (mapcat val) per-addon)
        policies  (into (sorted-map)
                        (map (fn [[id {:keys [lifecycle]}]] [(name id) (keyword (:policy lifecycle))]))
                        (:addons (report (hot/handle-lifecycle {}))))]
    {:spec/core-tools    (into (sorted-set) (remove addon-all) (advertised-tools))
     :spec/addon-tools   per-addon
     :spec/hook-only     (into (sorted-set) (keep (fn [[id ts]] (when (empty? ts) id))) per-addon)
     :spec/policy        policies
     :spec/inject-paths  (inject-paths)}))
