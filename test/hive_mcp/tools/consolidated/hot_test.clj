(ns hive-mcp.tools.consolidated.hot-test
  "The `hot` verbs' pure strata and their lifecycle seam (S14, S15).

   Pure: reload-all-seeds (seed filtering), no-reload-split, pin-decl,
   dormant-pin-note. Seam: lifecycle verbs run against a REAL
   hive-addon.lifecycle manager built over a reified ILifecycleHost stub, so
   the test depends on the port and never on a concretion (no with-redefs)."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.lifecycle :as lc]
            [hive-addon.lifecycle.port :as lport]
            [hive-addon.hot.port :as hport]
            [hive-addon.protocol :as proto]
            [hive-dsl.result :as r]
            [hive-mcp.extensions.mount-host :as mount-host]
            [hive-mcp.tools.consolidated.hot :as hot]
            [hive-mcp.tools.consolidated.hot-eject-fallback :as fb]
            [hive-mcp.addons.manifest :as manifest]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.source :as source]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- body [resp] (json/read-str (:text resp) :key-fn keyword))

;; =============================================================================
;; reload-all seed filtering
;; =============================================================================

(def ^:private gen-row
  (gen/let [id   (gen/fmap #(str "addon." %) (gen/choose 0 30))
            kind (gen/elements [:directory :jar :absent :weird])]
    {:addon/id id :hot/source-kind kind}))

(def ^:private gen-plan
  (gen/fmap (fn [rows]
              (let [rows (vals (into {} (map (juxt :addon/id identity)) rows))
                    {ok true no false} (group-by #(= :directory (:hot/source-kind %)) rows)]
                {:hot/registered (vec ok) :hot/skipped (vec no)}))
            (gen/vector gen-row 0 12)))

(defspec reload-all-seeds-partitions-the-plan 200
  (prop/for-all [plan gen-plan]
    (let [{:keys [seeds skipped]} (hot/reload-all-seeds plan)
          skipped-ids (set (map :addon/id skipped))]
      (and (= seeds (set (map :addon/id (:hot/registered plan))))
           (= skipped-ids (set (map :addon/id (:hot/skipped plan))))
           (empty? (filter skipped-ids seeds))
           (every? (comp string? :reason) skipped)))))

(deftest reload-all-never-seeds-a-jar-addon-test
  (let [plan {:hot/registered [{:addon/id "hive.carto" :hot/source-kind :directory}]
              :hot/skipped    [{:addon/id "hive.rss" :hot/source-kind :jar}
                               {:addon/id "hive.gen" :hot/source-kind :absent}]}
        {:keys [seeds skipped]} (hot/reload-all-seeds plan)]
    (is (= #{"hive.carto"} seeds))
    (is (= ["hive.rss" "hive.gen"] (mapv :addon/id skipped)))
    (is (str/includes? (:reason (first skipped)) "restart-required"))))

(deftest skip-reason-is-open-by-registration-test
  (testing "a new source kind is one defmethod"
    (defmethod hot/skip-reason ::remote [_] "remote source: fetched, not watched")
    (try
      (is (= "remote source: fetched, not watched"
             (:reason (first (:skipped (hot/reload-all-seeds
                                        {:hot/skipped [{:addon/id "x" :hot/source-kind ::remote}]}))))))
      (finally (remove-method hot/skip-reason ::remote)))))

;; =============================================================================
;; status: no-reload split
;; =============================================================================

(defspec no-reload-split-conserves-every-pin 200
  (prop/for-all [core  (gen/set gen/symbol {:max-elements 8})
                 addon (gen/set gen/symbol {:max-elements 8})]
    (let [{:keys [effective counts] :as split} (hot/no-reload-split core addon)]
      (and (= (set effective) (set (map str (into core addon))))
           (= (:core counts) (count (:core split)) (count core))
           (= (:addon counts) (count (:addon split)) (count addon))
           (= effective (sort effective))))))

;; =============================================================================
;; pin: validation, idle reset, dormant note
;; =============================================================================

(def ^:private policies [:enum :eager :lazy :pinned])

(deftest pin-decl-test
  (testing "the policy is validated"
    (is (str/includes? (:error (hot/pin-decl {:policy "pinnned"} policies 1000)) "pinnned")))
  (testing "default policy is pinned"
    (is (= :pinned (:policy (hot/pin-decl {} policies 1000)))))
  (testing "idle_ms absent RESETS to the baseline"
    (is (= 1000 (:idle-ms (hot/pin-decl {:policy "lazy"} policies 1000)))))
  (testing "idle_ms given wins"
    (is (= 42 (:idle-ms (hot/pin-decl {:policy "lazy" :idle_ms 42} policies 1000)))))
  (testing "idle_ms must be positive"
    (is (:error (hot/pin-decl {:policy "lazy" :idle_ms -1} policies 1000)))))

(deftest dormant-pin-note-test
  (is (string? (hot/dormant-pin-note :pinned :dormant "a")))
  (is (string? (hot/dormant-pin-note :eager :failed "a")))
  (is (nil? (hot/dormant-pin-note :lazy :dormant "a")) "lazy is meant to be dormant")
  (is (nil? (hot/dormant-pin-note :pinned :active "a")) "an active addon needs no note"))

;; =============================================================================
;; Lifecycle verbs over a stub host
;; =============================================================================

(def ^:private stub-host
  (reify lport/ILifecycleHost
    (-mount! [_ specs _]
      {:ok? true :mounted (mapv (fn [s] {:addon/id (:addon/id s) :success? true}) specs)})
    (-unmount! [_ _] {:ok? true :errors []})
    (-install-stubs! [_ _ _ _] nil)
    (-remove-stubs! [_ _] nil)
    (-observe-surface [_ _] nil)))

(def ^:private stub-spec
  {:addon/id "stub.lazy" :addon/init-ns 'stub.lazy
   :addon/lifecycle {:policy :lazy :idle-ms 5000}
   :addon/surface {:tools [{:name "stub_tool"}]}})

(use-fixtures :each
  (fn [f]
    (let [mgr (lc/manager {:host stub-host :specs [stub-spec]})]
      (lc/boot! mgr {:mount-eager? false})
      (lc/install! mgr)
      (try (f) (finally (lc/uninstall!))))))

(deftest pin-on-a-dormant-addon-says-it-stays-dormant-test
  (let [out (body (hot/handle-pin {:addon "stub.lazy" :policy "pinned"}))]
    (is (= "pinned" (get-in out [:lifecycle :policy])))
    (is (= "dormant" (:phase out)))
    (is (str/includes? (:note out) "activate"))))

(deftest pin-resets-idle-ms-to-the-declared-value-test
  (hot/handle-pin {:addon "stub.lazy" :policy "lazy" :idle_ms 99})
  (is (= 99 (get-in (body (hot/handle-pin {:addon "stub.lazy" :policy "lazy" :idle_ms 99}))
                    [:lifecycle :idle-ms])))
  (is (= 5000 (get-in (body (hot/handle-pin {:addon "stub.lazy" :policy "lazy"}))
                      [:lifecycle :idle-ms]))
      "omitting idle_ms goes back to the manifest's 5000, not the earlier 99"))

(deftest pin-rejects-an-unknown-policy-test
  (let [resp (hot/handle-pin {:addon "stub.lazy" :policy "sticky"})]
    (is (:isError resp))
    (is (str/includes? (:text resp) "sticky"))))

(deftest lifecycle-verbs-share-one-seam-test
  (testing "activate and evict are LifecycleVerb descriptors over the same manager"
    (is (true? (:ok? (body (hot/handle-activate {:addon "stub.lazy"})))))
    (is (true? (:evicted? (body (hot/handle-evict {:addon "stub.lazy"})))))))

(deftest a-new-lifecycle-verb-is-one-descriptor-test
  (testing "a bridge that exists is called with (mgr & args)"
    (let [verb (hot/lifecycle-verb {:bridge 'hive-addon.lifecycle/phase :requires [:addon]})]
      (is (= "dormant" (body (verb {:addon "stub.lazy"}))))))
  (testing "a bridge this hive-addon lacks answers an actionable error"
    (let [verb (hot/lifecycle-verb {:bridge 'hive-addon.lifecycle/eject! :requires [:addon]})
          resp (verb {:addon "stub.lazy"})]
      (is (:isError resp))
      (is (str/includes? (:text resp) "eject!"))))
  (testing "a missing required param is refused before the bridge"
    (is (:isError ((hot/lifecycle-verb {:bridge 'hive-addon.lifecycle/phase :requires [:addon]}) {}))))
  (testing "a malformed descriptor is refused at construction"
    (is (thrown? AssertionError (hot/lifecycle-verb {:bridge "not-a-symbol"})))))

(deftest advertised-commands-follow-the-verb-table-test
  (is (= (set (conj (map name (keys hot/canonical-handlers)) "help"))
         (set (get-in hot/tool-def [:inputSchema :properties "command" :enum])))))

(deftest lifecycle-gate-hint-names-the-real-config-path-test
  (lc/uninstall!)
  (is (str/includes? (:text (hot/handle-lifecycle {})) "{:services {:addons {:lifecycle")))

(deftest hot-inject-resolves-declared-addon-config-like-boot
  (let [id "test.hot-inject-config"
        config-source (source/static-source {:addons {id {:code-gate :approved}}})
        resolver (get-in (#'hot/reload-opts) [:mount-opts :resolve-config])]
    (is (identical? manifest/prepare-config resolver))
    (is (= :approved
           (binding [config/*config-source* config-source]
             (:code-gate (resolver {:addon/id id :addon/type :native
                                    :addon/config {:code-gate :default}})))))))

;; =============================================================================
;; evict: a refusal is never dressed as success
;; =============================================================================

(def ^:private gen-eviction
  (gen/let [evicted? gen/boolean
            refused? (gen/elements [nil true false])
            reason   (gen/elements [nil :pinned :no-surface :dependents])
            ok?      gen/boolean]
    (cond-> {:addon/id "a" :evicted? evicted? :ok? ok?}
      (some? refused?) (assoc :refused? refused?)
      reason           (assoc :reason reason))))

(defspec eviction-outcome-never-reports-a-refusal-as-ok 300
  (prop/for-all [r gen-eviction]
    (let [out (hot/eviction-outcome r)
          refusal? (or (:refused? r) (and (false? (:evicted? r)) (some? (:reason r))))]
      (if refusal?
        (and (false? (:ok? out)) (true? (:refused? out)) (= (:reason r) (:reason out)))
        (= r out)))))

(deftest evict-of-a-pinned-addon-is-a-refusal-test
  (hot/handle-pin {:addon "stub.lazy" :policy "pinned"})
  (let [out (body (hot/handle-evict {:addon "stub.lazy"}))]
    (is (false? (:ok? out)))
    (is (true? (:refused? out)))
    (is (some? (:reason out)))))

;; =============================================================================
;; pin: the force flag reaches set-policy! when it takes one
;; =============================================================================

(defn set-policy-3 [_mgr _id decl] {:policy (:policy decl) :arity 3})
(defn set-policy-4
  ([mgr id decl] (set-policy-4 mgr id decl {}))
  ([_mgr _id decl opts] {:policy (:policy decl) :arity 4 :opts opts}))

(deftest set-policy-call-degrades-to-the-3-arity-test
  (testing "a 4-arity set-policy! receives {:force? ...}"
    (is (= {:force? true}
           (get-in (hot/set-policy-call! #'set-policy-4 :mgr "a" {:policy :lazy} true)
                   [:lifecycle :opts]))))
  (testing "an older 3-arity set-policy! is called without it, and says so only when force was asked"
    (is (= {:lifecycle {:policy :lazy :arity 3} :force-unsupported? true}
           (hot/set-policy-call! #'set-policy-3 :mgr "a" {:policy :lazy} true)))
    (is (= {:lifecycle {:policy :lazy :arity 3}}
           (hot/set-policy-call! #'set-policy-3 :mgr "a" {:policy :lazy} false)))))

(deftest pin-lazy-on-a-surface-less-addon-needs-force-test
  (when (hot/accepts-arity? (requiring-resolve 'hive-addon.lifecycle/set-policy!) 4)
    (let [mgr (lc/manager {:host stub-host
                           :specs [{:addon/id "stub.bare" :addon/init-ns 'stub.bare
                                    :addon/lifecycle {:policy :eager}}]})]
      (lc/boot! mgr {:mount-eager? false})
      (lc/install! mgr)
      (let [refused (body (hot/handle-pin {:addon "stub.bare" :policy "lazy"}))
            forced  (body (hot/handle-pin {:addon "stub.bare" :policy "lazy" :force true}))]
        (is (false? (:ok? refused)))
        (is (= "no-surface" (:refused refused)))
        (is (true? (:ok? forced)))
        (is (= "lazy" (get-in forced [:lifecycle :policy])))))))

;; =============================================================================
;; eject: a HostVerb over stub ports
;; =============================================================================

(def ^:private eject-calls (atom []))

(defn stub-eject!
  "A bridge with eject!'s shape that records its call and answers an EjectReport."
  [host specs target opts]
  (swap! eject-calls conj {:host host :specs specs :target target :opts opts})
  {:hot/target target :hot/ejected [target] :hot/torn-down [target]
   :hot/unregistered [target] :hot/unsupported [] :hot/classpath-retained ["file:/x/"]
   :teardown/data-preserved? true :ok? true :internal/noise 1})

(defn stub-refusing-eject!
  [_host _specs target _opts]
  {:hot/target target :hot/ejected [target] :hot/refused? true
   :hot/blocking ["hive.dependent"] :ok? false
   :errors ["mounted addons depend on it"]})

(defn- stub-ports
  "HostPorts over stubs; :refreshes counts surface refreshes."
  [refreshes]
  {:host        (constantly ::host)
   :specs       (constantly [{:addon/id "hive.rss"}])
   :reload-opts (constantly {:mount-opts {:resolve-config ::cfg}})
   :refresh!    (fn [] (swap! refreshes inc) {:count 1})})

(defn- eject-verb [bridge]
  {:bridge bridge :target #'hot/eject-target
   :opts (fn [{:keys [cascade]}] {:cascade? (true? cascade)})
   :report-keys hot/eject-report-keys})

(deftest eject-ok-test
  (reset! eject-calls [])
  (let [refreshes (atom 0)
        out (body (hot/run-host-verb (eject-verb `stub-eject!) (stub-ports refreshes)
                                     {:addon "hive.rss" :cascade true}))
        call (first @eject-calls)]
    (is (true? (:ok? out)))
    (is (= ["hive.rss"] (:ejected out)))
    (is (= ["file:/x/"] (:classpath-retained out)) "what stays is reported")
    (is (not (contains? out :noise)) "only EjectReport keys are answered")
    (is (= 1 @refreshes) "the surface is refreshed")
    (is (= ::host (:host call)))
    (is (= "hive.rss" (:target call)))
    (is (true? (get-in call [:opts :cascade?])))
    (is (= ::cfg (get-in call [:opts :mount-opts :resolve-config])) "reload-opts ride along")))

(deftest eject-by-path-test
  (reset! eject-calls [])
  (hot/run-host-verb (eject-verb `stub-eject!) (stub-ports (atom 0)) {:path "/tmp/hive-rss"})
  (is (= "/tmp/hive-rss" (:target (first @eject-calls))))
  (is (false? (get-in (first @eject-calls) [:opts :cascade?]))))

(deftest eject-refused-with-blocking-test
  (let [out (body (hot/run-host-verb (eject-verb `stub-refusing-eject!) (stub-ports (atom 0))
                                     {:addon "hive.rss"}))]
    (is (false? (:ok? out)))
    (is (true? (:refused? out)))
    (is (= ["hive.dependent"] (:blocking out)))))

(deftest eject-needs-a-target-test
  (is (:isError (hot/run-host-verb (eject-verb `stub-eject!) (stub-ports (atom 0)) {}))))

(deftest a-new-verb-is-one-registration-test
  (testing "eject is one entry in canonical-handlers and therefore advertised"
    (is (= #'hot/handle-eject (:eject hot/canonical-handlers)))
    (is (some #{"eject"} (hot/command-enum))))
  (testing "a host verb built from a descriptor is a complete handler"
    (let [handlers (assoc hot/canonical-handlers
                          :probe (hot/host-verb {:bridge `stub-eject!
                                                 :target :addon
                                                 :report-keys [:ok? :hot/ejected]}))]
      (is (fn? (:probe handlers)))
      (is (thrown? AssertionError (hot/host-verb {:bridge `stub-eject! :target :addon}))
          "a descriptor without :report-keys is refused at construction"))))

;; =============================================================================
;; unmount: plug-out! (a Result) projected for the wire
;; =============================================================================

(deftest unmount-outcome-projects-the-railway-test
  (testing "ok: the EjectReport itself"
    (is (= {:ok? true :hot/ejected ["a"]}
           (hot/unmount-outcome (r/ok {:ok? true :hot/ejected ["a"]})))))
  (testing "a refusal is stated plainly, with its category and sentence"
    (let [out (hot/unmount-outcome (r/err :hot/eject-refused
                                          {:ok? false :hot/blocking ["b"] :message "b depends on a"}))]
      (is (false? (:ok? out)))
      (is (= "hot/eject-refused" (:reason out)))
      (is (true? (:hot/refused? out)))
      (is (= ["b"] (:hot/blocking out)))
      (is (= "b depends on a" (:message out)))
      (is (not (contains? out :error)) "the Result envelope is not leaked")))
  (testing "unknown and failed keep their category; a missing message is named"
    (is (= "hot/eject-unknown" (:reason (hot/unmount-outcome (r/err :hot/eject-unknown {:message "x"})))))
    (is (= ":hot/eject-failed" (:message (hot/unmount-outcome (r/err :hot/eject-failed {}))))))
  (testing "a bare report (no Result) passes through"
    (is (= {:ok? true} (hot/unmount-outcome {:ok? true})))))

(defn stub-plug-out-ok! [_host _specs target _opts]
  (r/ok {:hot/target target :hot/ejected [target] :hot/unregistered [target]
         :hot/unsupported [] :ok? true}))

(defn stub-plug-out-refused! [_host _specs target _opts]
  (r/err :hot/eject-refused {:hot/target target :hot/ejected [target] :hot/refused? true
                             :hot/blocking ["hive.dependent"] :ok? false
                             :message "mounted addons depend on it"}))

(defn- unmount-verb [bridge]
  {:bridge bridge :target #'hot/eject-target
   :opts (fn [{:keys [cascade]}] {:cascade? (true? cascade)})
   :project #'hot/unmount-outcome
   :report-keys hot/unmount-report-keys})

(deftest unmount-ok-over-stub-ports-test
  (let [refreshes (atom 0)
        resp (hot/run-host-verb (unmount-verb `stub-plug-out-ok!) (stub-ports refreshes) {:addon "hive.rss"})
        out  (body resp)]
    (is (not (:isError resp)))
    (is (true? (:ok? out)))
    (is (= ["hive.rss"] (:unregistered out)))
    (is (= 1 @refreshes))))

(deftest unmount-refusal-is-an-answer-not-a-throw-test
  (let [resp (hot/run-host-verb (unmount-verb `stub-plug-out-refused!) (stub-ports (atom 0))
                                {:addon "hive.rss"})
        out  (body resp)]
    (is (not (:isError resp)) "a refusal is the verb's answer, not a tool failure")
    (is (false? (:ok? out)))
    (is (= "hot/eject-refused" (:reason out)))
    (is (true? (:refused? out)))
    (is (= ["hive.dependent"] (:blocking out)))
    (is (= "mounted addons depend on it" (:message out)))))

(deftest unmount-is-registered-and-advertised-test
  (is (= #'hot/handle-unmount (:unmount hot/canonical-handlers)))
  (is (some #{"unmount"} (hot/command-enum)))
  (is (str/includes? (:description hot/tool-def) "unmount")))

;; Integration: the real plug-out! over hive-mcp's AddonRegistryHost (fake
;; registry seams) proves the host plugs OUT through IMountUnregister — the
;; ejected id lands under :hot/unregistered, never :hot/unsupported — and that
;; shutdown precedes the drop, so no data is deleted.

(defrecord UnmountProbe []
  proto/IAddon
  (addon-id [_] "probe.unmount")
  (addon-type [_] :native)
  (capabilities [_] #{})
  (initialize! [_ _] {:success? true :errors []})
  (shutdown! [_] nil)
  (tools [_] [])
  (schema-extensions [_] [])
  (health [_] {:status :ok})
  (excluded-tools [_] #{})
  (hooks [_] {}))

(defn- registry-host [log reg]
  (mount-host/addon-registry-host
   {:reg-fn        (fn [a] (swap! reg assoc (proto/addon-id a) a) {:success? true})
    :init-fn       (fn [_ _] {:success? true :errors []})
    :shutdown-fn   (fn [id] (swap! log conj [:shutdown id]) {:success? true})
    :unreg-fn      (fn [id] (swap! log conj [:unregister id]) (swap! reg dissoc id) {:success? true})
    :registered-fn (fn [id] (get @reg id))}))

(def ^:private null-dirs
  (reify hport/IHotDirs
    (-extend-dirs! [_ _] (r/ok {:dirs [] :added []}))
    (-remove-dirs! [_ req] {:removed [] :kept [] :absent (vec (:dirs req)) :dirs [] :shared {}})))

(deftest unmount-through-the-real-plug-out-unregisters-test
  (if-not (try (requiring-resolve 'hive-addon.hot.inject/plug-out!) (catch Throwable _ nil))
    (is true "skipped: this hive-addon predates plug-out!")
    (let [log  (atom [])
          reg  (atom {"probe.unmount" (->UnmountProbe)})
          h    (registry-host log reg)
          ports {:host        (constantly h)
                 ;; effective-specs' contract: manifests intersected with the live registry
                 :specs       (fn [] (filterv #(contains? @reg (:addon/id %))
                                              [{:addon/id "probe.unmount"
                                                :addon/init-ns 'hive-mcp.tools.consolidated.hot-test}]))
                 :reload-opts (constantly {:hot-dirs null-dirs})
                 :refresh!    (constantly {:count 0})}
          out  (body (hot/run-host-verb hot/unmount-verb ports {:addon "probe.unmount"}))]
      (is (true? (:ok? out)) (pr-str out))
      (is (= ["probe.unmount"] (:unregistered out)))
      (is (= [] (:unsupported out)) "the host has IMountUnregister")
      (is (= [[:shutdown "probe.unmount"] [:unregister "probe.unmount"]] @log)
          "shut down (data kept) before it is dropped")
      (is (empty? @reg))
      (testing "a second unmount is refused as unknown, plainly"
        (let [again (body (hot/run-host-verb hot/unmount-verb ports {:addon "probe.unmount"}))]
          (is (false? (:ok? again)))
          (is (= "hot/eject-unknown" (:reason again))))))))

;; =============================================================================
;; The host-native fallback: eject/unmount without hive-addon's eject!
;; Doubles reach it through the HostPorts (:host, :specs, :reload-opts).
;; =============================================================================

(defrecord FallbackProbe [id log throw?]
  proto/IAddon
  (addon-id [_] id)
  (addon-type [_] :native)
  (capabilities [_] #{})
  (initialize! [_ _] {:success? true :errors []})
  (shutdown! [_]
    (swap! log conj [:iaddon-shutdown id])
    (when throw? (throw (ex-info "probe shutdown exploded" {:id id})))
    nil)
  (tools [_] [])
  (schema-extensions [_] [])
  (health [_] {:status :ok})
  (excluded-tools [_] #{})
  (hooks [_] {}))

(defn- fallback-host
  "An AddonRegistryHost whose seams mirror addons.core over the REG atom:
   shutdown-fn calls the instance's IAddon shutdown! and answers a failure
   as {:success? false :errors [..]}, unreg-fn drops the entry."
  [log reg]
  (mount-host/addon-registry-host
   {:reg-fn        (fn [a] (swap! reg assoc (proto/addon-id a) a) {:success? true})
    :init-fn       (fn [_ _] {:success? true :errors []})
    :shutdown-fn   (fn [id]
                     (try (proto/shutdown! (get @reg id))
                          {:success? true}
                          (catch Throwable t {:success? false :errors [(ex-message t)]})))
    :unreg-fn      (fn [id] (swap! log conj [:unregister id]) (swap! reg dissoc id) {:success? true})
    :registered-fn (fn [id] (get @reg id))}))

(defn- fallback-ports [log reg specs refreshes]
  (let [h (fallback-host log reg)]
    {:host        (constantly h)
     :specs       (constantly specs)
     :reload-opts (constantly
                   {:unhot!      (fn [ss] (swap! log conj [:unhot (mapv :addon/id ss)])
                                   {:hot/unregistered (mapv :addon/id ss)})
                    :source-dirs (fn [ss] (mapv #(str "/src/" (:addon/id %)) ss))})
     :refresh!    (fn [] (swap! refreshes inc) {:count 1})}))

(defn- fallback-verb [bridge]
  {:bridge bridge :target #'hot/eject-target :opts #'hot/plug-out-opts
   :fallback #'fb/eject-fallback :report-keys hot/eject-report-keys})

(def ^:private absent-unmount-verb
  (assoc hot/unmount-verb :bridge 'no.such.hive-addon/plug-out!))

(def ^:private probe-spec {:addon/id "probe.fb" :addon/init-ns "probe.fb.init"})

(deftest eject-unsupported-when-the-bridge-is-absent-test
  (testing "a descriptor with no :fallback still answers unsupported"
    (let [refreshes (atom 0)
          resp (hot/run-host-verb (eject-verb 'no.such.hive-addon/eject!) (stub-ports refreshes)
                                  {:addon "hive.rss"})
          out  (body resp)]
      (is (not (:isError resp)) "an absent bridge is a report, not a failure of the tool")
      (is (false? (:ok? out)))
      (is (= "unsupported" (:reason out)))
      (is (zero? @refreshes))))
  (testing "eject and unmount carry the fallback, so a registered addon is plugged out"
    (is (= #'fb/plug-out-fallback (:fallback hot/unmount-verb)))
    (let [log       (atom [])
          refreshes (atom 0)
          reg       (atom {"probe.fb" (->FallbackProbe "probe.fb" log false)})
          resp      (hot/run-host-verb (fallback-verb 'no.such.hive-addon/eject!)
                                       (fallback-ports log reg [probe-spec] refreshes)
                                       {:addon "probe.fb"})
          out       (body resp)]
      (is (not (:isError resp)))
      (is (true? (:ok? out)) (pr-str out))
      (is (true? (:fallback out)))
      (is (nil? (:reason out)) "no longer unsupported")
      (is (= ["probe.fb"] (:ejected out)))
      (is (= ["probe.fb"] (:torn-down out)))
      (is (= ["probe.fb"] (:unregistered out)))
      (is (= ["probe.fb"] (:unhot out)))
      (is (= [[:iaddon-shutdown "probe.fb"] [:unregister "probe.fb"] [:unhot ["probe.fb"]]] @log)
          "IAddon shutdown! runs before the entry is dropped")
      (is (empty? @reg))
      (is (= 1 @refreshes) "the surface is refreshed")
      (is (false? (get-in out [:drain :drained?])) "it says plainly that it did not drain")
      (is (= ["/src/probe.fb"] (get-in out [:retained :classpath])))
      (is (= ["/src/probe.fb"] (get-in out [:retained :hive-hot-dirs])))
      (is (= ["probe.fb.init"] (get-in out [:retained :namespaces])))
      (is (true? (get-in out [:retained :restart-required?]))))))

(deftest fallback-unmount-answers-ok-test
  (let [log (atom [])
        reg (atom {"probe.fb" (->FallbackProbe "probe.fb" log false)})
        out (body (hot/run-host-verb absent-unmount-verb
                                     (fallback-ports log reg [probe-spec] (atom 0))
                                     {:addon "probe.fb"}))]
    (is (true? (:ok? out)) (pr-str out))
    (is (true? (:fallback out)))
    (is (= ["probe.fb"] (:unregistered out)))))

(deftest fallback-unknown-target-test
  (let [log (atom [])
        reg (atom {"probe.fb" (->FallbackProbe "probe.fb" log false)})
        ports (fallback-ports log reg [probe-spec] (atom 0))]
    (testing "eject: the bridge's unknown shape"
      (let [out (body (hot/run-host-verb (fallback-verb 'no.such.hive-addon/eject!) ports
                                         {:addon "nope"}))]
        (is (false? (:ok? out)))
        (is (= [] (:ejected out)))
        (is (= ["nope"] (:unknown out)))
        (is (seq (:errors out)))))
    (testing "unmount: refused as hot/eject-unknown"
      (let [out (body (hot/run-host-verb absent-unmount-verb ports {:addon "nope"}))]
        (is (false? (:ok? out)))
        (is (= "hot/eject-unknown" (:reason out)))))
    (is (empty? @log) "nothing was shut down")
    (is (contains? @reg "probe.fb"))))

(deftest fallback-refuses-while-dependents-are-mounted-test
  (let [log   (atom [])
        reg   (atom {"probe.fb"  (->FallbackProbe "probe.fb" log false)
                     "probe.dep" (->FallbackProbe "probe.dep" log false)})
        specs [probe-spec {:addon/id "probe.dep" :addon/dependencies #{"probe.fb"}}]
        ports (fallback-ports log reg specs (atom 0))]
    (testing "eject is refused and names the dependent"
      (let [out (body (hot/run-host-verb (fallback-verb 'no.such.hive-addon/eject!) ports
                                         {:addon "probe.fb"}))]
        (is (false? (:ok? out)))
        (is (true? (:refused? out)))
        (is (= ["probe.dep"] (:blocking out)))))
    (testing "cascade stays refused, saying it needs the newer hive-addon"
      (let [out (body (hot/run-host-verb (fallback-verb 'no.such.hive-addon/eject!) ports
                                         {:addon "probe.fb" :cascade true}))]
        (is (true? (:refused? out)))
        (is (str/includes? (first (:errors out)) "hive-addon.hot.inject/eject!"))))
    (testing "unmount answers hot/eject-refused"
      (let [out (body (hot/run-host-verb absent-unmount-verb ports {:addon "probe.fb"}))]
        (is (= "hot/eject-refused" (:reason out)))
        (is (true? (:refused? out)))))
    (is (empty? @log) "nothing was shut down")
    (is (= #{"probe.fb" "probe.dep"} (set (keys @reg))))))

(deftest fallback-shutdown-throwing-still-answers-a-report-test
  (let [log  (atom [])
        reg  (atom {"probe.fb" (->FallbackProbe "probe.fb" log true)})
        resp (hot/run-host-verb (fallback-verb 'no.such.hive-addon/eject!)
                                (fallback-ports log reg [probe-spec] (atom 0))
                                {:addon "probe.fb"})
        out  (body resp)]
    (is (not (:isError resp)) "a failed shutdown is in the report, not thrown")
    (is (false? (:ok? out)))
    (is (true? (:fallback out)))
    (is (some #(str/includes? % "probe shutdown exploded") (:errors out)))
    (is (= [] (:torn-down out)) "a shutdown that threw is not reported as torn down")
    (is (= ["probe.fb"] (:unregistered out)) "the entry is still dropped")
    (is (empty? @reg))
    (testing "unmount projects it as hot/eject-failed"
      (let [reg2 (atom {"probe.fb" (->FallbackProbe "probe.fb" log true)})
            out2 (body (hot/run-host-verb absent-unmount-verb
                                          (fallback-ports log reg2 [probe-spec] (atom 0))
                                          {:addon "probe.fb"}))]
        (is (= "hot/eject-failed" (:reason out2)))))))

(deftest a-present-bridge-never-runs-the-fallback-test
  (reset! eject-calls [])
  (let [fallback-calls (atom 0)
        verb  (assoc (eject-verb `stub-eject!)
                     :fallback (fn [& _] (swap! fallback-calls inc) {:ok? false}))
        with  (body (hot/run-host-verb verb (stub-ports (atom 0)) {:addon "hive.rss"}))
        without (body (hot/run-host-verb (eject-verb `stub-eject!) (stub-ports (atom 0))
                                         {:addon "hive.rss"}))]
    (is (zero? @fallback-calls))
    (is (= without with) "the bridge path answers exactly what it answered before")
    (is (not (contains? with :fallback)))
    (is (= 2 (count @eject-calls)))))

(deftest inject-says-how-it-can-be-undone-test
  (is (= (if (try (requiring-resolve 'hive-addon.hot.inject/eject!) (catch Throwable _absent nil))
           :bridge :fallback)
         (fb/undo-mode))))

(deftest inject-undo-report-states-each-capability-test
  (testing "the bridge is a complete undo"
    (let [u (fb/undo-report true true)]
      (is (= :bridge (:mode u)))
      (is (true? (:undoable? u)))
      (is (true? (:complete? u)))
      (is (false? (:restart-required? u)))))
  (testing "the host-native fallback undoes it, but not completely"
    (let [u (fb/undo-report false true)]
      (is (= :fallback (:mode u)))
      (is (true? (:undoable? u)))
      (is (false? (:complete? u)))
      (is (true? (:restart-required? u)))
      (is (str/includes? (:message u) "restart"))))
  (testing "nothing can undo it"
    (let [u (fb/undo-report false false)]
      (is (= :none (:mode u)))
      (is (false? (:undoable? u)))
      (is (str/includes? (:message u) "only a restart")))))

(deftest inject-undo-preflight-reads-the-image-test
  (let [u (fb/undo-preflight (mount-host/addon-registry-host))]
    (is (= (if (and (requiring-resolve 'hive-addon.hot.inject/eject!)
                    (requiring-resolve 'hive-addon.hot.inject/plug-out!))
             :bridge :fallback)
           (:mode u))
        "a registry host can always fall back; the bridge wins when present"))
  (is (= (if (and (requiring-resolve 'hive-addon.hot.inject/eject!)
                  (requiring-resolve 'hive-addon.hot.inject/plug-out!))
           :bridge :none)
         (:mode (fb/undo-preflight nil)))
      "without a host only the bridge can undo an inject"))

(deftest inject-undo-gate-refuses-before-mounting-test
  (let [bridge   (fb/undo-report true true)
        fallback (fb/undo-report false true)
        none     (fb/undo-report false false)]
    (testing "a complete undo proceeds silently"
      (is (= {:warning nil} (hot/inject-undo-gate bridge {})))
      (is (= {:warning nil} (hot/inject-undo-gate bridge {:require_undo true}))))
    (testing "a fallback undo proceeds with a warning"
      (is (str/includes? (:warning (hot/inject-undo-gate fallback {})) "restart")))
    (testing "require_undo refuses an incomplete undo up front"
      (is (str/includes? (:refuse (hot/inject-undo-gate fallback {:require_undo true}))
                         "require_undo")))
    (testing "no undo at all refuses unless force"
      (is (string? (:refuse (hot/inject-undo-gate none {}))))
      (is (string? (:warning (hot/inject-undo-gate none {:force true}))))
      (is (string? (:refuse (hot/inject-undo-gate none {:force true :require_undo true})))))))

(deftest inject-advertises-require-undo-test
  (is (contains? (get-in hot/tool-def [:inputSchema :properties]) "require_undo"))
  (is (str/includes? (:description hot/tool-def) ":hot/undo")))

(deftest fallback-pure-strata-test
  (testing "declared dependents close transitively and exclude the seeds"
    (is (= #{"b" "c"} (fb/declared-dependents [{:addon/id "a"}
                                               {:addon/id "b" :addon/dependencies #{"a"}}
                                               {:addon/id "c" :addon/dependencies #{"b"}}
                                               {:addon/id "d"}]
                                              ["a"]))))
  (testing "verdict"
    (is (= :unknown (fb/verdict [] [])))
    (is (= :refused (fb/verdict ["a"] ["b"])))
    (is (= :proceed (fb/verdict ["a"] []))))
  (testing "outcome keeps plug-out!'s categories"
    (is (= :hot/eject-refused (:error (fb/outcome {:ok? false :hot/refused? true :errors ["x"]}))))
    (is (= :hot/eject-unknown (:error (fb/outcome {:ok? false :hot/ejected [] :errors ["x"]}))))
    (is (= :hot/eject-failed (:error (fb/outcome {:ok? false :hot/ejected ["a"] :errors ["x"]}))))
    (is (= {:ok? true} (:ok (fb/outcome {:ok? true}))))))
