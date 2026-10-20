(ns hive-mcp.system.contract-test
  "Contract facets for the system tree (registry, addon-hot, keepalive,
   layer3/5, child-scope, sweep coordinator, in-core shutdown hooks, and the
   sweepers). Pure calculations get schema-driven trifectas; boundary fns get
   goldens over plain-EDN projections of their results, with every
   process-global registry they drive captured and restored around the test.
   No scheduler is started and no JVM shutdown hook is registered."
  (:require [clojure.core.async :as async]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.protocols.lifecycle :as lifecycle]
            [hive-mcp.system.addon-hot :as ah]
            [hive-mcp.system.child-scope :as cs]
            [hive-mcp.system.keepalive :as keepalive]
            [hive-mcp.system.layer3 :as layer3]
            [hive-mcp.system.layer5 :as layer5]
            [hive-mcp.system.registry :as reg]
            [hive-mcp.system.shutdown.in-core :as in-core]
            [hive-mcp.system.sweep-coordinator :as sc]
            [hive-mcp.system.sweepers.heap-pressure :as hp]
            [hive-mcp.system.sweepers.stale-cache :as stale]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; Isolation: the three lifecycle registries are process-global
;;; ---------------------------------------------------------------------------

(use-fixtures :each
  (fn [f]
    (let [saved (reg/capture-all)]
      (try (f) (finally (reg/restore-all! saved))))))

(defn- hook [n p]
  (reify lifecycle/IShutdownHook
    (shutdown-priority [_] p) (shutdown-name [_] n) (shutdown! [_ _] nil)))

(defn- sweep [n s]
  (reify lifecycle/ISweepable
    (sweep-interval-s [_] s) (sweep-name [_] n) (sweep! [_ _] {:swept 0 :errors []})))

(defn- owner [id]
  (reify lifecycle/IResourceOwner
    (owner-id [_] id) (owned-resources [_] []) (release-all! [_] nil)))

;;; ---------------------------------------------------------------------------
;;; registry
;;; ---------------------------------------------------------------------------

(golden/deftest-golden registry-lifecycle-golden
  "test/golden/hive-mcp/system/registry-lifecycle.edn"
  (do
    (reg/reset-all!)
    (let [empty-snap (reg/registry-snapshot)
          empty-sweeps (reg/registered-sweeps)]
      (reg/register-shutdown! (hook "c/late" 400))
      (reg/register-shutdown! (hook "c/early" 10))
      (reg/register-sweep! (sweep "c/sweep" 120))
      (reg/register-resource-owner! (owner "c/owner"))
      (let [full (reg/registry-snapshot)
            order (mapv lifecycle/shutdown-name (reg/registered-shutdown-hooks))
            got (some-> (reg/get-resource-owner "c/owner") lifecycle/owner-id)
            captured (reg/capture-all)]
        (reg/unregister-shutdown! "c/late")
        (reg/unregister-sweep! "c/sweep")
        (reg/unregister-resource-owner! "c/owner")
        (let [after (reg/registry-snapshot)]
          (reg/restore-all! captured)
          {:empty-snap empty-snap
           :empty-sweeps empty-sweeps
           :full full
           :order order
           :got got
           :absent (reg/get-resource-owner "c/none")
           :after-unregister after
           :restored (reg/registry-snapshot)
           :captured-keys (into {} (map (fn [[k v]] [k (vec (sort (keys v)))])) captured)})))))

(deftest registry-contracts-validate-their-own-outputs
  (reg/reset-all!)
  (reg/register-shutdown! (hook "c/a" 1))
  (reg/register-sweep! (sweep "c/s" 30))
  (reg/register-resource-owner! (owner "c/o"))
  (is (m/validate reg/RegistrySnapshot (reg/registry-snapshot)))
  (is (m/validate reg/RegistryState (reg/capture-all)))
  (is (m/validate reg/RestorableState (reg/capture-all)))
  (testing "the snapshot schema has teeth"
    (is (not (m/validate reg/RegistrySnapshot {:shutdown [["x" "not-a-number"]] :sweeps [] :resources nil})))
    (is (not (m/validate reg/ShutdownHook (sweep "x" 1))))))

;;; ---------------------------------------------------------------------------
;;; addon-hot: the pure strata
;;; ---------------------------------------------------------------------------

(def ^:private SpecIn [:map [:addon/id [:enum "hive.a" "hive.b" :hive.c]]])

(deftrifecta-from-schema registered-specs-keeps-exactly-the-registered
  #'ah/registered-specs
  {:in [:cat [:vector SpecIn]
        [:map [:hot/registered [:vector SpecIn]]]]
   :out [:vector ah/MountSpec]
   :rel (fn [[specs reg] out]
          (let [ids (set (map (comp str :addon/id) (:hot/registered reg)))]
            (= out (filterv #(ids (str (:addon/id %))) specs))))
   :golden-path "test/golden/hive-mcp/system/registered-specs.edn"
   :num-tests 100})

(deftrifecta-from-schema init-opts-never-carries-an-addon-dir
  #'ah/init-opts
  {:in [:cat [:map [:hot/no-reload [:set :symbol]] [:hot/dirs [:set :string]]]
        [:maybe [:int {:min 0}]]]
   :out ah/InitOpts
   :rel (fn [[plan since] out]
          (and (= [] (:dirs out))
               (= (set (:hot/no-reload plan)) (:no-reload out))
               (= since (:since out))))
   :num-tests 100})

(deftrifecta-from-schema boot-report-counts-and-sorts
  #'ah/boot-report
  {:in [:cat [:vector SpecIn]
        [:map [:hot/dirs [:set [:enum "/b" "/a" "/c"]]]
         [:hot/skipped [:vector SpecIn]]]
        [:map [:fresh? :boolean]]
        [:map [:ok? :boolean] [:hot/registered [:vector SpecIn]] [:errors [:vector :string]]]
        [:map [:claims [:map-of :string [:vector :string]]] [:errors [:vector :string]]]]
   :out ah/BootReport
   :rel (fn [[specs plan init reg claimed] out]
          (and (= (count specs) (:mounted out))
               (= (vec (sort (:hot/dirs plan))) (:dirs out))
               (= (:fresh? init) (:fresh? out))
               (= (:ok? reg) (:ok? out))
               (= (into (:errors reg) (:errors claimed)) (:errors out))))
   :num-tests 100})

(defn- catalog [specs host]
  (reify ah/IAddonCatalog (mounted-specs [_] specs) (mount-host [_] host)))

(defn- engine [throw-on]
  (reify ah/IHotEngine
    (plan [_ _ specs]
      (when (= throw-on :plan) (throw (ex-info "plan broke" {})))
      {:hot/dirs #{"/a/src"} :hot/no-reload #{'p} :hot/registered specs :hot/skipped []})
    (ensure-init! [_ _] {:fresh? true})
    (extend-dirs! [_ req] {:dirs (:dirs req) :added (:dirs req)})
    (register! [_ _ specs]
      (when (= throw-on :register) (throw (ex-info "register broke" {})))
      {:ok? true :hot/registered specs})
    (unregister! [_ specs] (mapv :addon/id specs))))

(golden/deftest-golden addon-hot-boot-arms-golden
  "test/golden/hive-mcp/system/addon-hot-boot.edn"
  (let [specs [{:addon/id "hive.a"}]
        prior (ah/last-report)]
    (try
      {:ok         (ah/boot! (catalog specs :host) (engine nil) 5)
       :empty      (ah/boot! (catalog [] :host) (engine nil) nil)
       :no-host    (ah/boot! (catalog specs nil) (engine nil) nil)
       :plan-fail  (:error (ah/boot! (catalog specs :host) (engine :plan) nil))
       :reg-fail   (:error (ah/boot-registration (catalog specs :host) (engine :register) nil))
       :registered (:registered (:ok (ah/boot-registration (catalog specs :host) (engine nil) nil)))
       :start      (let [st (ah/start! (catalog specs :host) (engine nil) nil)]
                     {:registered (:registered st)
                      :result-ok? (contains? (:result st) :ok)
                      :last-is-result (= (:result st) (ah/last-report))
                      :stop (ah/stop! st)})
       :stop-empty (ah/stop! {:engine (engine nil) :registered []})}
      (finally (reset! ah/last-boot prior)))))

(deftest addon-hot-boot-results-hit-each-arm
  (let [ok (ah/boot! (catalog [{:addon/id "x"}] :host) (engine nil) nil)
        err (ah/boot! (catalog [{:addon/id "x"}] nil) (engine nil) nil)]
    (is (m/validate ah/BootResult ok))
    (is (m/validate [:map [:ok ah/BootReport]] ok) "the full report on the happy arm")
    (is (m/validate ah/BootResult err))
    (is (not (m/validate [:map [:ok :any]] err)))
    (is (m/validate ah/AddonCatalog (ah/live-catalog)))
    (is (m/validate ah/HotEngine (ah/live-engine)))))

;;; ---------------------------------------------------------------------------
;;; keepalive: no hook is registered; channels are local
;;; ---------------------------------------------------------------------------

(deftest keepalive-awaits-return-what-was-delivered
  (let [ch (async/promise-chan)]
    (async/put! ch :shutdown)
    (is (= :shutdown (keepalive/await-shutdown! {:mode :stdio :shutdown-ch ch}))))
  (testing "stdio join wins when shutdown never fires"
    (let [p (doto (promise) (deliver :done))]
      (is (= [:stdio :done]
             (keepalive/await-shutdown-or-stdio! {:shutdown-ch (async/promise-chan)} p)))))
  (testing "shutdown wins when the join never delivers"
    (let [ch (async/promise-chan)]
      (async/put! ch :shutdown)
      (is (= [:shutdown :shutdown]
             (keepalive/await-shutdown-or-stdio! {:shutdown-ch ch} (promise)))))))

;;; ---------------------------------------------------------------------------
;;; layer3 / layer5
;;; ---------------------------------------------------------------------------

(deftrifecta-from-schema discovery-mode-defaults-to-classpath
  #'layer3/discovery-mode
  {:in [:cat [:maybe [:map [:discover {:optional true} [:enum :classpath :none :other]]]]]
   :out :keyword
   :rel (fn [[config] out] (= out (get config :discover :classpath)))
   :golden-path "test/golden/hive-mcp/system/discovery-mode.edn"
   :num-tests 100})

(deftest stdio-surface-registers-only-with-a-tools-atom
  (let [seen (atom [])
        reg! (fn [id s] (swap! seen conj [id (:surface/kind s)]) id)]
    (is (= :mcp-stdio (layer5/register-stdio-surface! reg! {:tools (atom {})})))
    (is (nil? (layer5/register-stdio-surface! reg! {})))
    (is (= [[:mcp-stdio :tools-atom]] @seen))))

;;; ---------------------------------------------------------------------------
;;; child-scope: pure argv policy
;;; ---------------------------------------------------------------------------

(def ^:private ArgvIn [:vector {:max 4} [:enum "sh" "ls" "/bin/sh" "nope-not-a-binary" "-c"]])

(deftrifecta-from-schema plan-argv-scopes-only-a-resolvable-command
  #'cs/plan-argv
  {:in [:cat [:enum true false nil :yes] ArgvIn
        [:map [:path [:enum "/bin:/usr/bin" "" "/nonexistent"]] [:cwd [:enum "/" "/tmp"]]
         [:prefix [:enum ["env" "S=1"] ["systemd-run"]]]]]
   :out [:vector :string]
   :rel (fn [[ok? argv {:keys [path cwd prefix]}] out]
          (if (and (true? ok?) (seq argv) (cs/resolve-executable (first argv) path cwd))
            (= out (into prefix argv))
            (= out argv)))
   :classify (fn [[_ argv _] out] (if (= out argv) :plain :scoped))
   :classify-domain #{:plain :scoped}
   :classify-floor 5
   :golden-path "test/golden/hive-mcp/system/plan-argv.edn"
   :num-tests 200})

(deftrifecta-from-schema scope-start-failure-needs-every-signal
  #'cs/scope-start-failure?
  {:in [:map [:exit [:enum 0 1 2]]
        [:stdout-quiet? :boolean]
        [:stderr [:enum "" "Failed to start transient scope unit: x"
                  "Failed to connect to bus: no" "real error" "Failed to connect to bus\na\nb"]]]
   :out :boolean
   :rel (fn [{:keys [exit stdout-quiet? stderr]} out]
          (= out (and (= 1 exit) stdout-quiet?
                      (contains? #{"Failed to start transient scope unit: x" "Failed to connect to bus: no"}
                                 stderr))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :classify-floor 3
   :golden-path "test/golden/hive-mcp/system/scope-start-failure.edn"
   :num-tests 200})

(deftest probes-cache-expire-and-record
  (let [calls (atom 0)
        probe (fn [] (swap! calls inc) true)
        cached (cs/caching-probe probe)]
    (is (true? (cached)))
    (is (true? (cached)))
    (is (= 1 @calls)))
  (let [now (atom 0)
        calls (atom 0)
        p (cs/expiring-probe (fn [] (swap! calls inc) true) {:ttl-ms 10 :clock #(deref now)})]
    (is (true? (p)))
    (is (nil? (cs/record-answer! p false)))
    (is (false? (p)) "a recorded answer stands until the ttl")
    (reset! now 100)
    (is (true? (p)) "after the ttl the probe runs again")
    (is (= 2 @calls)))
  (is (nil? (cs/record-answer! (constantly true) false)) "a 0-arg probe cannot record")
  (is (= ["x"] (cs/child-argv (constantly false) ["x"] {:path "/bin"})))
  (is (boolean? (cs/systemd-probe))))

(deftest child-scope-start-runs-a-plain-command
  (let [p (cs/start! (ProcessBuilder. ^java.util.List ["true"]) {:probe (constantly false)})]
    (is (instance? Process p))
    (is (zero? (.waitFor ^Process p)))))

;;; ---------------------------------------------------------------------------
;;; sweep coordinator: status only, never started here
;;; ---------------------------------------------------------------------------

(deftest coordinator-status-and-idempotent-stop
  (reg/reset-all!)
  (reg/register-sweep! (sweep "c/s" 5))
  (let [st (sc/status)]
    (is (m/validate sc/CoordinatorStatus st))
    (is (= 10 (:heartbeat-s st)) "the cadence is floored at 10 s")
    (is (= ["c/s"] (:registered st))))
  (when-not (:running? (sc/status))
    (is (false? (sc/stop!)) "stop! of a stopped coordinator is a no-op")))

;;; ---------------------------------------------------------------------------
;;; in-core shutdown hooks
;;; ---------------------------------------------------------------------------

(golden/deftest-golden in-core-hooks-golden
  "test/golden/hive-mcp/system/in-core-hooks.edn"
  (do
    (reg/reset-all!)
    {:ret (in-core/register-in-core-shutdown! (atom nil) (atom {}))
     :snapshot (reg/registry-snapshot)}))

;;; ---------------------------------------------------------------------------
;;; sweepers
;;; ---------------------------------------------------------------------------

(def ^:private gib (* 1024 1024 1024))

(def ^:private HeapIn
  [:map
   [:committed [:maybe [:enum 0 gib (* 3 gib) (* 5 gib) (* 9 gib) (* 12 gib)]]]
   [:used [:maybe [:enum 0 gib (* 3 gib) (* 4 gib) (* 10 gib)]]]])

(deftrifecta-from-schema reclaimable-is-the-empty-committed-heap
  #'hp/reclaimable
  {:in HeapIn
   :out [:int {:min 0}]
   :rel (fn [{:keys [committed used]} out] (= out (max 0 (- (or committed 0) (or used 0)))))
   :golden-path "test/golden/hive-mcp/system/heap-reclaimable.edn"
   :num-tests 100})

(deftrifecta-from-schema worth-collecting-needs-both-thresholds
  #'hp/worth-collecting?
  {:in HeapIn
   :out :boolean
   :rel (fn [{:keys [committed used]} out]
          (let [c (or committed 0) u (or used 0)]
            (= out (and (>= (- c u) hp/min-waste-bytes)
                        (> c (* hp/min-waste-ratio (max u 1)))))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :classify-floor 5
   :golden-path "test/golden/hive-mcp/system/heap-worth-collecting.edn"
   :num-tests 200})

(deftest heap-snapshot-reads-the-runtime
  (let [s (hp/heap-snapshot)]
    (is (m/validate hp/HeapSnap s))
    (is (<= (:used s) (:committed s)))))

(deftest stale-cache-evictor-registry-is-a-set
  (let [f (fn [] 0)
        before (stale/registered-evictors)]
    (is (contains? (stale/register-cache-evictor! f) f))
    (is (= (stale/registered-evictors) (stale/register-cache-evictor! f)) "idempotent")
    (is (set? before))))

;;; ---------------------------------------------------------------------------
;;; Coverage gate
;;; ---------------------------------------------------------------------------

(coverage/deftest-contract-coverage system-contracts
  {:paths ["src/hive_mcp/system"]})

(deftest system-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths ["src/hive_mcp/system"]})]
    (is (= 48 (count (:universe report))) "every public defn of the tree")
    (is (= 48 (count (:covered report))))))

(deftest every-system-contract-generates-both-sides
  (doseq [ns-sym '[hive-mcp.system.registry hive-mcp.system.addon-hot hive-mcp.system.keepalive
                   hive-mcp.system.layer3 hive-mcp.system.layer5 hive-mcp.system.child-scope
                   hive-mcp.system.sweep-coordinator hive-mcp.system.shutdown.in-core
                   hive-mcp.system.sweepers.heap-pressure hive-mcp.system.sweepers.stale-cache]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))
