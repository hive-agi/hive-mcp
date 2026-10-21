(ns hive-mcp.system.contract-test
  "Contract facets for the system registry and child-scope, and the coverage
   gate for the whole system tree. The other system namespaces carry their
   facets in their own <ns>-contract-test (addon-hot, keepalive, layer3,
   layer5, sweep-coordinator, shutdown.in-core, sweepers.heap-pressure,
   sweepers.stale-cache) or <ns>-test (sweepers.orphan-edge). Pure
   calculations get schema-driven trifectas; boundary fns get goldens over
   plain-EDN projections of their results, with every process-global registry
   they drive captured and restored around the test. No scheduler is started
   and no JVM shutdown hook is registered."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.protocols.lifecycle :as lifecycle]
            [hive-mcp.system.addon-hot]
            [hive-mcp.system.child-scope :as cs]
            [hive-mcp.system.keepalive]
            [hive-mcp.system.layer3]
            [hive-mcp.system.layer5]
            [hive-mcp.system.registry :as reg]
            [hive-mcp.system.shutdown.in-core]
            [hive-mcp.system.sweep-coordinator]
            [hive-mcp.system.sweepers.heap-pressure]
            [hive-mcp.system.sweepers.orphan-edge]
            [hive-mcp.system.sweepers.stale-cache]
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
;;; Coverage gate
;;; ---------------------------------------------------------------------------

(coverage/deftest-contract-coverage system-contracts
  {:paths ["src/hive_mcp/system"]})

(deftest system-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths ["src/hive_mcp/system"]})]
    (is (= 55 (count (:universe report))) "every public defn of the tree")
    (is (= 55 (count (:covered report))))))

(deftest every-system-contract-generates-both-sides
  (doseq [ns-sym '[hive-mcp.system.registry hive-mcp.system.addon-hot hive-mcp.system.keepalive
                   hive-mcp.system.layer3 hive-mcp.system.layer5 hive-mcp.system.child-scope
                   hive-mcp.system.sweep-coordinator hive-mcp.system.shutdown.in-core
                   hive-mcp.system.sweepers.heap-pressure hive-mcp.system.sweepers.stale-cache
                   hive-mcp.system.sweepers.orphan-edge]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))
