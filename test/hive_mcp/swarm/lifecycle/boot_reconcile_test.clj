(ns hive-mcp.swarm.lifecycle.boot-reconcile-test
  "Facets for boot reconciliation, through the 1-arity's injected registry
   ports (no swarm library, no with-redefs).

   The rows are generated from a schema that reaches every branch of
   spare?: the probe tags (:verified / :unverified / :absent), a registered
   terminal spawn-mode, and an OS pid that is ALIVE (this test JVM's own),
   dead or absent. The stub update! port records what was retired, so the
   relation checks both the counts and WHICH rows were zombified, in order,
   with what. purge-test-junk! reads the store directly (no port), so offline
   it answers the shim's addon-missing; test-swarm/ drives it."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.swarm.lifecycle.boot-reconcile :as br]
            [hive-mcp.test.shim-facets :as sf]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private live-pid
  "A pid that is alive for the whole test: this JVM."
  (.pid (java.lang.ProcessHandle/current)))

(def ^:private dead-pid 2147483000)

(def ^:private Row
  "A rehydrated slave row, as the store reads it back."
  [:map
   [:slave/id [:enum "s1" "s2" "s3" "s4" "s5"]]
   [:slave/status [:enum :working :idle :zombie :terminated :error]]
   [:slave/alive? {:optional true} :boolean]
   [:slave/liveness {:optional true} [:enum :verified :unverified :absent]]
   [:ling/spawn-mode {:optional true} [:enum :vterm :headless :claude]]
   [:slave/process-pid {:optional true} [:enum live-pid dead-pid]]])

(def ^:private Scenario
  "Data for one run: the rows load-slaves answers and the registered terminal
   modes."
  [:map [:rows [:vector {:max 6} Row]] [:terminals [:set [:enum :vterm :claude]]]])

(defn- run!
  "Drive reconcile-rehydrated-slaves! with stub ports over SCENARIO; answer
   its result and the [id updates] the update! port was handed, in order."
  [{:keys [rows terminals]}]
  (let [retired (atom [])
        result (br/reconcile-rehydrated-slaves!
                {:load-slaves (constantly rows)
                 :update! (fn [id updates] (swap! retired conj [id (dissoc updates :slave/status-changed-at)]))
                 :terminal-modes (constantly terminals)})]
    {:result result :retired @retired}))

(defn- oracle
  "The docstring's rule: a live-looking row is retired unless the probe
   verified or could not judge it, its spawn-mode is a registered terminal, or
   its pid is alive."
  [{:keys [rows terminals]}]
  (let [live? #(and (not (false? (:slave/alive? %)))
                    (not (contains? #{:zombie :terminated :error} (:slave/status %))))
        spare? #(or (contains? #{:verified :unverified} (:slave/liveness %))
                    (contains? terminals (:ling/spawn-mode %))
                    (= live-pid (:slave/process-pid %)))
        live (filter live? rows)
        retire (remove spare? live)]
    {:result {:reconciled (count retire)
              :spared (- (count live) (count retire))
              :skipped (- (count rows) (count live))
              :total (count rows)}
     :retired (mapv (fn [r] [(:slave/id r) {:slave/status :zombie :slave/alive? false}]) retire)}))

(defn- scenario->ports
  "ReconcilePorts over SCENARIO, the scenario kept on the map's metadata so
   the relation can read it back. update! is a no-op here; which rows it is
   handed is pinned by the golden and the mutation test below."
  [{:keys [rows terminals] :as scenario}]
  (with-meta {:load-slaves (constantly rows)
              :update! (constantly nil)
              :terminal-modes (constantly terminals)}
    {::scenario scenario}))

(def ^:private GeneratedPorts
  "The contract's ReconcilePorts, generated from a Scenario."
  [:and {:gen/fmap scenario->ports :gen/schema Scenario} br/ReconcilePorts])

(deftrifecta-from-schema reconcile-retires-exactly-the-unbacked-live-rows
  hive-mcp.swarm.lifecycle.boot-reconcile/reconcile-rehydrated-slaves!
  {:in [:cat GeneratedPorts]
   :out br/ReconcileResult
   :rel (fn [[ports] out] (= out (:result (oracle (::scenario (meta ports))))))
   :classify (fn [_ out]
               (cond (pos? (:reconciled out)) :retired
                     (pos? (:spared out)) :spared-only
                     :else :nothing-live))
   :classify-domain #{:retired :spared-only :nothing-live}
   :classify-floor 5
   :num-tests 300})

(deftest-golden reconcile-scenarios-golden
  "test/golden/hive-mcp/swarm/lifecycle/boot-reconcile.edn"
  {:empty (:result (run! {:rows [] :terminals #{}}))
   :one-headless (run! {:rows [{:slave/id "s1" :slave/status :working}] :terminals #{}})
   :mixed (run! {:rows [{:slave/id "s1" :slave/status :idle :ling/spawn-mode :vterm}
                        {:slave/id "s2" :slave/status :zombie}
                        {:slave/id "s3" :slave/status :working :slave/liveness :unverified}
                        {:slave/id "s4" :slave/status :working :slave/alive? false}
                        {:slave/id "s5" :slave/status :working :slave/liveness :absent}]
                 :terminals #{:vterm}})
   :unregistered-terminal (run! {:rows [{:slave/id "s1" :slave/status :idle :ling/spawn-mode :vterm}]
                                 :terminals #{}})})

(deftest a-live-pid-spares-and-a-dead-pid-does-not
  (is (= {:result {:reconciled 1 :spared 1 :skipped 0 :total 2}
          :retired [["s2" {:slave/status :zombie :slave/alive? false}]]}
         (run! {:rows [{:slave/id "s1" :slave/status :working :slave/process-pid live-pid}
                       {:slave/id "s2" :slave/status :working :slave/process-pid dead-pid}]
                :terminals #{}}))))

(deftest reconciliation-is-idempotent
  (let [rows [{:slave/id "s1" :slave/status :working} {:slave/id "s2" :slave/status :idle :ling/spawn-mode :vterm}]
        {:keys [retired]} (run! {:rows rows :terminals #{:vterm}})
        after (mapv (fn [r] (if-let [[_ u] (first (filter #(= (:slave/id r) (first %)) retired))] (merge r u) r)) rows)]
    (is (= 0 (:reconciled (:result (run! {:rows after :terminals #{:vterm}})))))))

(def ^:private reconcile-original
  "The un-mutated subject, captured at load, so a mutant can delegate to it."
  br/reconcile-rehydrated-slaves!)

(hmut/deftest-mutations reconcile-mutations
  hive-mcp.swarm.lifecycle.boot-reconcile/reconcile-rehydrated-slaves!
  [["retires nothing" (fn [ports] (let [rows ((:load-slaves ports))]
                                    {:reconciled 0 :spared 0 :skipped 0 :total (count rows)}))]
   ["ignores registered terminals"
    (fn [ports] (reconcile-original (assoc ports :terminal-modes (constantly #{}))))]
   ["retires the already-dead too"
    (fn [ports] (reconcile-original
                 (update ports :load-slaves
                         (fn [load] #(mapv (fn [r] (assoc r :slave/status :working :slave/alive? true)) (load))))))]]
  (fn []
    (is (= {:result {:reconciled 1 :spared 1 :skipped 1 :total 3}
            :retired [["s1" {:slave/status :zombie :slave/alive? false}]]}
           (run! {:rows [{:slave/id "s1" :slave/status :working}
                         {:slave/id "s2" :slave/status :idle :ling/spawn-mode :vterm}
                         {:slave/id "s3" :slave/status :terminated}]
                  :terminals #{:vterm}})))))

(deftest purge-test-junk-needs-the-swarm-library
  (if (sf/offline?)
    (is (= {:ns "hive-agent.swarm.bootstrap.noop" :var "make-noop-bootstrap"}
           (sf/call 'hive-mcp.swarm.lifecycle.boot-reconcile 'purge-test-junk! [])))
    (is (map? (br/purge-test-junk!)))))
