(ns hive-mcp.swarm.store-contract-coverage-test
  "Gate: every public fn under the swarm store directories (datalevin,
   datascript, datascript/coordination, ledger, lifecycle, logic) carries an
   `m/=>` contract or a REASONED exemption, and no exemption is stale.

   Almost every namespace there is a compat shim over hive-agent or
   hive-datascript, so their trifectas live in the swarm suite
   (test-swarm/hive_mcp/swarm/store_*_test.clj), where the addon is on the
   classpath. What runs here without the addon: the gate, every contract
   generating both sides, the shims failing loudly when the library is absent,
   and a trifecta over boot-reconcile, whose ports are injected."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.swarm.datalevin.connection :as dl-conn]
            [hive-mcp.swarm.datalevin.driver]
            [hive-mcp.swarm.datascript.connection :as ds-conn]
            [hive-mcp.swarm.datascript.coordination]
            [hive-mcp.swarm.datascript.coordination.cleanup]
            [hive-mcp.swarm.datascript.coordination.config]
            [hive-mcp.swarm.datascript.coordination.coordinator]
            [hive-mcp.swarm.datascript.coordination.session-registry]
            [hive-mcp.swarm.datascript.coordination.wrap-queue]
            [hive-mcp.swarm.datascript.lings]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.swarm.datascript.registry]
            [hive-mcp.swarm.datascript.schema :as s]
            [hive-mcp.swarm.delegate :as delegate]
            [hive-mcp.swarm.ledger.default :as ledger]
            [hive-mcp.swarm.lifecycle.boot-reconcile :as br]
            [hive-mcp.swarm.lifecycle.sweep]
            [hive-mcp.swarm.lifecycle.terminal-sweep]
            [hive-mcp.swarm.logic.predicates]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private paths
  ["src/hive_mcp/swarm/datalevin"
   "src/hive_mcp/swarm/datascript"
   "src/hive_mcp/swarm/ledger"
   "src/hive_mcp/swarm/lifecycle"
   "src/hive_mcp/swarm/logic"])

(def ^:private exempt
  {'hive-mcp.swarm.datalevin.driver/q
   "Passthrough to datalevin.core/q: the result shape is chosen by the
    query's :find spec (a set of tuples, a scalar, a collection, a pulled map),
    so no schema states the domain; its argument list is query-then-inputs,
    which is datalevin's own contract."})

(coverage/deftest-contract-coverage swarm-store-contracts-gate
  {:paths paths
   :exempt exempt})

(deftest swarm-store-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths paths :exempt exempt})]
    (is (= 138 (count (:universe report))) "every public defn of the six directories")
    (is (= 137 (count (:covered report))) "contracted: all but the one exemption")
    (is (= #{'hive-mcp.swarm.datalevin.driver/q} (set (keys (:exempt report)))))))

(def ^:private store-nses
  '[hive-mcp.swarm.datalevin.connection hive-mcp.swarm.datalevin.driver
    hive-mcp.swarm.datascript.connection hive-mcp.swarm.datascript.lings
    hive-mcp.swarm.datascript.queries hive-mcp.swarm.datascript.registry
    hive-mcp.swarm.datascript.schema
    hive-mcp.swarm.datascript.coordination.cleanup
    hive-mcp.swarm.datascript.coordination.config
    hive-mcp.swarm.datascript.coordination.coordinator
    hive-mcp.swarm.datascript.coordination.session-registry
    hive-mcp.swarm.datascript.coordination.wrap-queue
    hive-mcp.swarm.ledger.default hive-mcp.swarm.lifecycle.boot-reconcile
    hive-mcp.swarm.lifecycle.sweep hive-mcp.swarm.lifecycle.terminal-sweep
    hive-mcp.swarm.logic.predicates])

(deftest every-store-contract-generates-both-sides
  (doseq [ns-sym store-nses
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (let [sch (m/schema schema)]
                  (if (= :function (m/type sch)) (m/children sch) [sch]))
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

(deftest store-value-schemas-generate
  (doseq [sch [s/Conn s/TxReport s/MaybeTxReport s/EntityId s/SlaveStatus s/TaskStatus
               s/CriticalOp s/CoordinatorStatus s/Inst s/StringColl s/Slave s/Task
               s/ClaimInfo s/ClaimRow s/ClaimHistory s/Count s/Atom s/Threshold
               s/IncludeStaleArgs s/CompletedTaskArgs s/ClaimHistoryArgs s/SessionArgs
               s/ThresholdOpts s/StdoutLine s/Coordinator s/CompletedTask s/KanbanMovement
               s/WrapRow]]
    (is (seq (mg/sample sch {:size 4})) (pr-str (m/form sch)))))

(deftest kwargs-tails-accept-pairs-and-a-trailing-map
  (testing "a & {:keys} tail is empty, alternating pairs, or one map"
    (is (m/validate [:cat s/IncludeStaleArgs] []))
    (is (m/validate [:cat s/IncludeStaleArgs] [:include-stale? true]))
    (is (m/validate [:cat s/IncludeStaleArgs] [{:include-stale? true}]))
    (is (not (m/validate [:cat s/IncludeStaleArgs] [:include-stale?])) "an odd tail is refused")
    (is (not (m/validate [:cat s/IncludeStaleArgs] [:unknown 1])) "an undeclared key is refused")))

(deftest shims-fail-loudly-without-the-library
  (if (delegate/available? "hive-datascript.swarm.queries")
    (is true "the swarm library is on the classpath; the swarm suite drives the shims")
    (doseq [call [#(queries/get-slave "s") #(ds-conn/ensure-conn) #(dl-conn/get-db-path)
                  #(ledger/store)]]
      (let [e (try (call) nil (catch clojure.lang.ExceptionInfo e e))]
        (is (true? (:swarm/addon-missing (ex-data e))))))))

;;; ---------------------------------------------------------------------------
;;; boot-reconcile: the 1-arity takes its registry ports, so it runs here
;;; ---------------------------------------------------------------------------

(def ^:private SlaveRow
  "A rehydrated row as the store reads it back. The pid is nil (unknown, so
   never treated as alive) and the liveness probe tag is the spare signal."
  [:map
   [:slave/id [:enum "s1" "s2" "s3" "s4"]]
   [:slave/status [:enum :working :idle :zombie :terminated :error]]
   [:slave/alive? {:optional true} :boolean]
   [:slave/liveness {:optional true} [:enum :verified :unverified :absent]]
   [:ling/spawn-mode {:optional true} [:enum :vterm :headless :claude]]])

(defn- run-reconcile
  "Drive reconcile with stub ports over ROWS and TERMINALS; answer the result
   and the ids it retired, in order."
  [rows terminals]
  (let [retired (atom [])]
    {:result (br/reconcile-rehydrated-slaves!
              {:load-slaves (constantly rows)
               :update! (fn [id _] (swap! retired conj id))
               :terminal-modes (constantly terminals)})
     :retired @retired}))

(defn- live-looking? [row]
  (and (not (false? (:slave/alive? row)))
       (not (contains? br/dead-statuses (:slave/status row)))))

(defn- spared? [terminals row]
  (or (contains? #{:verified :unverified} (:slave/liveness row))
      (contains? terminals (:ling/spawn-mode row))))

(hst/deftrifecta-from-schema reconcile-retires-only-unbacked-live-rows
  hive-mcp.swarm.lifecycle.boot-reconcile/reconcile-rehydrated-slaves!
  {:in        [:map
               [:load-slaves [:enum (constantly [])
                              (constantly [{:slave/id "s1" :slave/status :working}])
                              (constantly [{:slave/id "s1" :slave/status :idle :ling/spawn-mode :vterm}
                                           {:slave/id "s2" :slave/status :zombie}
                                           {:slave/id "s3" :slave/status :working :slave/liveness :unverified}
                                           {:slave/id "s4" :slave/status :working :slave/alive? false}])]]
               [:update! [:enum (constantly nil)]]
               [:terminal-modes [:enum (constantly #{}) (constantly #{:vterm})]]]
   :out       [:map [:reconciled s/Count] [:spared s/Count] [:skipped s/Count] [:total s/Count]]
   :rel       (fn [{:keys [load-slaves terminal-modes]} out]
                (let [rows (load-slaves)
                      live (filter live-looking? rows)
                      spare (filter (partial spared? (terminal-modes)) live)]
                  (= out {:reconciled (- (count live) (count spare))
                          :spared (count spare)
                          :skipped (- (count rows) (count live))
                          :total (count rows)})))
   ;; the ports are fns, which EDN cannot snapshot; the golden below pins
   ;; the same scenarios as data
   :num-tests 60})

(deftest reconcile-golden-scenarios
  (testing "each arm, driven through the injected ports"
    (is (= {:result {:reconciled 0 :spared 0 :skipped 0 :total 0} :retired []}
           (run-reconcile [] #{})))
    (is (= {:result {:reconciled 1 :spared 0 :skipped 0 :total 1} :retired ["s1"]}
           (run-reconcile [{:slave/id "s1" :slave/status :working}] #{})))
    (is (= {:result {:reconciled 0 :spared 2 :skipped 2 :total 4} :retired []}
           (run-reconcile [{:slave/id "s1" :slave/status :idle :ling/spawn-mode :vterm}
                           {:slave/id "s2" :slave/status :zombie}
                           {:slave/id "s3" :slave/status :working :slave/liveness :unverified}
                           {:slave/id "s4" :slave/status :working :slave/alive? false}]
                          #{:vterm})))
    (is (= {:result {:reconciled 1 :spared 1 :skipped 0 :total 2} :retired ["s1"]}
           (run-reconcile [{:slave/id "s1" :slave/status :idle :ling/spawn-mode :vterm}
                           {:slave/id "s3" :slave/status :working :slave/liveness :verified}]
                          #{})))))

(deftest reconcile-row-schema-generates
  (is (every? #(m/validate SlaveRow %) (mg/sample SlaveRow {:size 5}))))
