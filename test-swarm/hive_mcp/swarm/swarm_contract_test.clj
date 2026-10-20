(ns hive-mcp.swarm.swarm-contract-test
  "Contract facets for the top-level hive-mcp.swarm namespaces (batch m21):
   delegate, digest, logic, event-bridge, registry, coordinator, callback,
   datascript and sync. They run in test-swarm/, because logic, digest and
   the store wrappers delegate to hive-agent / hive-datascript.

   Every test owns its state: the swarm store, the logic db, the coordinator
   queue and the callback registry are reset around each test. No real agent
   is spawned or dispatched to: the sync probe and the bootstrap are injected,
   and the callback tests never fire a callback.

   Also the coverage gate for the top-level src/hive_mcp/swarm/*.clj files
   (the subdirectories have their own gates; datalevin, datascript/, ledger,
   lifecycle and logic/ belong to batch m22)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.swarm.bootstrap.noop :as noop]
            [hive-mcp.swarm.bootstrap.protocol :as bp]
            [hive-mcp.swarm.callback :as callback]
            [hive-mcp.swarm.coordinator :as coord]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.datascript.connection :as conn]
            [hive-mcp.swarm.delegate :as delegate]
            [hive-mcp.swarm.digest :as digest]
            [hive-mcp.swarm.event-bridge :as bridge]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.swarm.registry :as reg]
            [hive-mcp.swarm.sync :as sync]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :as hst]
            [hive-test.golden :as golden]
            [hive-test.mutation :as mut]
            [malli.core :as m]
            [malli.generator :as mg]
            [hive-mcp.swarm.addon-gate :as gate]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- reset-all! []
  (when (gate/present?)
    (conn/reset-conn!)
    (logic/reset-db!))
  (coord/clear-queue!)
  (doseq [tid (keys (callback/registrations))] (callback/unregister! tid)))

(use-fixtures :each (fn [f] (reset-all!) (try (f) (finally (reset-all!)))))

;;; ---------------------------------------------------------------------------
;;; delegate
;;; ---------------------------------------------------------------------------

(deftest delegate-resolution-test
  (is (true? (delegate/available? "hive-mcp.swarm.delegate")))
  (is (false? (delegate/available? "no.such.namespace-m21")))
  (let [e (delegate/addon-missing "hive-agent.swarm.x" 'f)]
    (is (m/validate delegate/AddonMissing e))
    (is (= {:swarm/addon-missing true :addon "hive-agent" :ns "hive-agent.swarm.x" :var "f"}
           (ex-data e))))
  (let [stand-in (delegate/resolve-var "no.such.namespace-m21" 'f)]
    (is (nil? @stand-in))
    (is (= :caught (try (stand-in) (catch clojure.lang.ExceptionInfo _ :caught))))))

;;; ---------------------------------------------------------------------------
;;; digest (pure)
;;; ---------------------------------------------------------------------------

(def ^:private EntryIn
  [:map [:data [:map [:messages [:vector {:max 3}
                                 [:map [:timestamp [:int {:min 0 :max 9000}]]
                                  [:event-type [:enum :progress :completed]]
                                  [:message {:optional true} [:enum "hi" "a long message here"]]
                                  [:parent-id {:optional true} [:enum "coord" "other"]]
                                  [:data {:optional true} [:map [:turn [:int {:min 0 :max 9}]]]]]]]]]])

(hst/deftrifecta-from-schema agent-row-projects-the-latest-shout
  hive-mcp.swarm.digest/agent-row
  {:in [:cat [:enum 10000] [:enum "ling-a" "ling-b"] EntryIn [:enum 5 200]]
   :out [:maybe digest/Row]
   :rel (fn [[now id entry _] out]
          (let [msgs (get-in entry [:data :messages])]
            (if (empty? msgs)
              (nil? out)
              (and (= id (:a out))
                   (= (count msgs) (:shouts out))
                   (= (max 0 (quot (- now (:timestamp (peek msgs))) 1000)) (:idle-s out))))))
   :classify (fn [_ out] (if out :row :none))
   :classify-domain #{:row :none}
   :golden-path "test/golden/hive-mcp/swarm/digest-agent-row.edn"})

(hst/deftrifecta-from-schema roster-sorts-by-idleness
  hive-mcp.swarm.digest/roster
  {:in [:cat [:enum 10000] [:map-of {:max 3} [:enum "a" "b" "c"] EntryIn]]
   :out [:vector digest/Row]
   :rel (fn [[_ registry] out]
          (and (= (count out) (count (filter #(seq (get-in (val %) [:data :messages])) registry)))
               (= out (vec (sort-by :idle-s out)))))
   :num-tests 100})

(golden/deftest-golden digest-render-golden
  "test/golden/hive-mcp/swarm/digest-render.edn"
  (let [reg {"a" {:data {:messages [{:timestamp 1000 :event-type :progress :message "hi"
                                     :parent-id "coord" :data {:turn 3}}]}}
             "b" {:data {:messages [{:timestamp 9000 :event-type :completed}]}}
             "c" {:data {}}}]
    {:roster   (digest/roster 10000 reg)
     :children (digest/roster 10000 reg {:only-children-of "coord"})
     :render   (digest/render (digest/roster 10000 reg))}))

;;; ---------------------------------------------------------------------------
;;; logic (core.logic store, reset per test)
;;; ---------------------------------------------------------------------------

(defn- logic-scenario []
  (logic/reset-db!)
  (logic/add-slave! "s1" :active)
  (logic/add-claim! "f1" "s1")
  (logic/add-task-file! "t1" "f1")
  (let [r {:exists [(logic/slave-exists? "s1") (logic/slave-exists? "s2")]
           :conflicts-other (vec (logic/check-file-conflicts "s2" ["f1" "f2"]))
           :conflicts-self (vec (logic/check-file-conflicts "s1" ["f1"]))
           :conflicts-none (logic/check-file-conflicts "s2" [])
           :claim (logic/get-claim-for-file "f1")
           :all (logic/get-all-claims)
           :files (logic/get-files-for-task "t1")
           :stats (logic/db-stats)
           :deadlock (logic/check-would-deadlock "t1" "t2")}
        _ (logic/release-claims-for-task! "t1")
        after {:claim-after-task-release (logic/get-claim-for-file "f1")
               :release-unclaimed (logic/release-claim-for-file! "f1")}]
    (merge r after)))

(golden/deftest-golden logic-golden
  "test/golden/hive-mcp/swarm/logic.edn"
  (logic-scenario))

(mut/deftest-mutations check-file-conflicts-mutants-caught
  hive-mcp.swarm.logic/check-file-conflicts
  [["never-conflicts" (fn [_ _] ())]
   ["self-conflicts" (fn [_ files] (map (fn [f] {:file f :held-by "s1"}) files))]]
  (fn []
    (let [{:keys [conflicts-other conflicts-self]} (logic-scenario)]
      (is (= [{:file "f1" :held-by "s1"}] conflicts-other))
      (is (= [] conflicts-self)))))

;;; ---------------------------------------------------------------------------
;;; event-bridge
;;; ---------------------------------------------------------------------------

(hst/deftrifecta-from-schema slave-subject-is-the-hive-v1-subject
  hive-mcp.swarm.event-bridge/slave-subject
  {:in [:cat [:enum :slave-spawned :task-failed "slave-ready"] [:enum "ling-a" nil]]
   :out :string
   :rel (fn [[et id] out] (= out (str "hive.v1.slave." (name et) "." (or id "unknown"))))
   :golden-path "test/golden/hive-mcp/swarm/slave-subject.edn"
   :n-cases 6})

(deftest bridge-degrades-without-a-backbone-test
  (is (m/validate bridge/BridgeState (bridge/bridge-status)))
  (when-not (:running (bridge/bridge-status))
    (is (false? (bridge/start-nats-bridge!)) "no connected backbone in a test JVM")
    (is (nil? (bridge/stop-nats-bridge!))))
  (is (= :caught (try (bridge/publish-slave-event! {:slave-id "x"}) (catch clojure.lang.ExceptionInfo _ :caught)))))

;;; ---------------------------------------------------------------------------
;;; registry wrappers over the swarm store
;;; ---------------------------------------------------------------------------

(defn- registry-scenario []
  (reset-all!)
  (reg/add-slave! "s1" {:name "one" :status :idle})
  (reg/add-slave! "s2" {})
  (reg/add-task! "t1" "s1" {:status :dispatched :files ["/f.clj"]})
  (reg/claim-file! "/f.clj" "s1" "t1")
  (let [r {:exists [(reg/slave-exists? "s1") (reg/slave-exists? "zz")]
           :slave (select-keys (reg/get-slave "s1") [:slave/id :slave/name :slave/status])
           :ids (sort (reg/list-slaves {:ids-only? true}))
           :idle (sort (map :slave/id (reg/list-slaves {:status :idle})))
           :task (select-keys (reg/get-task "t1") [:task/id :task/status :task/files])
           :update-missing (reg/update-slave! "zz" {:slave/status :working})
           :can-kill (reg/can-kill? "s1")
           ;; BUG pinned: the wrappers forward their args in the wrong order
           :has-conflict (reg/has-conflict? ["/f.clj"] "s2")
           :check-file-conflicts (vec (reg/check-file-conflicts ["/f.clj"] "s2"))
           :stats (select-keys (reg/db-stats) [:slaves :tasks :claims])}
        _ (reg/remove-slave! "s2")]
    (assoc r :after-remove (sort (reg/list-slaves {:ids-only? true}))
           :remove-missing (reg/remove-slave! "zz"))))

(golden/deftest-golden registry-golden
  "test/golden/hive-mcp/swarm/registry.edn"
  (registry-scenario))

(deftest registry-arg-order-bug-is-pinned-test
  (testing "has-conflict? never sees a conflict on a held file (BUG, see contract)"
    (let [{:keys [has-conflict check-file-conflicts]} (registry-scenario)]
      (is (= {:conflict? false} has-conflict))
      (is (= [] check-file-conflicts)))))

;;; ---------------------------------------------------------------------------
;;; coordinator queue
;;; ---------------------------------------------------------------------------

(hst/deftrifecta-from-schema extract-files-finds-paths
  hive-mcp.swarm.coordinator/extract-files-from-prompt
  {:in [:or [:enum "edit src/foo.clj please" "look at `a/b.cljs` and \"c/d.edn\""
             "see /abs/path/x.clj and ./rel/y.clj" "visit http://x.io/a.html" "nothing here" ""]
        :int :nil]
   :out [:maybe [:vector :string]]
   :rel (fn [in out] (if (string? in) (and (vector? out) (= out (distinct out))) (nil? out)))
   :golden-path "test/golden/hive-mcp/swarm/extract-files.edn"
   :n-cases 9})

(defn- coordinator-scenario []
  (reset-all!)
  (logic/add-slave! "holder" :active)
  (logic/add-claim! "/held.clj" "holder")
  ;; the :blocked arm needs a depends-on fact in the logic db, which no public
  ;; fn of this batch adds; it is pinned by its contract arm instead
  (let [blocked (coord/dispatch-or-queue! {:slave-id "w" :files ["/x.clj"]
                                           :dependencies [["t1" "t2"]]})
        queued  (coord/dispatch-or-queue! {:id "q1" :slave-id "w" :files ["/held.clj"]})
        go      (coord/dispatch-or-queue! {:slave-id "w" :prompt "edit src/free.clj"})
        status  (coord/coordinator-status)
        ready0  (vec (coord/get-ready-tasks))
        _       (logic/release-claim-for-file! "/held.clj")
        ready1  (mapv :id (coord/process-queue!))]
    {:blocked blocked
     :queued (dissoc queued :task-id)
     :dispatch go
     :queue-length (:queue-length status)
     :ready-before ready0
     :ready-after-release ready1
     :queue-after (coord/get-queue)}))

(golden/deftest-golden coordinator-golden
  "test/golden/hive-mcp/swarm/coordinator.edn"
  (coordinator-scenario))

(mut/deftest-mutations dispatch-or-queue!-mutants-caught
  hive-mcp.swarm.coordinator/dispatch-or-queue!
  [["always-dispatch" (fn [{:keys [files]}] {:action :dispatch :files files})]
   ["always-queue" (fn [_] {:action :queued :task-id "x" :conflicts [] :position 1})]]
  (fn []
    (let [{:keys [blocked queued dispatch]} (coordinator-scenario)]
      (is (= :dispatch (:action blocked)) "no cycle: t1 -> t2")
      (is (= :queued (:action queued)))
      (is (= :dispatch (:action dispatch))))))

(deftest pre-flight-and-claims-test
  (logic/add-slave! "holder" :active)
  (logic/add-claim! "/held.clj" "holder")
  (let [pf (coord/pre-flight-check {:slave-id "w" :files ["/held.clj"]})]
    (is (m/validate coord/PreFlight pf))
    (is (false? (:approved? pf))))
  (conn/reset-conn!)
  (sync/get-swarm-registry)
  (reg/add-slave! "w" {})
  (let [r (coord/atomic-claim-files! "t9" "w" ["/tmp/m21-not-a-file.clj"])]
    (is (= {:acquired? true :conflicts [] :files-claimed 1} r)))
  (is (nil? (coord/atomic-claim-files! "t9" "w" [])))
  (is (= {:released? false :reason :no-claim} (coord/contextual-claim-release! "/never-claimed.clj")))
  (let [rel (coord/release-task-claims! "t9")]
    (is (= 1 (:released rel)))))

;;; ---------------------------------------------------------------------------
;;; callback registry (never fires)
;;; ---------------------------------------------------------------------------

(deftest callback-registry-test
  (is (= "task-1" (callback/register! "task-1" {:ling-id "ling-a"})))
  (callback/register! "task-2" {:ling-id "ling-b" :format-fn (fn [t _] (str "done " t))})
  (is (callback/registered? "task-1"))
  (is (= #{"task-1" "task-2"} (set (keys (callback/registrations)))))
  (is (m/validate [:map-of :string callback/Registration] (callback/registrations)))
  (let [st (callback/status)]
    (is (= 2 (:registered-count st))))
  (callback/cleanup-for-agent! "ling-a")
  (is (not (callback/registered? "task-1")))
  (callback/cleanup-stale! 0)
  (Thread/sleep 2)
  (callback/cleanup-stale! 0)
  (is (= {} (callback/registrations)))
  (testing "a nil ling-id is refused (the :pre, or the contract when instrumented)"
    (is (= :caught (try (callback/register! "t" {:ling-id nil})
                        (catch AssertionError _ :caught)
                        (catch clojure.lang.ExceptionInfo _ :caught))))))

;;; ---------------------------------------------------------------------------
;;; datascript facade
;;; ---------------------------------------------------------------------------

(deftest datascript-facade-test
  (is (m/validate ds/SwarmDb (ds/db-handle)))
  (is (m/validate ds/Db (ds/current-db)))
  (reg/add-slave! "s1" {:name "one"})
  (is (= #{["s1"]} (ds/q '[:find ?id :where [_ :slave/id ?id]])))
  (is (= #{["one"]} (ds/q-db (ds/current-db) '[:find ?n :in $ ?id :where [?e :slave/id ?id] [?e :slave/name ?n]] "s1")))
  (is (= {:slave/name "one"} (ds/pull [:slave/name] [:slave/id "s1"])))
  (is (m/validate ds/TxReport (ds/transact! [{:slave/id "s2"}])))
  (ds/listen! ::k (fn [_]))
  (ds/unlisten! ::k))

;;; ---------------------------------------------------------------------------
;;; sync: pure classification and the injected bootstrap path
;;; ---------------------------------------------------------------------------

(def ^:private RowIn
  [:map [:slave-id [:enum "a" "b"]]
   [:spawn-mode {:optional true} [:maybe [:enum :vterm :headless]]]
   [:process-pid {:optional true} [:maybe [:enum 11 22]]]])

(def ^:private EvidenceIn
  "Sparse evidence, so each verdict is reached: sets are mostly empty."
  [:map
   [:emacs [:or [:map [:state [:= :unknown]]] [:map [:state [:= :known]] [:ids [:set {:max 1} [:enum "a" "z"]]]]]]
   [:live-pids [:set {:max 1} [:enum 11 99]]]
   [:live-ids [:set {:max 1} [:enum "b" "z"]]]
   [:terminal-modes [:set {:max 1} [:enum :vterm]]]])

(hst/deftrifecta-from-schema classify-row-only-live-backing-verifies
  hive-mcp.swarm.sync/classify-row
  {:in [:cat RowIn EvidenceIn]
   :out [:map]
   :rel (fn [[row ev] out]
          (let [mode (:spawn-mode row)
                emacs? (or (contains? (:terminal-modes ev) mode) (nil? mode))
                verified (or (contains? (:live-ids ev) (:slave-id row))
                             (and (:process-pid row) (contains? (:live-pids ev) (:process-pid row)))
                             (and emacs? (contains? (get-in ev [:emacs :ids]) (:slave-id row))))]
            (cond verified (= :verified (:liveness out))
                  (and emacs? (= :unknown (get-in ev [:emacs :state]))) (= :unverified (:liveness out))
                  :else (and (= :zombie (:status out)) (false? (:alive? out))))))
   :classify (fn [_ out] (or (:liveness out) :zombie))
   :classify-domain #{:verified :unverified :zombie}
   :classify-floor 10
   :num-tests 300
   :golden-path "test/golden/hive-mcp/swarm/classify-row.edn"})

(hst/deftrifecta-from-schema missing-from-emacs-is-not-member
  hive-mcp.swarm.sync/missing-from-emacs?
  {:in [:cat [:enum "a" "x"] [:set [:enum "a" "b"]]]
   :out :boolean
   :rel (fn [[id ids] out] (= out (not (contains? ids id))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}})

(deftest full-sync-uses-the-injected-ports-test
  (let [registered (atom [])
        bs (reify bp/ISwarmBootstrap
             (-load-slaves [_] [{:slave-id "a" :spawn-mode :headless}
                                {:slave-id "b" :process-pid 22}])
             (-snapshot-slave! [this _ _] this)
             (-forget-slave! [this _] this)
             (-close! [_] nil))
        probe (fn [_] {:emacs {:state :known :ids #{}} :live-pids #{22} :live-ids #{}
                       :terminal-modes #{}})]
    (is (nil? (sync/full-sync-from-bootstrap! bs probe {:reset! (fn [] :reset)
                                                       :register! #(swap! registered conj %)})))
    (is (= [{:slave-id "a" :spawn-mode :headless :status :zombie :alive? false}
            {:slave-id "b" :process-pid 22 :liveness :verified}]
           @registered))))

(deftest sync-injection-slots-test
  (let [old-bs (sync/get-swarm-bootstrap)
        old-hooks (sync/get-hooks-registry)]
    (try
      (is (m/validate bp/Bootstrap old-bs))
      (sync/set-swarm-bootstrap! (noop/make-noop-bootstrap))
      (is (m/validate bp/Bootstrap (sync/get-swarm-bootstrap)))
      (sync/set-hooks-registry! nil)
      (is (nil? (sync/get-hooks-registry)))
      (is (m/validate sync/SwarmRegistry (sync/get-swarm-registry)))
      (is (false? (:running (sync/sync-status))))
      (finally
        (sync/set-swarm-bootstrap! old-bs)
        (sync/set-hooks-registry! old-hooks)))))

;;; ---------------------------------------------------------------------------
;;; schemas and the gate
;;; ---------------------------------------------------------------------------

(def ^:private top-nss
  '[hive-mcp.swarm.delegate hive-mcp.swarm.digest hive-mcp.swarm.logic
    hive-mcp.swarm.event-bridge hive-mcp.swarm.registry hive-mcp.swarm.coordinator
    hive-mcp.swarm.callback hive-mcp.swarm.datascript hive-mcp.swarm.sync])

(deftest every-top-level-contract-generates-both-sides-test
  (doseq [ns-sym top-nss
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          :let [s (m/schema schema)]
          arity (if (= :function (m/type s)) (m/children s) [s])
          side (take 2 (m/children arity))]
    (is (seq (mg/sample side {:size 2 :seed 7})) (str ns-sym "/" fn-sym))))

(deftest store-shims-without-the-addon-test
  (when-not (gate/present?)
    (testing "without the addon every store and logic call raises :swarm/addon-missing"
      (is (gate/addon-missing? #(conn/reset-conn!)))
      (is (gate/addon-missing? #(logic/reset-db!)))
      (is (gate/addon-missing? #(reg/add-slave! "s1" {})))
      (is (gate/addon-missing? #(ds/current-db)))
      (is (gate/addon-missing? #(digest/roster 0 {})))
      (is (gate/addon-missing? #(noop/make-noop-bootstrap))))))

(def ^:private offline?
  "The tests that never reach hive-agent or hive-datascript: the delegate,
   the pure event-bridge, coordinator and sync fns, the callback registry,
   the sync path over injected ports, and the schema and coverage checks."
  (gate/offline #{"delegate-resolution-test" "bridge-degrades-without-a-backbone-test"
                  "callback-registry-test" "full-sync-uses-the-injected-ports-test"
                  "store-shims-without-the-addon-test"
                  "every-top-level-contract-generates-both-sides-test"
                  "swarm-top-level-contracts-gate"}
                ["slave-subject-is-the-hive-v1-subject" "extract-files-finds-paths"
                 "classify-row-only-live-backing-verifies" "missing-from-emacs-is-not-member"]))

(defn test-ns-hook [] (gate/run 'hive-mcp.swarm.swarm-contract-test offline?))

(deftest-contract-coverage swarm-top-level-contracts-gate
  {:paths ["src/hive_mcp/swarm/callback.clj" "src/hive_mcp/swarm/coordinator.clj"
           "src/hive_mcp/swarm/datascript.clj" "src/hive_mcp/swarm/delegate.clj"
           "src/hive_mcp/swarm/digest.clj" "src/hive_mcp/swarm/event_bridge.clj"
           "src/hive_mcp/swarm/logic.clj" "src/hive_mcp/swarm/protocol.clj"
           "src/hive_mcp/swarm/registry.clj" "src/hive_mcp/swarm/sync.clj"]
   :exempt {}})
