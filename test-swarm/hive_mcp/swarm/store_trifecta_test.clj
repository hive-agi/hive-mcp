(ns hive-mcp.swarm.store-trifecta-test
  "Trifectas and goldens for the swarm DataScript store shims (connection,
   queries, lings, registry, schema), driven by their contracts.

   Runs in the swarm suite: the shims delegate to hive-datascript. Every test
   owns its state. The fixture binds a FRESH in-memory DataScript conn for the
   thread (with-test-conn) and resets the process-global stdout ring buffers
   before and after, so no golden depends on test order or on what another
   suite left in the store. The scenario rows are built once per conn by
   `build!`; timestamps and generated ids are dropped before a snapshot.

   complete-task!, fail-task! and archive-claim-to-history! write through to
   the process-default swarm ledger, which would otherwise open an on-disk
   store. The fixture installs an in-memory recording ledger first and
   re-arms the default (lazy, unopened) on the way out."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.swarm.datascript.connection :as conn]
            [hive-mcp.swarm.datascript.lings :as lings]
            [hive-mcp.swarm.datascript.queries :as q]
            [hive-mcp.swarm.datascript.registry :as registry]
            [hive-mcp.swarm.datascript.schema :as s]
            [hive-mcp.swarm.delegate :as delegate]
            [hive-mcp.swarm.ledger.default :as ledger]
            [hive-spi.swarm.ledger :as ledger-port]
            [hive-schemas.test :as hst]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- build!
  "The fixed scenario: two live slaves (s2 a child of s1, in another project),
   one task with a file and a claim on it, one claim history row."
  []
  (lings/add-slave! "s1" {:name "one" :status :working :cwd "/w" :project-id "p"
                          :presets ["a"]})
  (lings/add-slave! "s2" {:name "two" :parent "s1" :project-id "p2" :kanban-task-id "k-9"})
  (lings/add-task! "t1" "s1" {:prompt "do it" :files ["f.clj"]})
  (lings/claim-file! "f.clj" "s1" {:task-id "t1" :prior-hash "abcdef0123"})
  (lings/archive-claim-to-history! "h.clj" {:slave-id "s1" :lines-added 3 :lines-removed 1}))

(def ^:private ledger-events
  "What the recording ledger received during the current test."
  (atom []))

(defn- recording-ledger []
  (reify ledger-port/ILedgerStore
    (append! [_ e] (swap! ledger-events conj (:type e)) (assoc e :seq (count @ledger-events)))
    (close! [_] nil)))

(def ^:private store-present?
  "Is hive-datascript on this classpath? The swarm library is optional; a
   classpath without it (the public :test:test-swarm build) skips the bodies
   instead of failing on :swarm/addon-missing. The addon-missing refusal
   itself is asserted in test/hive_mcp/swarm/store_contract_coverage_test."
  (delegate/available? "hive-datascript.swarm.lings"))

(declare isolated*)

(defn- isolated
  "Fresh conn for this thread, scenario built, stdout buffers reset and a
   recording ledger installed around it. Skipped when the library is absent."
  [f]
  (if-not store-present?
    (is true "hive-datascript absent: store trifectas skipped")
    (isolated* f)))

(defn- isolated*
  [f]
  (lings/reset-stdout-buffers!)
  (reset! ledger-events [])
  (ledger/set-store! (recording-ledger))
  (try
    (conn/with-test-conn (conn/create-conn) (fn [] (build!) (f)))
    (finally
      (lings/reset-stdout-buffers!)
      (ledger/reset-store!))))

(use-fixtures :each isolated)

(def ^:private SlaveId [:enum "s1" "s2" "zz"])

(defn- strip
  "Drop the wall-clock and random fields a snapshot must not hold."
  [x]
  (cond
    (map? x) (into (sorted-map)
                   (for [[k v] (apply dissoc x [:slave/created-at :slave/last-active-at
                                               :task/started-at :task/completed-at
                                               :created-at :released-at :id])]
                     [k (strip v)]))
    (set? x) (into (sorted-set) (map strip x))
    (sequential? x) (mapv strip x)
    :else x))

;;; ---------------------------------------------------------------------------
;;; queries
;;; ---------------------------------------------------------------------------

(hst/deftrifecta-from-schema get-slave-reads-the-row
  hive-mcp.swarm.datascript.queries/get-slave
  {:in        SlaveId
   :out       [:maybe s/Slave]
   :rel       (fn [id out] (if (= "zz" id) (nil? out) (= id (:slave/id out))))
   :classify  (fn [_ out] (some? out))
   :classify-domain #{true false}
   :num-tests 60})

(hst/deftrifecta-from-schema has-conflict-only-for-another-holder
  hive-mcp.swarm.datascript.queries/has-conflict?
  {:in        [:cat [:enum "f.clj" "g.clj"] SlaveId]
   :out       [:or [:map [:conflict? [:= false]]]
               [:map [:conflict? [:= true]] [:held-by [:maybe :string]]]]
   :rel       (fn [[file who] out]
                (= out (if (and (= "f.clj" file) (not= "s1" who))
                         {:conflict? true :held-by "s1"}
                         {:conflict? false})))
   :classify  (fn [_ out] (:conflict? out))
   :classify-domain #{true false}
   :golden-path "test/golden/hive-mcp/swarm/store/has-conflict.edn"
   :num-tests 60})

(hst/deftrifecta-from-schema child-projects-exclude-the-parent
  hive-mcp.swarm.datascript.queries/get-child-project-ids
  {:in        [:maybe [:enum "p" "p2" "zz"]]
   :out       [:set :string]
   :rel       (fn [pid out] (= out (if (= "p" pid) #{"p2"} #{})))
   :golden-path "test/golden/hive-mcp/swarm/store/child-project-ids.edn"
   :num-tests 30})

(hst/deftrifecta-from-schema check-file-conflicts-lists-foreign-holders
  hive-mcp.swarm.datascript.queries/check-file-conflicts
  {:in        [:cat SlaveId [:maybe [:vector {:max 3} [:enum "f.clj" "g.clj"]]]]
   :out       [:maybe [:sequential [:map [:file :string] [:held-by [:maybe :string]]]]]
   :rel       (fn [[who files] out]
                (if (empty? files)
                  (nil? out)
                  (= (vec out) (vec (for [f files :when (and (= "f.clj" f) (not= "s1" who))]
                                      {:file f :held-by "s1"})))))
   :num-tests 60})

(golden/deftest-golden queries-surface-golden
  "test/golden/hive-mcp/swarm/store/queries-surface.edn"
  {:slave (strip (q/get-slave "s1"))
   :by-name (:slave/id (q/get-slave-by-name "two"))
   :by-name-or-id (:slave/id (q/get-slave-by-name-or-id "agent:s2"))
   :by-kanban (:slave/id (q/get-slave-by-kanban-task "k-9"))
   :all (sort (map :slave/id (q/get-all-slaves)))
   :all-kw-map (sort (map :slave/id (q/get-all-slaves {:include-stale? true})))
   :by-status (map :slave/id (q/get-slaves-by-status :working))
   :by-project (map :slave/id (q/get-slaves-by-project "p" :include-stale? true))
   :ids-by-project (q/get-slave-ids-by-project "p2")
   :task (strip (q/get-task "t1"))
   :tasks (strip (q/get-tasks-for-slave "s1"))
   :tasks-unknown (q/get-tasks-for-slave "zz")
   :claim (strip (q/get-claims-for-file "f.clj"))
   :claims (strip (q/get-all-claims))
   :history (strip (q/get-recent-claim-history :slave-id "s1"))
   :completed (q/get-completed-tasks :limit 5)
   :stats (q/db-stats)
   :dump-keys (sort (keys (q/dump-db)))})

;;; ---------------------------------------------------------------------------
;;; lings: critical ops, claims, stdout ring buffer
;;; ---------------------------------------------------------------------------

(hst/deftrifecta-from-schema can-kill-follows-critical-ops
  hive-mcp.swarm.datascript.lings/can-kill?
  {:in        SlaveId
   :out       [:or [:map [:can-kill? [:= true]] [:blocking-ops [:= #{}]]]
               [:map [:can-kill? [:= false]] [:blocking-ops [:set {:min 1} :keyword]]]]
   :rel       (fn [id out] (= out {:can-kill? true :blocking-ops #{}}))
   :num-tests 30})

(deftest critical-ops-block-a-kill
  (is (some? (lings/enter-critical-op! "s1" :wrap)))
  (is (= {:can-kill? false :blocking-ops #{:wrap}} (lings/can-kill? "s1")))
  (is (= #{:wrap} (lings/get-critical-ops "s1")))
  (is (some? (lings/exit-critical-op! "s1" :wrap)))
  (is (= {:can-kill? true :blocking-ops #{}} (lings/can-kill? "s1")))
  (is (nil? (lings/enter-critical-op! "zz" :commit)) "an unknown slave answers nil"))

(hst/deftrifecta-from-schema append-stdout-counts-the-buffer
  hive-mcp.swarm.datascript.lings/append-stdout!
  {:in        [:cat [:enum "out-a"] [:or [:enum "one line"] [:vector {:max 4} [:enum "x" "y"]]]]
   :out       s/Count
   ;; the buffer starts empty in each property case: the slave id is fixed
   ;; and the cleanup below the rel resets it
   :rel       (fn [[id lines] out]
                (let [n (if (string? lines) 1 (count lines))
                      ok (= (count (lings/get-stdout id 10000)) out)]
                  (lings/cleanup-stdout-buffer! id)
                  (and ok (<= n out))))
   :num-tests 60})

(deftest stdout-ring-buffer-round-trip
  (is (m/validate s/Atom (lings/init-stdout-buffer! "b")))
  (is (= 3 (lings/append-stdout! "b" ["l0" "l1" "l2"])))
  (is (= ["l1" "l2"] (mapv :text (lings/get-stdout "b" 2))))
  (is (= ["l2"] (mapv :text (lings/get-stdout-since "b" 1))))
  (is (= {:line-count 3 :next-idx 3 :oldest-idx 0 :newest-idx 2 :max-lines 5000}
         (lings/get-stdout-buffer-info "b")))
  (is (true? (lings/cleanup-stdout-buffer! "b")))
  (is (false? (lings/cleanup-stdout-buffer! "b")))
  (is (= [] (lings/get-stdout "b")) "a missing buffer reads empty")
  (is (nil? (lings/get-stdout-buffer-info "b")))
  (is (= {} (lings/reset-stdout-buffers!))))

(deftest claim-file-refuses-a-live-foreign-holder
  (testing "SWARM-CLAIM-STEAL: another live slave's claim is refused, not upserted"
    (is (= {:claimed? false :refused :held-by-live-slave :file "f.clj" :held-by "s1"}
           (lings/claim-file! "f.clj" "s2"))))
  (testing "re-claiming one's own key upserts"
    (is (m/validate s/TxReport (lings/claim-file! "f.clj" "s1"))))
  (testing "a bare task-id string as opts is read as no opts"
    (is (m/validate s/TxReport (lings/claim-file! "g.clj" "s2" "t1")))))

(golden/deftest-golden lings-lifecycle-golden
  "test/golden/hive-mcp/swarm/store/lings-lifecycle.edn"
  (let [update-missing (lings/update-slave! "zz" {:slave/status :idle})
        stale-none (lings/claim-stale? "zz")
        stale-fresh (lings/claim-stale? "f.clj")
        stale-zero (lings/claim-stale? "f.clj" -1)
        stale-list (count (lings/get-stale-claims -1))
        refresh (some? (lings/refresh-claim! "f.clj"))
        wait (some? (lings/add-to-wait-queue! "s2" "f.clj"))
        complete (some? (lings/complete-task! "t1"))
        after-complete (strip (q/get-task "t1"))
        claims-left (count (q/get-all-claims))
        fail-missing (lings/fail-task! "zz" :error)
        _ (lings/add-task! "t2" "s2" {})
        failed (some? (lings/fail-task! "t2" :timeout))
        update-task (some? (lings/update-task! "t2" {:task/prompt "p"}))
        released-slave (lings/release-claims-for-slave! "s1")
        released-task (lings/release-claims-for-task! "t2")
        release-missing (lings/release-claim! "zz")
        cleanup (lings/cleanup-stale-claims! 100000000)
        removed (some? (lings/remove-slave! "s2"))
        remove-missing (lings/remove-slave! "zz")]
    {:update-missing update-missing :stale-none stale-none :stale-fresh stale-fresh
     :stale-zero stale-zero :stale-list stale-list :refresh refresh :wait wait
     :complete complete :after-complete after-complete :claims-left claims-left
     :fail-missing fail-missing :failed failed :update-task update-task
     :released-slave released-slave :released-task released-task
     :release-missing release-missing :cleanup cleanup :removed removed
     :remove-missing remove-missing
     :remaining (map :slave/id (q/get-all-slaves :include-stale? true))
     :ledger @ledger-events}))

;;; ---------------------------------------------------------------------------
;;; connection, registry, schema
;;; ---------------------------------------------------------------------------

(deftest connection-contract-test
  (is (m/validate s/Conn (conn/get-conn)))
  (is (identical? (conn/get-conn) (conn/ensure-conn)) "ensure-conn answers the bound test conn")
  (is (identical? (conn/get-conn) (conn/current-test-conn)))
  (is (not (identical? (conn/get-conn) (conn/create-conn))) "create-conn is fresh")
  (is (= 42 (conn/with-test-conn (conn/create-conn) (constantly 42))))
  (is (inst? (conn/now)))
  (is (= 36 (count (conn/gen-id))))
  (is (re-matches #"^task-.{36}$" (conn/gen-id "task"))))

(deftest registry-instances-satisfy-their-ports
  (doseq [[ctor getter port] [[registry/->DataScriptRegistry registry/get-default-registry
                               registry/SwarmRegistry]
                              [registry/->DataScriptClaimStore registry/get-default-claim-store
                               registry/ClaimStore]
                              [registry/->DataScriptCriticalOps registry/get-default-critical-ops
                               registry/CriticalOps]
                              [registry/->DataScriptCoordination registry/get-default-coordination
                               registry/Coordination]
                              [registry/->DataScriptDb registry/get-default-db registry/SwarmDb]]]
    (is (m/validate port (ctor)))
    (is (m/validate port (getter)))
    (is (identical? (getter) (getter)) "the default instance is a singleton")))

(hst/deftrifecta-from-schema claude-model-recognises-claude-names
  hive-mcp.swarm.datascript.schema/claude-model?
  {:in        [:maybe [:enum "claude" "claude-3-opus" "anthropic/claude-x" "gpt-4" "kimi" :claude]]
   :out       :boolean
   :rel       (fn [model out]
                (= out (or (nil? model)
                           (and (string? model)
                                (or (= "claude" model)
                                    (.startsWith ^String model "claude-")
                                    (.startsWith ^String model "anthropic/claude"))))))
   :classify  (fn [_ out] out)
   :classify-domain #{true false}
   :golden-path "test/golden/hive-mcp/swarm/store/claude-model.edn"
   :num-tests 60})
