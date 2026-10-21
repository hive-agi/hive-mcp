(ns hive-mcp.swarm.lifecycle.terminal-sweep-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.ling.spawn :as ling]
            [hive-mcp.agent.ling.terminal-registry :as terminal-reg]
            [hive-mcp.agent.protocol :as agent]
            [hive-mcp.swarm.datascript.lings :as ds-lings]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.swarm.lifecycle.terminal-sweep :as terminal-sweep]
            [hive-mcp.test.shim-facets :as sf]
            [hive-mcp.protocols.lifecycle :as lifecycle]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mutation :as hmut]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [malli.generator :as mg]))

(defn- status-agent
  [status-map]
  (reify agent/IAgent
    (spawn! [_ _opts] nil)
    (dispatch! [_ _task-opts] nil)
    (kill! [_] nil)
    (status [_] status-map)
    (agent-type [_] :ling)
    (can-chain-tools? [_] true)
    (claims [_] [])
    (claim-files! [_ _files _task-id] nil)
    (release-claims! [_] nil)))

(deftest sweep-once-zombifies-dead-terminal-lings
  (let [txs (atom [])
        slave {:slave/id "vterm-1"
               :slave/depth 1
               :slave/status :working
               :slave/alive? true
               :ling/spawn-mode :vterm}]
    (with-redefs [terminal-reg/registered-terminals (fn [] #{:vterm})
                  queries/get-all-slaves (fn [& _opts] [slave])
                  ling/->ling (fn [_id _opts]
                                (status-agent {:slave/id "vterm-1"
                                               :slave/status :dead
                                               :elisp-alive? false}))
                  ds-lings/update-slave! (fn [slave-id updates]
                                           (swap! txs conj [slave-id updates]))]
      (let [result (terminal-sweep/sweep-once! 1234)]
        (is (= {:checked 1 :zombified 1 :alive 0 :errors []} result))
        (is (= [["vterm-1" {:slave/alive? false
                            :slave/status :zombie
                            :slave/status-changed-at 1234}]]
               @txs))))))

(deftest sweep-once-ignores-unregistered-terminal-modes
  (testing "Rows for unloaded addons are not probed or zombified"
    (let [txs (atom [])
          slave {:slave/id "vterm-1"
                 :slave/depth 1
                 :slave/status :working
                 :ling/spawn-mode :vterm}]
      (with-redefs [terminal-reg/registered-terminals (fn [] #{})
                    queries/get-all-slaves (fn [& _opts] [slave])
                    ds-lings/update-slave! (fn [slave-id updates]
                                             (swap! txs conj [slave-id updates]))]
        (is (= {:checked 0 :zombified 0 :alive 0 :errors []}
               (terminal-sweep/sweep-once! 1234)))
        (is (empty? @txs))))))

;; =============================================================================
;; Facets without with-redefs
;; =============================================================================
;;
;; SEAM GAP (recorded, src unchanged): sweep-once! reads the store through
;; queries/get-all-slaves and writes through ds-lings/update-slave! with no
;; injected port (unlike boot-reconcile's 1-arity), so its zombify logic can
;; only be driven with with-redefs (the two tests above) or the swarm library
;; (test-swarm/). What IS observable offline: the record's ISweepable surface,
;; and that every sweep reaches the store read before anything else.

(def ^:private store-read
  "The shim var a sweep reaches first: offline, its addon-missing outcome."
  {:ns "hive-datascript.swarm.queries" :var "get-all-slaves"})

(defn- sweep-record-outcome
  "What the ISweepable record answers for CTX, as data."
  [ctx]
  (sf/outcome #(lifecycle/sweep! (terminal-sweep/->TerminalLivenessSweep) ctx) []))

(deftest-golden terminal-sweep-surface-golden
  "test/golden/hive-mcp/swarm/lifecycle/terminal-sweep-surface.edn"
  (let [r (terminal-sweep/->TerminalLivenessSweep)]
    {:interval-s (lifecycle/sweep-interval-s r)
     :name (lifecycle/sweep-name r)
     :default-interval-s terminal-sweep/default-sweep-interval-s
     :registered? terminal-sweep/registered?
     :offline (when (sf/offline?)
                {:sweep-once-0 (sf/call 'hive-mcp.swarm.lifecycle.terminal-sweep 'sweep-once! [])
                 :sweep-once-1 (sf/call 'hive-mcp.swarm.lifecycle.terminal-sweep 'sweep-once! [1234])
                 :record-sweep (sweep-record-outcome {:now-ms 1234})
                 :record-sweep-no-clock (sweep-record-outcome {})})}))

(defspec every-offline-sweep-reads-the-store-first 100
  (prop/for-all [args (mg/generator [:or [:cat] [:cat :int]])
                 ctx (mg/generator [:map [:now-ms {:optional true} [:maybe :int]]])]
    (or (not (sf/offline?))
        (and (= store-read (sf/call 'hive-mcp.swarm.lifecycle.terminal-sweep 'sweep-once! args))
             (= store-read (sweep-record-outcome ctx))))))

(when (sf/offline?)
  (hmut/deftest-mutations sweep-once-store-read-mutations
    hive-mcp.swarm.lifecycle.terminal-sweep/sweep-once!
    [["answers an empty sweep without reading the store"
      (fn [& _] {:checked 0 :zombified 0 :alive 0 :errors []})]
     ["reads the wrong store"
      (fn [& _] (throw (ex-info "x" {:swarm/addon-missing true :ns "hive-agent.swarm.queries" :var "get-all-slaves"})))]]
    (fn []
      (is (= store-read (sf/call 'hive-mcp.swarm.lifecycle.terminal-sweep 'sweep-once! [1])))
      (is (= store-read (sweep-record-outcome {:now-ms 1}))
          "the record's sweep! goes through sweep-once!"))))
