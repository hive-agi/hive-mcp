(ns hive-mcp.swarm.restore-liveness-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.swarm.sync :as sync]
            [hive-spi.swarm.bootstrap :as bootstrap]
            [hive-mcp.swarm.lifecycle.boot-reconcile :as boot-reconcile]))

(deftrifecta restore-classification
  sync/classify-row
  {:cases {:dead [{:slave-id "dead" :status :working :created-at 100} {:emacs {:state :known :ids #{}} :live-pids #{}}]
           :alive [{:slave-id "alive" :status :idle :spawn-mode :vterm :created-at 200} {:emacs {:state :known :ids #{"alive"}} :terminal-modes #{:vterm} :live-pids #{}}]
           :pid [{:slave-id "pid" :status :working :process-pid 42 :created-at 300} {:emacs {:state :known :ids #{}} :live-pids #{42}}]}
   :gen (gen/tuple (gen/hash-map :slave-id gen/string-alphanumeric
                                  :status (gen/elements [:working :idle])
                                  :created-at gen/pos-int)
                   (gen/return {:emacs {:state :known :ids #{}} :live-pids #{}}))
   :apply? true
   :pred (fn [row] (and (= :zombie (:status row)) (false? (:alive? row))))
   :num-tests 30})

(defn- isolated-restore-store
  "Per-test registry ports: no globally installed DataScript delegate or addon."
  []
  (let [state (atom {})]
    {:state state
     :sync-ports {:reset! #(reset! state {})
                  :register! (fn [{:keys [slave-id status created-at liveness alive? spawn-mode]}]
                               (swap! state assoc slave-id
                                      (cond-> {:slave/id slave-id :slave/status status
                                               :slave/alive? (not (false? alive?))}
                                        created-at (assoc :slave/created-at created-at)
                                        liveness (assoc :slave/liveness liveness)
                                        spawn-mode (assoc :ling/spawn-mode spawn-mode))))}
     :reconcile-ports {:load-slaves #(vals @state)
                       :update! (fn [id updates] (swap! state update id merge updates))
                       :terminal-modes #(identity #{:vterm})}}))

(deftest restore-never-publishes-dead-as-working
  (let [original (java.util.Date. 1234567890)
        rows [{:slave-id "dead" :status :working :created-at original}
              {:slave-id "alive" :status :idle :spawn-mode :vterm :created-at original}]
        source (reify bootstrap/ISwarmBootstrap
                 (-load-slaves [_] rows)
                 (-snapshot-slave! [this _ _] this)
                 (-forget-slave! [this _] this)
                 (-close! [_] nil))
        {:keys [state sync-ports]} (isolated-restore-store)]
    (sync/full-sync-from-bootstrap! source
                                    (fn [_] {:emacs {:state :known :ids #{"alive"}}
                                             :terminal-modes #{:vterm} :live-pids #{}})
                                    sync-ports)
    (let [dead (get @state "dead")
          alive (get @state "alive")]
      (is (not= :working (:slave/status dead)))
      (is (false? (:slave/alive? dead)))
      (is (= :idle (:slave/status alive)))
      (is (= original (:slave/created-at alive))))))

(deftest unknown-emacs-membership-preserves-terminal-row
  (let [row {:slave-id "vterm" :status :working :spawn-mode :vterm :created-at 123}
        classified (sync/classify-row row {:emacs {:state :unknown}
                                              :terminal-modes #{:vterm}
                                              :live-pids #{}})]
    (is (= :working (:status classified)))
    (is (= :unverified (:liveness classified)))
    (is (= 123 (:created-at classified)))))

(deftest headless-session-evidence-preserves-same-jvm-row
  (let [row {:slave-id "in-jvm" :status :working :spawn-mode :hive-agent}]
    (is (= :working (:status (sync/classify-row row {:emacs {:state :known :ids #{}} :live-ids #{"in-jvm"} :live-pids #{}}))))
    (is (= :zombie (:status (sync/classify-row row {:emacs {:state :known :ids #{}} :live-ids #{} :live-pids #{}}))))))

(deftest full-sync-keeps-unknown-vessel-and-live-headless
  (let [rows [{:slave-id "vessel" :status :working :spawn-mode :vterm}
              {:slave-id "session" :status :working :spawn-mode :hive-agent}
              {:slave-id "old" :status :working :spawn-mode :hive-agent}]
        source (reify bootstrap/ISwarmBootstrap
                 (-load-slaves [_] rows)
                 (-snapshot-slave! [this _ _] this)
                 (-forget-slave! [this _] this)
                 (-close! [_] nil))
        {:keys [state sync-ports]} (isolated-restore-store)]
    (sync/full-sync-from-bootstrap!
     source (fn [_] {:emacs {:state :unknown} :terminal-modes #{:vterm}
                     :live-ids #{"session"} :live-pids #{}}) sync-ports)
    (is (= :working (:slave/status (get @state "vessel"))))
    (is (= :unverified (:slave/liveness (get @state "vessel"))))
    (is (= :working (:slave/status (get @state "session"))))
    (is (= :zombie (:slave/status (get @state "old"))))))

(deftest boot-reconciliation-retires-restored-zombies
  (let [rows [{:slave-id "previous-boot" :status :working :spawn-mode :hive-agent}
              {:slave-id "live-session" :status :working :spawn-mode :hive-agent}
              {:slave-id "unverified-vessel" :status :working :spawn-mode :vterm}]
        source (reify bootstrap/ISwarmBootstrap
                 (-load-slaves [_] rows)
                 (-snapshot-slave! [this _ _] this)
                 (-forget-slave! [this _] this)
                 (-close! [_] nil))
        {:keys [state sync-ports reconcile-ports]} (isolated-restore-store)]
    (sync/full-sync-from-bootstrap!
     source (fn [_] {:emacs {:state :unknown} :terminal-modes #{:vterm}
                     :live-ids #{"live-session"} :live-pids #{}}) sync-ports)
    (let [result (boot-reconcile/reconcile-rehydrated-slaves! reconcile-ports)]
      (is (= 0 (:reconciled result)))
      (is (= 2 (:spared result)))
      (is (= 1 (:skipped result)))
      (is (false? (:slave/alive? (get @state "previous-boot"))))
      (is (= :working (:slave/status (get @state "live-session"))))
      (is (= :working (:slave/status (get @state "unverified-vessel")))))))

(deftest stale-emacs-membership-is-not-headless-loop-evidence
  (let [row {:slave-id "ghost" :status :working :spawn-mode :hive-agent}
        evidence {:emacs {:state :known :ids #{"ghost"}}
                  :terminal-modes #{:vterm} :live-ids #{} :live-pids #{}}]
    (is (= :zombie (:status (sync/classify-row row evidence))))
    (is (false? (:alive? (sync/classify-row row evidence))))))
