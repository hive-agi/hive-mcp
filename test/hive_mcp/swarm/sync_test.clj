(ns hive-mcp.swarm.sync-test
  "Facets for hive-mcp.swarm.sync on the unit classpath.

     trifecta  classify-row from its contract, over rows and evidence drawn
               from small id / pid / mode alphabets so every arm (live id,
               live pid, Emacs-tracked membership, unknown Emacs, zombie) is
               reached; the oracle restates the docstring's rule
     golden    full-sync-from-bootstrap! with every port injected (a reified
               bootstrap, a probe fn, recording reset!/register! ports), and
               missing-from-emacs? over named cases
     property  set/get of the injected hooks registry round-trips

   The event handlers and start-sync! need the swarm store (hive-datascript)
   and run in test-swarm/hive_mcp/swarm/sync_test.clj."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.hooks.core :as hooks]
            [hive-mcp.swarm.sync :as sync]
            [hive-schemas.test :as hst]
            [hive-spi.swarm.bootstrap :as bootstrap]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f]
    (let [before (sync/get-hooks-registry)]
      (try (f) (finally (sync/set-hooks-registry! before))))))

(def ^:private Row
  [:map
   [:slave-id [:enum "s1" "s2" "s3"]]
   [:status {:optional true} [:enum :working :idle]]
   [:spawn-mode {:optional true} [:maybe [:enum :vterm :headless]]]
   [:process-pid {:optional true} [:maybe [:enum 41 42]]]])

(def ^:private Evidence
  [:map
   [:emacs [:or [:map [:state [:= :unknown]]]
            [:map [:state [:= :known]] [:ids [:set {:gen/max 3} [:enum "s1" "s2" "s3"]]]]]]
   [:live-pids [:set {:gen/max 2} [:enum 41 42]]]
   [:live-ids [:set {:gen/max 2} [:enum "s1" "s2" "s3"]]]
   [:terminal-modes [:set {:gen/max 1} [:enum :vterm]]]])

(defn- expected
  "The docstring's rule, restated: live backing verifies; Emacs membership
   verifies only terminal or mode-less rows; an unknown Emacs leaves those
   unverified; everything else is a zombie."
  [row {:keys [emacs live-pids live-ids terminal-modes]}]
  (let [tracked? (or (nil? (:spawn-mode row)) (contains? terminal-modes (:spawn-mode row)))]
    (cond
      (contains? live-ids (:slave-id row))                       (assoc row :liveness :verified)
      (and (:process-pid row) (live-pids (:process-pid row)))   (assoc row :liveness :verified)
      (and tracked? (contains? (:ids emacs) (:slave-id row)))    (assoc row :liveness :verified)
      (and tracked? (= :unknown (:state emacs)))                 (assoc row :liveness :unverified)
      :else                                                      (assoc row :status :zombie :alive? false))))

(defn- verdict [out] (or (:liveness out) :zombie))

(hst/deftrifecta-from-schema classify-row-follows-the-liveness-rule
  hive-mcp.swarm.sync/classify-row
  {:in  [:cat Row Evidence]
   :out [:or [:map [:liveness [:enum :verified :unverified]]]
         [:map [:status [:= :zombie]] [:alive? [:= false]]]]
   :rel (fn [[row ev] out] (= (expected row ev) out))
   :classify (fn [_ out] (verdict out))
   :classify-domain #{:verified :unverified :zombie}
   :num-tests 300
   :golden-path "test/golden/hive-mcp/swarm/sync-classify-row.edn"})

(defn- source-of
  "A reified ISwarmBootstrap port answering ROWS (or throwing when ROWS is :throws)."
  [rows]
  (reify bootstrap/ISwarmBootstrap
    (-load-slaves [_] (if (= :throws rows) (throw (ex-info "store down" {})) rows))
    (-snapshot-slave! [this _ _] this)
    (-forget-slave! [this _] this)
    (-close! [_] nil)))

(defn- run-full-sync
  "full-sync-from-bootstrap! over injected ports; what each port saw."
  [rows evidence]
  (let [calls (atom [])
        ret   (sync/full-sync-from-bootstrap!
               (source-of rows)
               (fn [rs] (swap! calls conj [:probe (mapv :slave-id rs)]) evidence)
               {:reset!    #(swap! calls conj [:reset])
                :register! #(swap! calls conj [:register (select-keys % [:slave-id :status :liveness :alive?])])})]
    {:returned ret :calls @calls}))

(deftest-golden full-sync-golden
  "test/golden/hive-mcp/swarm/sync-full-sync.edn"
  {:mixed (run-full-sync [{:slave-id "live" :status :working :process-pid 7}
                          {:slave-id "emacs" :status :idle :spawn-mode :vterm}
                          {:slave-id "dead" :status :working :spawn-mode :headless}]
                         {:emacs {:state :known :ids #{"emacs" "dead"}} :live-pids #{7}
                          :live-ids #{} :terminal-modes #{:vterm}})
   :unknown-emacs (run-full-sync [{:slave-id "vt" :status :working :spawn-mode :vterm}]
                                 {:emacs {:state :unknown} :live-pids #{} :live-ids #{}
                                  :terminal-modes #{:vterm}})
   :empty (run-full-sync [] {:emacs {:state :known :ids #{}} :live-pids #{} :live-ids #{}
                             :terminal-modes #{}})
   :load-throws (run-full-sync :throws {:emacs {:state :known :ids #{}} :live-pids #{}
                                        :live-ids #{} :terminal-modes #{}})
   :missing-from-emacs [(sync/missing-from-emacs? "a" #{"a"})
                        (sync/missing-from-emacs? "b" #{"a"})
                        (sync/missing-from-emacs? "a" nil)]})

(deftest hooks-registry-round-trips-test
  (let [r (hooks/create-registry)]
    (is (nil? (sync/set-hooks-registry! r)))
    (is (identical? r (sync/get-hooks-registry)))
    (sync/set-hooks-registry! nil)
    (is (nil? (sync/get-hooks-registry)))))
