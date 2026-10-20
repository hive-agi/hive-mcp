(ns hive-mcp.system.sweepers.orphan-edge-test
  "OrphanEdgeSweep prunes auto-kg edges whose source memory entry is gone, and
   only those. Collaborators are injected ports, so no store is touched."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.system.sweepers.orphan-edge :as sweeper]
            [hive-spi.lifecycle.ports :as ports]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private gone  "20260101000000-aaaaaaaa")
(def ^:private alive "20260101000001-bbbbbbbb")
(def ^:private flaky "20260101000002-cccccccc")

(defn- edge [id from source-type]
  {:kg-edge/id id :kg-edge/from from :kg-edge/to "x"
   :kg-edge/relation :derived-from :kg-edge/source-type source-type})

(def ^:private all-edges
  [(edge "e1" gone :automated)
   (edge "e2" gone :co-access)
   (edge "e3" alive :automated)
   (edge "e4" gone :manual)
   (edge "e5" flaky :automated)
   (edge "e6" "not-a-memory-id" :automated)])

(defn- state [id]
  (cond (= id gone) :absent
        (= id flaky) :unknown
        :else :present))

(defn- scan-all [offset limit]
  (let [es (vec (take limit (drop offset all-edges)))]
    {:edges es :taken (count es)}))

(defn- ports-with [removed & {:as extra}]
  (merge {:scan-fn scan-all
          :entry-state-fn state
          :remove-fn (fn [id] (swap! removed conj id) true)}
         extra))

(deftest memory-id-shape
  (is (sweeper/memory-id? gone))
  (is (not (sweeper/memory-id? "not-a-memory-id")))
  (is (not (sweeper/memory-id? nil)))
  (is (not (sweeper/memory-id? "20260101000000-zzzzzzzz"))))

(deftest only-absent-auto-edges-are-orphans
  (testing "present, unknown, manual and non-memory sources are kept"
    (is (= ["e1" "e2"]
           (mapv :kg-edge/id (sweeper/find-orphans state all-edges))))))

(deftest dry-run-retracts-nothing
  (let [removed (atom [])
        r (ports/sweep! (sweeper/->sweep (ports-with removed)) {})]
    (is (empty? @removed))
    (is (= 2 (:would-prune r)))
    (is (zero? (:swept r)))
    (is (true? (:dry-run? r)))))

(deftest retract-removes-orphans-up-to-cap
  (let [removed (atom [])
        r (ports/sweep! (sweeper/->sweep (ports-with removed :retract? true :prune-cap 1)) {})]
    (is (= ["e1"] @removed))
    (is (= 1 (:swept r)))
    (is (false? (:dry-run? r)))))

(deftest a-failing-removal-is-reported-and-does-not-stop-the-sweep
  (let [removed (atom [])
        remove! (fn [id] (if (= id "e1") (throw (ex-info "boom" {})) (do (swap! removed conj id) true)))
        r (ports/sweep! (sweeper/->sweep (ports-with removed :retract? true :remove-fn remove!)) {})]
    (is (= ["e2"] @removed))
    (is (= 1 (:swept r)))
    (is (= 1 (count (:errors r))))))

(deftest cursor-rotates-through-windows
  (let [removed (atom [])
        sweep (sweeper/->sweep (ports-with removed :retract? true :scan-window 3))]
    (ports/sweep! sweep {})
    (is (= ["e1" "e2"] @removed))
    (is (= 3 @(:cursor sweep)))
    (ports/sweep! sweep {})
    (is (= 6 @(:cursor sweep)))
    (ports/sweep! sweep {})
    (is (= 0 @(:cursor sweep)) "an empty window wraps to the start")))

(deftest backfill-reports-count-before-retracting
  (let [removed (atom [])
        dry (sweeper/backfill (ports-with removed :scan-window 2))]
    (is (empty? @removed))
    (is (= 2 (:orphans dry)))
    (is (= 6 (:scanned dry)))
    (is (true? (:dry-run? dry)))
    (let [wet (sweeper/backfill (ports-with removed :scan-window 2 :retract? true))]
      (is (= 2 (:swept wet)))
      (is (= ["e1" "e2"] @removed)))))

;;; ---------------------------------------------------------------------------
;;; Contract facets: trifectas over the schemas, goldens over stubbed ports
;;; ---------------------------------------------------------------------------

(def ^:private digits (set "0123456789"))
(def ^:private hexes (set "0123456789abcdefABCDEF"))

(defn- oracle-memory-id? [id]
  (and (string? id) (= 23 (count id)) (= \- (nth id 14))
       (every? digits (subs id 0 14)) (every? hexes (subs id 15))))

(def ^:private IdCandidate
  "Memory ids, and memory ids broken in exactly one way, so the property
   reaches both arms of every check instead of only rejecting noise."
  [:any {:gen/schema [:tuple sweeper/MemoryId
                      [:enum :keep :upper :bad-hex :short :long :no-dash :nil :int]]
         :gen/fmap (fn [[id how]]
                     (case how
                       :keep id
                       :upper (str/upper-case id)
                       :bad-hex (str (subs id 0 22) "g")
                       :short (subs id 1)
                       :long (str id "0")
                       :no-dash (str (subs id 0 14) "0" (subs id 15))
                       :nil nil
                       :int 42))}])

(deftrifecta-from-schema memory-id-matches-the-id-shape
  #'sweeper/memory-id?
  {:in IdCandidate
   :out :boolean
   :rel (fn [id out] (= out (oracle-memory-id? id)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :classify-floor 5
   :golden-path "test/golden/hive-mcp/system/orphan-edge-memory-id.edn"
   :num-tests 200})

(def ^:private Edge
  [:map
   [:kg-edge/id [:enum "e1" "e2" "e3"]]
   [:kg-edge/from [:enum gone alive flaky "not-a-memory-id"]]
   [:kg-edge/source-type [:enum :automated :co-access :manual :inferred
                          "automated" "co-access" "manual" nil]]])

(deftrifecta-from-schema auto-kg-edge-is-automated-or-co-access
  #'sweeper/auto-kg-edge?
  {:in [:maybe Edge]
   :out :boolean
   :rel (fn [edge out]
          (= out (contains? #{"automated" "co-access"}
                            (some-> edge :kg-edge/source-type name))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :classify-floor 5
   :golden-path "test/golden/hive-mcp/system/orphan-edge-auto.edn"
   :num-tests 200})

(deftrifecta-from-schema find-orphans-keeps-only-absent-auto-memory-edges
  #'sweeper/find-orphans
  {:in [:cat [:map [gone sweeper/EntryState] [alive sweeper/EntryState] [flaky sweeper/EntryState]]
        [:vector {:max 6} Edge]]
   :out [:vector sweeper/OrphanEdge]
   :rel (fn [[states edges] out]
          (= out (filterv (fn [{:kg-edge/keys [from source-type]}]
                            (and (#{"automated" "co-access"} (some-> source-type name))
                                 (oracle-memory-id? from)
                                 (= :absent (get states from))))
                          edges)))
   :classify (fn [_ out] (if (seq out) :some :none))
   :classify-domain #{:some :none}
   :classify-floor 5
   :golden-path "test/golden/hive-mcp/system/orphan-edge-find.edn"
   :num-tests 200})

(deftrifecta-from-schema config-fills-every-default
  #'sweeper/config
  {:in [:maybe [:map
                [:retract? {:optional true} [:maybe :boolean]]
                [:scan-window {:optional true} [:maybe [:int {:min 1 :max 50}]]]
                [:prune-cap {:optional true} [:maybe [:int {:min 0 :max 50}]]]]]
   :out sweeper/SweepConfig
   :rel (fn [opts out]
          (and (= (boolean (:retract? opts)) (:retract? out))
               (= (or (:scan-window opts) sweeper/default-scan-window) (:scan-window out))
               (= (or (:prune-cap opts) sweeper/default-prune-cap) (:prune-cap out))
               (every? ifn? ((juxt :scan-fn :entry-state-fn :remove-fn) out))))
   :num-tests 100})

(golden/deftest-golden sweep-window-and-backfill-golden
  "test/golden/hive-mcp/system/orphan-edge-sweep.edn"
  (let [removed (atom [])
        cfg #(sweeper/config (ports-with removed %))
        throwing (fn [id] (if (= id "e2") (throw (ex-info "boom" {})) true))]
    {:dry-window   (sweeper/sweep-window! (cfg {}) 0)
     :wet-window   (sweeper/sweep-window! (cfg {:retract? true :scan-window 2}) 0)
     :tail-window  (sweeper/sweep-window! (cfg {:retract? true :scan-window 4}) 4)
     :error-window (sweeper/sweep-window! (assoc (cfg {:retract? true}) :remove-fn throwing) 0)
     :capped       (sweeper/sweep-window! (cfg {:retract? true :prune-cap 0}) 0)
     :backfill-dry (sweeper/backfill (ports-with removed :scan-window 4))
     :removed      @removed
     :new-cursor   @(:cursor (sweeper/->sweep nil))}))

(deftest orphan-edge-results-validate-and-the-schemas-bite
  (let [removed (atom [])
        w (sweeper/sweep-window! (sweeper/config (ports-with removed)) 0)
        b (sweeper/backfill (ports-with removed :retract? true))]
    (is (m/validate sweeper/WindowResult w))
    (is (m/validate sweeper/BackfillResult b))
    (is (m/validate sweeper/BackfillResult (sweeper/backfill (ports-with removed))) "0-arg opts arm")
    (is (not (m/validate sweeper/WindowResult (dissoc w :next-offset))))
    (is (not (m/validate sweeper/BackfillResult (assoc b :dry-run? nil))))
    (is (instance? hive_mcp.system.sweepers.orphan_edge.OrphanEdgeSweep (sweeper/->sweep {})))
    (is (every? sweeper/memory-id? (mg/sample sweeper/MemoryId {:size 20 :seed 1})))
    (is (not (m/validate sweeper/EntryState :gone)))))
