(ns hive-mcp.channel.drain-trifecta-test
  "Trifectas for what the piggyback drain sends and keeps: the async-result
   buffer and its journal, the wire projection, the floor-lane compaction and
   the commitment density, driven by their contracts. The journal is bound to
   a temp file for every test."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.async-result :as ar]
            [hive-mcp.channel.async-result-journal :as journal]
            [hive-mcp.channel.drain-metrics :as dm]
            [hive-mcp.channel.drain-projection :as dp]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- with-temp-journal [f]
  (let [tmp (java.io.File/createTempFile "m04-journal" ".edn")]
    (.delete tmp)
    (binding [journal/*journal-path* (str tmp)]
      (ar/reset-all!)
      (try (f) (finally (ar/reset-all!) (io/delete-file tmp true))))))

(use-fixtures :each with-temp-journal)

(defn- out-schema [nsym fsym]
  (-> (m/function-schemas) (get-in [nsym fsym :schema]) m/children second))

;;; ---------------------------------------------------------------------------
;;; journal replay (pure)
;;; ---------------------------------------------------------------------------

(def ^:private Record
  [:or
   [:map [:op [:= :submitted]] [:caller [:enum "c1" "c2"]] [:task-id [:enum "t1" "t2" "t3"]]
    [:tool [:enum "memory"]] [:at [:enum 1000 5000]]]
   [:map [:op [:= :result]] [:caller [:enum "c1" "c2"]] [:task-id [:enum "t1" "t2" "t3"]]
    [:entry [:map [:task-id [:enum "t1" "t2" "t3"]] [:status [:enum :ok :error]]]]]
   [:map [:op [:= :delivered]] [:caller [:enum "c1" "c2"]]
    [:task-ids [:vector {:max 2} [:enum "t1" "t2" "t3"]]]]])

(hst/deftrifecta-from-schema replay-contract
  hive-mcp.channel.async-result-journal/replay
  ;; a :result record's entry answers the record's own task, as
  ;; record-result! writes it
  {:in        [:vector {:max 6
                        :gen/fmap (fn [rs]
                                    (mapv #(if (= :result (:op %))
                                             (assoc-in % [:entry :task-id] (:task-id %))
                                             %)
                                          rs))}
               Record]
   :out       (out-schema 'hive-mcp.channel.async-result-journal 'replay)
   ;; nothing delivered comes back, and a task never resulted is :interrupted
   :rel       (fn [records out]
                (let [delivered (set (for [{:keys [op caller task-ids]} records
                                           :when (= :delivered op) t task-ids] [caller t]))]
                  (every? (fn [[caller {:keys [entries]}]]
                            (every? (fn [e]
                                      (and (not (delivered [caller (:task-id e)]))
                                           (or (not= :interrupted (:status e))
                                               (= journal/interrupted-error (:error e)))))
                                    entries))
                          out)))
   :num-tests 300
   :golden-path "test/golden/channel/journal-replay.edn"})

(deftest journal-io-contract-test
  (is (str/ends-with? (journal/default-path) "async-result/journal.edn"))
  (is (= journal/*journal-path* (journal/journal-path)))
  (is (m/validate (out-schema 'hive-mcp.channel.async-result-journal 'status) (journal/status)))
  (is (true? (journal/record-submitted! "c1" "t1" "memory")))
  (is (true? (journal/record-result! "c1" "t2" {:task-id "t2" :status :ok})))
  (is (true? (journal/record-delivered! "c1" ["t2"])))
  (is (true? (journal/record-delivered! "c1" [])) "nothing delivered writes nothing")
  (is (= 3 (count (journal/read-records))))
  (let [restored (journal/restore)]
    (is (m/validate journal/Buffers restored))
    (is (= [:interrupted] (mapv :status (get-in restored ["c1" :entries])))))
  (is (= 1 (journal/compact! (journal/restore))))
  (is (= 1 (count (journal/read-records)))))

;;; ---------------------------------------------------------------------------
;;; async-result buffer
;;; ---------------------------------------------------------------------------

(deftest async-result-contract-test
  (let [caller "coordinator:zz"]
    (is (nil? (ar/enqueue-result! caller {:task-id "t1" :tool "memory" :status :ok :result "r1"})))
    (is (nil? (ar/enqueue-result! caller {:task-id "t1" :tool "memory" :status :ok :result "r1"}))
        "a duplicate is skipped, not refused")
    (is (nil? (ar/enqueue-result! caller {:task-id "t2" :tool "kg" :status :error :result "boom"})))
    (is (true? (ar/has-pending? caller)))
    (is (= "r1" (:result (ar/poll-task "t1"))))
    (is (nil? (ar/poll-task "zz-none")))
    (is (m/validate (out-schema 'hive-mcp.channel.async-result 'stats) (ar/stats)))
    (let [batch (ar/drain! caller)]
      (is (m/validate ar/DrainBatch batch))
      (is (= ["t1" "t2"] (mapv :task-id (:results batch))))
      (is (true? (:done batch))))
    (is (nil? (ar/drain! caller)) "nothing left")
    (is (false? (ar/has-pending? caller)))
    (is (nil? (ar/drain! caller 0)) "a zero budget drains nothing")
    (is (true? (ar/record-submission! caller "t3" "memory")))
    (is (nat-int? (ar/gc-expired!)))
    (is (map? (ar/clear-buffer! caller)))
    (is (nat-int? (ar/restore!)))
    (is (= {} (ar/reset-all!)))))

(deftest budgeted-drain-marks-oversized-results-test
  (let [caller "coordinator:zz-big"]
    (ar/enqueue-result! caller {:task-id "big" :status :ok :result (apply str (repeat 500 "x"))})
    (let [batch (ar/drain! caller 200)]
      (is (m/validate ar/DrainBatch batch))
      (is (str/includes? (pr-str (:results batch)) "truncated")))))

;;; ---------------------------------------------------------------------------
;;; axiom projection (pure)
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; drain projection
;;; ---------------------------------------------------------------------------

(def ^:private PoolEntry
  [:map [:id [:enum "e1" "e2"]] [:T [:enum "note" "decision" "axiom"]]
   [:C [:enum "# Title line\nbody body body body body body body body body body" "x"]]
   [:tags {:optional true} [:vector {:max 7} [:enum "a" "b" "c"]]]
   [:ref {:optional true} [:enum true]]])

(hst/deftrifecta-from-schema title-of-contract
  hive-mcp.channel.drain-projection/title-of
  {:in        [:enum "# Heading\nrest" "\n\n  first real line  \nsecond" "" nil
               "x"]
   :out       :string
   :rel       (fn [_ out] (and (<= (count out) (+ dp/title-chars 3)) (not (str/starts-with? out "#"))))
   :golden-path "test/golden/channel/projection-title-of.edn"})

(hst/deftrifecta-from-schema index-entry-contract
  hive-mcp.channel.drain-projection/index-entry
  {:in        PoolEntry
   :out       dp/IndexRow
   :rel       (fn [in out]
                (and (= (:id in) (:id out))
                     (<= (count (:tags out [])) dp/index-tags)
                     (= (boolean (seq (:tags in))) (contains? out :tags))))
   :num-tests 200
   :golden-path "test/golden/channel/projection-index-entry.edn"})

(hst/deftrifecta-from-schema project-entry-contract
  hive-mcp.channel.drain-projection/project-entry
  {:in        [:cat PoolEntry [:map [:policy [:enum :index :full]] [:axiom-policy [:enum :full :compact]]]]
   :out       :map
   ;; a projection never grows the entry, and :full sends a pool entry whole
   :rel       (fn [[e {:keys [policy]}] out]
                (and (<= (count (pr-str out)) (count (pr-str e)))
                     (or (not= :full policy) (= "axiom" (:T e)) (= e out))))
   :num-tests 200})

(hst/deftrifecta-from-schema project-contract
  hive-mcp.channel.drain-projection/project
  {:in        [:cat [:vector {:max 4} PoolEntry] [:map [:policy [:enum :index :full]]
                                                  [:axiom-policy [:enum :full]]]]
   :out       [:vector :map]
   ;; idempotent: projecting a projected buffer changes nothing
   :rel       (fn [[es opts] out] (and (= (count es) (count out)) (= out (dp/project out opts))))
   :num-tests 200})

(hst/deftrifecta-from-schema pull-hint-contract
  hive-mcp.channel.drain-projection/pull-hint
  {:in        [:vector {:max 3} [:map [:id [:enum "e"]] [:ref {:optional true} [:enum true]]]]
   :out       [:maybe :string]
   :rel       (fn [batch out] (= (some? out) (boolean (some :ref batch))))})

(deftest resolve-policies-test
  (is (= :full (dp/resolve-policy :full)))
  (is (= :index (dp/resolve-policy ":index")))
  (is (contains? dp/policies (dp/resolve-policy "nonsense")))
  (is (contains? dp/policies (dp/resolve-policy)))
  (is (= :full (dp/resolve-axiom-policy " :full ")))
  (is (contains? dp/axiom-policies (dp/resolve-axiom-policy :nonsense)))
  (is (contains? dp/axiom-policies (dp/resolve-axiom-policy))))

;;; ---------------------------------------------------------------------------
;;; commitment density
;;; ---------------------------------------------------------------------------

(hst/deftrifecta-from-schema commitment-density-contract
  hive-mcp.channel.drain-metrics/commitment-density
  {:in        [:cat [:vector {:max 5} dm/Atom] [:int {:min -10 :max 5000}]]
   :out       [:double {:min 0.0}]
   :rel       (fn [[atoms chars] out]
                (if (pos? chars)
                  (= out (* 1000.0 (/ (double (count atoms)) chars)))
                  (= 0.0 out)))
   :num-tests 200})

(deftest drain-schemas-generate-test
  (doseq [s [journal/Buffer journal/Buffers journal/JournalRecord ar/CallerId ar/AsyncResult
             ar/DrainBatch dp/BufferedEntry dp/IndexRow dp/ProjectionOpts]]
    (is (seq (mg/sample s {:size 3})) (pr-str (m/form s)))))
