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
            [hive-mcp.channel.axiom-projection :as ax]
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

(def ^:private Content
  [:enum
   "# Rule\nDo the thing.\n"
   "# Rule\nDo it.\n## Why\nBecause once it broke.\nand twice\n"
   "# Rule\n## Examples\n- a\n- b\n## Strict definitions\nX means Y\n"
   "# Rule\n```guard-rule\n{:rule/id :guard/zz-probe}\n```\nafter\n"
   "intro\n## Background\n<!-- rationale -->\nold story\n"
   "no heading at all"
   ""])

(hst/deftrifecta-from-schema normalise-header-contract
  hive-mcp.channel.axiom-projection/normalise-header
  {:in        [:enum "## Why it matters" "# Examples!" "### Strict Definitions" "plain" nil]
   :out       :string
   ;; lowercase words and single spaces only, trimmed
   :rel       (fn [_ out]
                (and (every? #(or (Character/isLowerCase ^char %) (= \space %)) out)
                     (= out (str/trim out))
                     (not (str/includes? out "  "))))
   :golden-path "test/golden/channel/axiom-normalise-header.edn"})

(hst/deftrifecta-from-schema explanatory-header-contract
  hive-mcp.channel.axiom-projection/explanatory-header?
  {:in        [:enum "## Why it matters" "## Examples" "## Strict definitions" "## Antipatterns"
               "## Background" "## The rule" nil]
   :out       :boolean
   :rel       (fn [h out]
                (= out (contains? #{"## Why it matters" "## Examples" "## Background"} h)))
   :n-cases   10})

(hst/deftrifecta-from-schema sections-contract
  hive-mcp.channel.axiom-projection/sections
  {:in        Content
   :out       [:vector ax/Section]
   ;; every line of the source lands in exactly one section, in order
   :rel       (fn [in out]
                (= (str/split-lines in)
                   (vec (mapcat (fn [{:keys [header body]}] (if header (cons header body) body)) out))))
   :n-cases   10
   :golden-path "test/golden/channel/axiom-sections.edn"})

(hst/deftrifecta-from-schema withhold-contract
  hive-mcp.channel.axiom-projection/withhold?
  {:in        [:map [:header [:enum "## Why" "## Rules" nil]]
               [:body [:vector {:max 2} [:enum "<!-- rationale -->" "text"]]]]
   :out       :boolean
   :rel       (fn [{:keys [header body]} out]
                (= out (boolean (and header (or (= "## Why" header)
                                                (some #{"<!-- rationale -->"} (take 2 body)))))))
   :num-tests 200})

(hst/deftrifecta-from-schema llmlingua-round-trip-contract
  hive-mcp.channel.axiom-projection/llmlingua
  {:in        Content
   :out       :string
   ;; strip-llmlingua is an exact inverse
   :rel       (fn [in out] (= in (ax/strip-llmlingua out)))
   :n-cases   10
   :golden-path "test/golden/channel/axiom-llmlingua.edn"})

(hst/deftrifecta-from-schema strip-llmlingua-contract
  hive-mcp.channel.axiom-projection/strip-llmlingua
  {:in        Content
   :out       :string
   ;; a text with no control tags is its own strip
   :rel       (fn [in out] (= in out))
   :n-cases   10})

(hst/deftrifecta-from-schema strip-guard-blocks-contract
  hive-mcp.channel.axiom-projection/strip-guard-blocks
  {:in        [:cat Content [:enum "mem-1"]]
   :out       :string
   :rel       (fn [[in id] out]
                (and (not (str/includes? out "```guard-rule"))
                     (= (str/includes? in "```guard-rule") (str/includes? out (str "memory get " id)))))
   :n-cases   10})

(hst/deftrifecta-from-schema compact-contract
  hive-mcp.channel.axiom-projection/compact
  {:in        [:cat Content [:enum "mem-1"]]
   :out       :string
   ;; bounded by the source plus its pointers, and the leading statement
   ;; survives intact
   :rel       (fn [[in _] out]
                (and (<= (count out) (+ (count in) 200))
                     (str/starts-with? out (or (first (str/split-lines (str in))) ""))))
   :n-cases   10
   :golden-path "test/golden/channel/axiom-compact.edn"})

(hst/deftrifecta-from-schema compact-entry-contract
  hive-mcp.channel.axiom-projection/compact-entry
  {:in        [:map [:id [:enum "mem-1"]] [:C Content] [:compacted {:optional true} [:enum true]]]
   :out       :map
   ;; idempotent by flag, and compaction never grows the entry
   :rel       (fn [in out]
                (and (<= (count (str (:C out))) (count (str (:C in))))
                     (or (not (:compacted in)) (= in out))
                     (= out (ax/compact-entry out))))
   :n-cases   12})

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
             ar/DrainBatch ax/Section dp/BufferedEntry dp/IndexRow dp/ProjectionOpts]]
    (is (seq (mg/sample s {:size 3})) (pr-str (m/form s)))))
