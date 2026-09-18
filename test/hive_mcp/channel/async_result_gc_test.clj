(ns hive-mcp.channel.async-result-gc-test
  "What `gc-expired!` is allowed to reclaim.

   The buffer's whole job is to hold a result until the caller collects it, so
   AGE alone must never decide a removal: only an entry the caller has already
   been given is cheap to drop. These tests pin that distinction, and one of
   them runs the pre-fix implementation to show what it did to an undelivered
   result."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.async-result :as ar]
            [hive-mcp.channel.async-result-journal :as journal]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- with-temp-journal
  "Point the durability journal at a throwaway file.

   `enqueue-result!` now writes to the journal and `gc-expired!` compacts it,
   so without this a test run would rewrite the REAL coordinator's journal at
   ~/.config/hive-mcp/data and could discard results a live process still owes."
  [t]
  (let [p (str (System/getProperty "java.io.tmpdir") "/hive-async-gc-test-" (random-uuid) ".edn")]
    (binding [journal/*journal-path* p]
      (ar/reset-all!)
      (try (t) (finally (ar/reset-all!) (.delete (java.io.File. p)))))))

(use-fixtures :each with-temp-journal)

(def ^:private caller "gc-test-caller")

(def ^:private ttl @#'ar/ttl-seconds)
(def ^:private grace @#'ar/undelivered-ttl-seconds)

(defn- age-every-entry!
  "Backdate every buffered entry by `secs`, so a TTL boundary can be crossed
   without sleeping through it."
  [secs]
  (swap! ar/buffers
         (fn [bufs]
           (reduce-kv (fn [acc k buf]
                        (assoc acc k (update buf :entries
                                             (fn [es]
                                               (mapv #(update % :timestamp - secs) es)))))
                      {}
                      bufs))))

(defn- buffered
  "Every entry currently held for `caller`, oldest first."
  []
  (->> @ar/buffers vals (mapcat :entries) vec))

(defn- task-ids [] (set (map :task-id (buffered))))

(defn- enqueue! [id]
  (ar/enqueue-result! caller {:task-id id :tool "t" :status :ok :result {:n id}}))

;; The pre-fix implementation, kept here as the thing being ruled out.
(defn- old-gc-expired!
  "`gc-expired!` as it stood before the delivered/undelivered split: every entry
   expires at `ttl-seconds`, whether or not the caller ever saw it."
  []
  (let [now-secs (.getEpochSecond (java.time.Instant/now))
        expired? (fn [entry] (> (- now-secs (:timestamp entry 0)) ttl))]
    (swap! ar/buffers
           (fn [bufs]
             (reduce-kv
              (fn [acc k {:keys [entries cursor] :as buf}]
                (let [live (vec (remove expired? entries))
                      before (count (filter (fn [idx] (expired? (nth entries idx)))
                                            (range (min cursor (count entries)))))]
                  (if (empty? live)
                    acc
                    (assoc acc k (assoc buf :entries live
                                        :cursor (max 0 (- cursor before)))))))
              {}
              bufs)))))

(deftest an-undelivered-result-is-not-reclaimed-just-because-it-is-old
  (testing "the defect: age alone used to be enough to destroy it"
    (enqueue! "never-drained")
    (age-every-entry! (+ ttl 60))
    (old-gc-expired!)
    (is (empty? (buffered))
        "pre-fix behaviour, recorded: the undelivered result was destroyed"))

  (testing "the fix: an uncollected result survives the delivered TTL"
    (ar/reset-all!)
    (enqueue! "never-drained")
    (age-every-entry! (+ ttl 60))
    (let [removed (ar/gc-expired!)]
      (is (zero? removed) "nothing is reclaimable yet")
      (is (= #{"never-drained"} (task-ids))
          "a result the caller was promised is still there to collect")
      (is (some? (ar/drain! caller))
          "and it can still actually be drained"))))

(deftest a-delivered-result-is-reclaimed-at-the-delivered-ttl
  ;; The cursor is set by hand rather than by draining: `drain!` takes
  ;; everything inside its budget and then drops the whole buffer, so a real
  ;; drain cannot leave the half-collected buffer this case is about.
  (enqueue! "collected")
  (enqueue! "pending")
  (swap! ar/buffers (fn [bufs] (reduce-kv (fn [acc k b] (assoc acc k (assoc b :cursor 1))) {} bufs)))
  (age-every-entry! (+ ttl 60))
  (let [removed (ar/gc-expired!)]
    (is (= 1 removed) "exactly the delivered one")
    (is (= #{"pending"} (task-ids))
        "the receipt goes, the undelivered result stays")))

(deftest an-undelivered-result-is-dropped-only-past-the-grace-period
  (enqueue! "abandoned")
  (age-every-entry! (+ grace 60))
  (let [removed (ar/gc-expired!)]
    (is (= 1 removed) "a caller that never drains cannot pin the buffer forever")
    (is (empty? (buffered)))))

(deftest the-cursor-still-points-past-the-last-delivered-survivor
  (doseq [id ["a" "b" "c" "d"]] (enqueue! id))
  ;; Mark a, b, c delivered; only d is pending.
  (swap! ar/buffers (fn [bufs] (reduce-kv (fn [acc k b] (assoc acc k (assoc b :cursor 3))) {} bufs)))
  (age-every-entry! (+ ttl 60))
  (ar/gc-expired!)
  (let [buf (first (vals @ar/buffers))]
    (is (= ["d"] (mapv :task-id (:entries buf)))
        "the three delivered entries are gone")
    (is (= 0 (:cursor buf))
        "and the cursor moved back with them, so d is still undelivered")
    (is (= #{"d"} (set (map :task-id (:results (ar/drain! caller)))))
        "d drains exactly once, neither skipped nor repeated")))

(deftest the-count-matches-what-actually-left-the-buffers
  (doseq [id ["a" "b" "c"]] (enqueue! id))
  (swap! ar/buffers (fn [bufs] (reduce-kv (fn [acc k b] (assoc acc k (assoc b :cursor 3))) {} bufs)))
  (age-every-entry! (+ ttl 60))
  (let [before (count (buffered))
        removed (ar/gc-expired!)
        after (count (buffered))]
    (is (= removed (- before after))
        "the returned count is derived from the buffers, not tallied alongside them")))

(deftest gc-on-an-empty-store-is-zero-not-a-crash
  (is (= 0 (ar/gc-expired!))))

(deftest a-buffer-with-nothing-expired-is-left-exactly-as-it-was
  (doseq [id ["a" "b"]] (enqueue! id))
  (let [before @ar/buffers]
    (is (= 0 (ar/gc-expired!)))
    (is (= before @ar/buffers) "no churn on a young buffer")))
