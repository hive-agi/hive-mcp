(ns hive-mcp.channel.async-result-concurrency-test
  "The sweep is a THIRD writer on `buffers`, and these pin that it is safe.

   Until 2026-09-16 `gc-expired!` had no caller, so `enqueue-result!` and
   `drain!` were the only two things that ever touched the atom. Registering
   the sweep with the coordinator put a timer on it: every 300 s something
   now rewrites every buffer and deletes keys underneath callers that are
   mid-drain. Nothing tested that interleaving, because until then it could
   not happen.

   All three writers are `swap!`/`swap-vals!` over pure update functions, so
   each is atomic and a contended retry re-applies to the winner's value.
   That is a property of HOW they are written, not of what they compute, and
   it is exactly what an innocent-looking refactor back to
   read-modify-write would destroy. `drain!` already carries a docstring
   about having been that bug once. These tests fail if it becomes one
   again."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.async-result :as ar]
            [hive-mcp.channel.async-result-journal :as journal]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private gc-buffer @#'ar/gc-buffer)

(defn- with-temp-journal
  "Point the durability journal at a throwaway file.

   The race test drives thousands of enqueues and drains, each of which now
   appends a journal record, and the sweep compacts. Bound here so a test run
   cannot rewrite the REAL coordinator's journal. `future` conveys dynamic
   bindings, so the producer, consumer and sweep threads all see this path."
  [t]
  (let [p (str (System/getProperty "java.io.tmpdir") "/hive-async-race-test-" (random-uuid) ".edn")]
    (binding [journal/*journal-path* p]
      (ar/reset-all!)
      (try (t) (finally (ar/reset-all!) (.delete (java.io.File. p)))))))

(use-fixtures :each with-temp-journal)

(defn- now-s [] (long (/ (System/currentTimeMillis) 1000)))

(defn- entry
  "A buffer entry aged `age-s` seconds."
  [id age-s]
  {:task-id id :tool "probe" :status :ok
   :timestamp (- (now-s) age-s) :content-hash (str "h-" id)})

;; ---------------------------------------------------------------- cursor

(deftest reclaiming-a-delivered-entry-moves-the-cursor-with-it
  ;; The invariant the cursor encodes: entries before it have been handed to
  ;; the caller, entries from it on have not. Removing a delivered entry
  ;; without moving the cursor would re-deliver a survivor or skip one.
  (let [buf {:entries [(entry "d0" 400) (entry "d1" 10) (entry "u2" 10)]
             :cursor 2}
        [buf' orphaned] (gc-buffer buf (now-s))]
    (is (= ["d1" "u2"] (mapv :task-id (:entries buf'))))
    (is (= 1 (:cursor buf'))
        "d1 is still delivered and must still sit before the cursor")
    (is (= [] orphaned) "a delivered entry reaching its TTL is not a loss")))

(deftest reclaiming-every-delivered-entry-leaves-the-cursor-at-zero
  (let [buf {:entries [(entry "d0" 400) (entry "d1" 400) (entry "u2" 10)]
             :cursor 2}
        [buf' _] (gc-buffer buf (now-s))]
    (is (= ["u2"] (mapv :task-id (:entries buf'))))
    (is (zero? (:cursor buf')) "nothing delivered survives, so nothing precedes")))

(deftest an-undelivered-entry-past-its-grace-period-is-reported-not-counted
  ;; The one path where a caller loses a result it was promised.
  (let [buf {:entries [(entry "d0" 10) (entry "u1" 90000) (entry "u2" 10)]
             :cursor 1}
        [buf' orphaned] (gc-buffer buf (now-s))]
    (is (= ["d0" "u2"] (mapv :task-id (:entries buf'))))
    (is (= 1 (:cursor buf'))
        "u2 must still be undelivered after the entry ahead of it went")
    (is (= ["u1"] (mapv :task-id orphaned))
        "and the loss is named, because nothing else will ever mention it")))

(deftest a-buffer-that-loses-everything-is-dropped-rather-than-left-empty
  (let [[buf' _] (gc-buffer {:entries [(entry "d0" 400)] :cursor 1} (now-s))]
    (is (nil? buf') "nil is the signal to drop the key, not an empty buffer")))

;; ---------------------------------------------------------------- races

(defn- cold-buffer
  "A buffer of delivered entries old enough for the sweep to reclaim all of."
  [n]
  {:entries (mapv #(entry (str "cold-" %) 400) (range n))
   :cursor n})

(deftest no-result-is-lost-or-doubled-while-the-sweep-runs
  ;; Producers, consumers and the sweep all writing at once. The results
  ;; being enqueued are seconds old, so the sweep must reclaim NONE of them:
  ;; the expected set is exact, and any clobbered enqueue shows up as a
  ;; missing task-id rather than as a flake.
  (let [caller "stress-caller"
        n-producers 4
        per-producer 150
        expected (set (map #(str "t-" %) (range (* n-producers per-producer))))
        delivered (atom [])
        stop? (atom false)
        ;; The sweep plants a fresh cold buffer before each pass, so it is
        ;; genuinely deleting keys throughout the run rather than rebuilding
        ;; an unchanged map after the first pass.
        sweeper (future
                  (loop [n 0]
                    (when-not @stop?
                      (swap! ar/buffers assoc (str "cold-caller-" n) (cold-buffer 50))
                      (ar/gc-expired!)
                      (recur (inc n)))))
        consumers (mapv (fn [_]
                          (future
                            (loop []
                              (when-not @stop?
                                (if-let [{:keys [results]} (ar/drain! caller)]
                                  (swap! delivered into (map :task-id results))
                                  (Thread/sleep 1))
                                (recur)))))
                        (range 3))
        producers (mapv (fn [p]
                          (future
                            (dotimes [i per-producer]
                              (ar/enqueue-result!
                               caller {:task-id (str "t-" (+ (* p per-producer) i))
                                       :tool "probe" :status :ok}))))
                        (range n-producers))]
    (run! deref producers)
    (Thread/sleep 100)
    (reset! stop? true)
    (run! deref consumers)
    @sweeper
    ;; Whatever the consumers had not reached before they were stopped.
    (loop []
      (when-let [{:keys [results]} (ar/drain! caller)]
        (swap! delivered into (map :task-id results))
        (recur)))
    (let [got @delivered]
      (testing "exactly once"
        (is (= (count got) (count (distinct got)))
            "a result delivered twice means a cursor went backwards"))
      (testing "and not at all lost"
        (is (= expected (set got))
            "a missing task-id means one writer clobbered another")))))

(deftest the-sweep-does-not-strand-a-caller-mid-drain
  ;; A caller holding a partly drained buffer while the sweep reclaims the
  ;; entries it has already been given. The undelivered tail must survive
  ;; intact and in order, or the caller silently skips results.
  (let [caller "midway-caller"]
    (dotimes [i 6]
      (ar/enqueue-result! caller {:task-id (str "m-" i) :tool "probe" :status :ok}))
    (let [{:keys [results]} (ar/drain! caller)
          seen (mapv :task-id results)
          key (ffirst @ar/buffers)]
      (is (seq seen) "precondition: the caller was given something")
      ;; Age only what the caller has already seen.
      (swap! ar/buffers update key
             (fn [{:keys [entries cursor] :as buf}]
               (assoc buf :entries
                      (vec (map-indexed (fn [idx e]
                                          (cond-> e
                                            (< idx cursor)
                                            (assoc :timestamp (- (now-s) 400))))
                                        entries)))))
      (ar/gc-expired!)
      (let [rest-seen (loop [acc []]
                        (if-let [{:keys [results]} (ar/drain! caller)]
                          (recur (into acc (map :task-id results)))
                          acc))]
        (is (= (mapv #(str "m-" %) (range 6)) (into seen rest-seen))
            "every result arrives exactly once and in order across the sweep")))))
