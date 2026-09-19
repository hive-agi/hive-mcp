(ns hive-mcp.channel.async-result-journal-test
  "That an async result outlives the process that was holding it.

   The end-to-end tests simulate the kill that motivated the journal by
   throwing the atom away with `reset-all!` and restoring from disk, which is
   exactly what the OOM killer did to the coordinator twice on 2026-09-16:
   heap gone, file intact, no shutdown hook run."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.async-result :as ar]
            [hive-mcp.channel.async-result-journal :as journal]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private ^:dynamic *path* nil)

(defn- with-temp-journal [t]
  (let [p (str (System/getProperty "java.io.tmpdir")
               "/hive-async-journal-test-" (random-uuid) ".edn")]
    (binding [journal/*journal-path* p
              *path* p]
      (ar/reset-all!)
      (try (t)
           (finally
             (ar/reset-all!)
             (.delete (io/file p))
             (.delete (io/file (str p ".compacting"))))))))

(use-fixtures :each with-temp-journal)

(defn- kill-the-process!
  "Everything the JVM was holding, gone. The file is not."
  []
  (ar/reset-all!))

;; ---------------------------------------------------------------- replay

(deftest a-result-nobody-took-is-restored-undelivered
  (let [bufs (journal/replay [{:op :result :caller "c" :task-id "t1"
                               :entry {:task-id "t1" :status :completed :result 42}}])]
    (is (= [42] (mapv :result (get-in bufs ["c" :entries]))))
    (is (zero? (get-in bufs ["c" :cursor]))
        "a journalled entry is by definition one the caller never got")))

(deftest a-task-that-never-produced-a-result-is-restored-as-interrupted
  ;; The case the card is named for: accepted, acked, then killed with
  ;; nothing to show. Silence here means the caller waits on that ack forever.
  (let [bufs (journal/replay [{:op :submitted :caller "c" :task-id "t1"
                               :tool "memory" :at 1700000000000}])
        e (first (get-in bufs ["c" :entries]))]
    (is (= :interrupted (:status e)))
    (is (= "t1" (:task-id e)))
    (is (= "memory" (:tool e)))
    (is (= journal/interrupted-error (:error e))
        "and it says why, because nothing else ever will")))

(deftest a-result-supersedes-the-submission-that-preceded-it
  (let [bufs (journal/replay [{:op :submitted :caller "c" :task-id "t1" :tool "memory"}
                              {:op :result :caller "c" :task-id "t1"
                               :entry {:task-id "t1" :status :completed :result "done"}}])
        entries (get-in bufs ["c" :entries])]
    (is (= 1 (count entries)) "one task is one entry, not two")
    (is (= :completed (:status (first entries)))
        "the work finished, so :interrupted would be a lie")))

(deftest a-delivered-result-is-not-restored
  (let [bufs (journal/replay [{:op :result :caller "c" :task-id "t1"
                               :entry {:task-id "t1" :status :completed}}
                              {:op :delivered :caller "c" :task-ids ["t1"]}])]
    (is (= {} bufs) "re-handing a caller something it already has is a bug too")))

(deftest replay-keeps-arrival-order-per-caller
  (let [bufs (journal/replay (for [i (range 5)]
                               {:op :result :caller "c" :task-id (str "t" i)
                                :entry {:task-id (str "t" i)}}))]
    (is (= ["t0" "t1" "t2" "t3" "t4"]
           (mapv :task-id (get-in bufs ["c" :entries]))))))

;; ---------------------------------------------------------------- file

(deftest a-record-torn-in-half-by-the-kill-is-dropped-not-fatal
  ;; A process dying mid-append leaves a partial last line. That is the
  ;; NORMAL shape of this file after the crash it exists to survive, so it
  ;; must cost the torn record only, never the whole journal.
  (journal/append! {:op :result :caller "c" :task-id "t1" :entry {:task-id "t1"}})
  (journal/append! {:op :result :caller "c" :task-id "t2" :entry {:task-id "t2"}})
  (spit (io/file *path*) "{:op :result :caller \"c\" :task-id \"t3\" :entr" :append true)
  (let [records (journal/read-records)]
    (is (= 2 (count records)) "both intact records survive")
    (is (= ["t1" "t2"] (mapv :task-id records)))))

(deftest an-unreadable-journal-yields-nothing-rather-than-refusing-to-start
  (spit (io/file *path*) "not edn at all ][\n")
  (is (= [] (journal/read-records)))
  (is (= {} (journal/restore))
      "losing replay is bad; failing to boot is worse"))

(deftest a-failed-append-is-reported-not-thrown
  (binding [journal/*journal-path* "/proc/definitely/not/writable/journal.edn"]
    (is (false? (journal/append! {:op :result :caller "c" :task-id "t"}))
        "the caller must still get its result, so this cannot throw")
    (is (some? (:degraded (journal/status)))
        "and durability having silently stopped must be visible")))

(deftest compaction-creates-no-file-for-a-process-that-owes-nothing
  (is (= 0 (journal/compact! {})))
  (is (not (.exists (io/file *path*)))
      "a coordinator that never runs an async call leaves no journal behind"))

(deftest compaction-keeps-only-what-is-still-owed
  (journal/append! {:op :result :caller "c" :task-id "old" :entry {:task-id "old"}})
  (journal/compact! {"c" {:entries [{:task-id "a"} {:task-id "b"}] :cursor 1}})
  (let [records (journal/read-records)]
    (is (= ["b"] (mapv :task-id records))
        "the delivered entry and the stale record are both gone")))

;; ---------------------------------------------------------------- crash

(deftest a-result-survives-the-process-that-was-holding-it
  (ar/enqueue-result! "victim" {:task-id "t1" :tool "memory"
                                :status :completed :result "the payload"})
  (kill-the-process!)
  (is (zero? (:total-entries (ar/stats)))
      "precondition: the in-memory copy is genuinely gone")
  (is (= 1 (ar/restore!)))
  (let [{:keys [results]} (ar/drain! "victim")]
    (is (= ["t1"] (mapv :task-id results)))
    (is (= "the payload" (:result (first results)))
        "and it is the actual payload, not a placeholder")))

(deftest a-task-killed-before-it-finished-is-answered-rather-than-abandoned
  ;; The exact bug on kanban 20260721013318-1a9d46a7: the caller holds an ack
  ;; for work that died. It must eventually be TOLD, not left waiting.
  (ar/record-submission! "victim" "t-dead" "memory")
  (kill-the-process!)
  (is (= 1 (ar/restore!)))
  (let [r (first (:results (ar/drain! "victim")))]
    (is (= "t-dead" (:task-id r)))
    (is (= :interrupted (:status r))
        "the caller learns the work died instead of waiting on the ack")))

(deftest a-result-already-delivered-is-not-handed-over-twice-after-a-crash
  (ar/enqueue-result! "victim" {:task-id "t1" :tool "memory" :status :completed :result 1})
  (is (seq (:results (ar/drain! "victim"))) "precondition: the caller took it")
  (kill-the-process!)
  (is (zero? (ar/restore!)))
  (is (nil? (ar/drain! "victim"))
      "a crash must not resurrect something the caller already holds"))

(deftest the-submission-is-recorded-under-the-key-the-caller-drains
  ;; record-submission! normalises the caller id; the journal does not know
  ;; how. Recording the raw id would restore the entry into a buffer nobody
  ;; reads, and the caller would still be waiting.
  (ar/record-submission! "coordinator:12345" "t-dead" "memory")
  (kill-the-process!)
  (ar/restore!)
  (is (seq (:results (ar/drain! "coordinator:12345")))
      "the restored entry is reachable by the id that was acked"))

(deftest restoring-never-clobbers-a-live-buffer
  (ar/enqueue-result! "victim" {:task-id "t1" :tool "memory" :status :completed :result 1})
  (let [before (ar/stats)]
    (ar/restore!)
    (is (= (:total-entries before) (:total-entries (ar/stats)))
        "calling restore late must not duplicate what memory already holds")))

(deftest the-gc-pass-compacts-the-journal-to-what-is-still-owed
  (testing "a delivered result leaves nothing behind on disk"
    (ar/enqueue-result! "victim" {:task-id "t1" :tool "memory" :status :completed :result 1})
    (ar/drain! "victim")
    (ar/gc-expired!)
    (is (= [] (journal/read-records))
        "reclaiming the heap copy and the disk copy is one event, not two")))
