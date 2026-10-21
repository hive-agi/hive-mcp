(ns hive-mcp.server.log-guard-test
  "A closed log sink makes no native call, and its shutdown hook is idempotent.

   The native file writer calls libc through JNA. The JVM that crashed with
   SIGSEGV in libc did so while its shutdown hooks ran, so the sink must stop
   reaching the writer once closed: a line offered or written after close, or a
   second run of the hook, must never reach it.

   Kanban 20261020231003-2a41a962."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.server.log-guard :as guard]
            [hive-mcp.server.log-sink :as sink]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord CountingWriter [calls]
  sink/LineWriter
  (write-line! [_ line] (swap! calls conj line)))

(defn- await-count [calls n]
  (let [end (+ (System/currentTimeMillis) 2000)]
    (while (and (< (count @calls) n) (< (System/currentTimeMillis) end))
      (Thread/sleep 5))
    (count @calls)))

(deftest guarded-writer-refuses-after-close
  (let [calls (atom [])
        w (guard/guarded-writer (->CountingWriter calls))]
    (sink/write-line! w "before")
    (is (true? (guard/close-writer! w 1000)))
    (sink/write-line! w "after")
    (is (= ["before"] @calls) "nothing reaches the delegate after close")
    (is (true? (guard/close-writer! w 1000)) "closing twice is a no-op")
    (is (guard/closed? w))))

(deftest close-waits-for-an-in-flight-write
  (let [entered (promise)
        release (promise)
        done (atom false)
        w (guard/guarded-writer
           (reify sink/LineWriter
             (write-line! [_ _] (deliver entered true) @release (reset! done true))))
        writer (future (sink/write-line! w "slow"))]
    (is (true? (deref entered 1000 false)))
    (let [closer (future (guard/close-writer! w 2000))]
      (Thread/sleep 50)
      (is (not (realized? closer)) "close blocks while the native write is in flight")
      (deliver release true)
      (is (true? (deref closer 2000 :timeout)))
      (is (true? @done) "the in-flight write finished before close returned"))
    (deref writer 1000 nil)))

(deftest closed-sink-makes-no-native-call
  (let [calls (atom [])
        s (sink/make-sink (guard/guarded-writer (->CountingWriter calls)) {:capacity 8})]
    (sink/offer! s "first")
    (is (= 1 (await-count calls 1)))
    (guard/close-sink! s 1000)
    (testing "a line offered after close is dropped, never written"
      (is (= :dropped (sink/offer! s "late")))
      (Thread/sleep 50)
      (is (= ["first"] @calls)))
    (testing "the drain thread has stopped"
      (is (not (.isAlive ^Thread (:thread s)))))))

(deftest shutdown-hook-runs-twice-safely
  (let [calls (atom [])
        s (sink/make-sink (guard/guarded-writer (->CountingWriter calls)) {:capacity 8})
        hook (guard/close-sinks-fn [s] 1000)]
    (hook)
    (hook)
    (sink/write-line! (:writer s) "after-hook")
    (is (empty? @calls) "no write reached the native writer after the hook")
    (is (guard/closed? (:writer s)))))

(deftest real-file-writer-touches-no-file-after-close
  (let [dir (java.nio.file.Files/createTempDirectory
             "hive-log-guard" (make-array java.nio.file.attribute.FileAttribute 0))
        path (.resolve dir "coordinator.log")
        w (guard/guarded-writer (sink/file-line-writer (.toFile path)))]
    (try
      (guard/close-writer! w 1000)
      (sink/write-line! w "after close")
      (is (not (java.nio.file.Files/exists path (make-array java.nio.file.LinkOption 0)))
          "no openat ran: the file was never created")
      (finally
        (java.nio.file.Files/deleteIfExists path)
        (java.nio.file.Files/deleteIfExists dir)))))

(deftest embed-warnings-reach-the-file-sink
  (let [s (sink/make-sink (->CountingWriter (atom [])) {:capacity 8 :start? false})
        append! (guard/ns-warn-appender-fn s ["hive-mcp.embeddings."])]
    (append! {:level :warn :?ns-str "hive-mcp.embeddings.resilient"
              :vargs ["embedding provider" :ollama "failed after" 9800 "ms"]
              :instant (java.util.Date. 0)})
    (append! {:level :info :?ns-str "hive-mcp.embeddings.resilient"
              :vargs ["fine"] :instant (java.util.Date. 0)})
    (append! {:level :warn :?ns-str "hive-mcp.server.core"
              :vargs ["unrelated"] :instant (java.util.Date. 0)})
    (append! {:level :error :?ns-str "hive-mcp.embeddings.ollama"
              :vargs ["auth failed" "Authorization: Bearer sk-abc123 api_key=hunter2"]
              :instant (java.util.Date. 0)})
    (let [lines (vec (:queue s))]
      (is (= 2 (count lines)))
      (is (str/includes? (first lines) "WARN hive-mcp.embeddings.resilient"))
      (is (str/includes? (first lines) "failed after 9800 ms"))
      (is (not (str/includes? (second lines) "sk-abc123")))
      (is (not (str/includes? (second lines) "hunter2"))))))
