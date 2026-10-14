(ns hive-mcp.server.log-sink-test
  "Pins the console log sink: admission is bounded and never blocks, and a
   blocked writer cannot stall the logging caller.

   The writer is a stub LineWriter injected through the port; nothing here
   touches *err*, Timbre's global config or a running server.

   Mutants are self-contained and never call the subject var, which the
   trifecta has rebound to the mutant while it runs them."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.server.log-sink :as sink]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Stubs
;; =============================================================================

(defrecord NullWriter []
  sink/LineWriter
  (write-line! [_ _] nil))

(defrecord BlockedWriter [gate entered written]
  sink/LineWriter
  (write-line! [_ line]
    (deliver entered true)
    @gate
    (swap! written conj line)))

(defn- blocked-writer []
  (->BlockedWriter (promise) (promise) (atom [])))

;; =============================================================================
;; Admission — golden + property + mutation
;; =============================================================================

(defn run-admission
  "Unary adapter: {:capacity n :lines [s]} -> admission summary, over a sink
   with no drain thread so the outcome is deterministic."
  [{:keys [capacity lines]}]
  (let [s (sink/make-sink (->NullWriter) {:capacity capacity :start? false})
        results (mapv #(sink/offer! s %) lines)]
    {:queued  (count (filter #{:queued} results))
     :dropped (sink/dropped s)
     :depth   (sink/depth s)
     :results results}))

(defn- admission-holds?
  [{:keys [queued dropped depth results]}]
  (and (= (count results) (+ queued dropped))
       (= queued depth)
       ;; drops only ever follow a full queue: no :queued after a :dropped
       (let [[_ tail] (split-with #{:queued} results)]
         (every? #{:dropped} tail))))

(deftrifecta admission-contract
  hive-mcp.server.log-sink-test/run-admission
  {:golden-path "test/golden/server/log-sink-admission.edn"
   :cases       {:empty        {:capacity 2 :lines []}
                 :under        {:capacity 3 :lines ["a" "b"]}
                 :exactly-full {:capacity 2 :lines ["a" "b"]}
                 :overflow     {:capacity 2 :lines ["a" "b" "c" "d" "e"]}}
   :gen         (gen/let [cap (gen/choose 1 8)
                          lines (gen/vector gen/string-alphanumeric 0 20)]
                  {:capacity cap :lines lines})
   :pred        (fn [out] (admission-holds? out))
   :num-tests   200
   :mutations   [["unbounded — queues everything, the old synchronous shape"
                  (fn [{:keys [lines]}]
                    {:queued (count lines) :dropped 0 :depth (count lines)
                     :results (mapv (constantly :queued) lines)})]
                 ["drops-everything"
                  (fn [{:keys [lines]}]
                    {:queued 0 :dropped (count lines) :depth 0
                     :results (mapv (constantly :dropped) lines)})]
                 ["uncounted-drops"
                  (fn [{:keys [capacity lines]}]
                    (let [q (min capacity (count lines))]
                      {:queued q :dropped 0 :depth q
                       :results (vec (concat (repeat q :queued)
                                             (repeat (- (count lines) q) :dropped)))}))]]
   :assert      (fn []
                  (let [out (run-admission {:capacity 2 :lines ["a" "b" "c"]})]
                    (is (= [:queued :queued :dropped] (:results out)))
                    (is (= 1 (:dropped out)))
                    (is (= 2 (:depth out)))))})

;; =============================================================================
;; Backpressure — a blocked writer never blocks the logging caller
;; =============================================================================

(deftest blocked-writer-does-not-block-callers
  (let [w (blocked-writer)
        s (sink/make-sink w {:capacity 16})
        log! (sink/appender-fn s)
        n 10000]
    (try
      (log! {:output_ (delay "first")})
      (is (true? (deref (:entered w) 2000 false))
          "the drain thread is now stuck inside the writer")
      (let [t0 (System/nanoTime)]
        (dotimes [i n] (log! {:output_ (delay (str "line " i))}))
        (let [ms (/ (- (System/nanoTime) t0) 1e6)]
          (is (< ms 2000.0)
              (str n " log calls must return while the writer is blocked; took " ms " ms"))))
      (testing "overflow is dropped and counted, the queue stays bounded"
        (is (<= (sink/depth s) 16))
        (is (>= (sink/dropped s) (- n 16))))
      (finally
        (deliver (:gate w) true)
        (sink/stop! s)))
    (testing "once released, the writer resumes draining"
      (let [deadline (+ (System/currentTimeMillis) 2000)]
        (while (and (empty? @(:written w))
                    (< (System/currentTimeMillis) deadline))
          (Thread/sleep 10)))
      (is (some #{"first"} @(:written w))))))

(defn file-write-contract
  "Unary adapter: write lines through the file port in a disposable directory."
  [lines]
  (let [dir (java.nio.file.Files/createTempDirectory "hive-log-test" (make-array java.nio.file.attribute.FileAttribute 0))
        file (.toFile (.resolve dir "coordinator.log"))
        writer (sink/file-line-writer file)]
    (try
      (doseq [line lines] (sink/write-line! writer line))
      {:lines (str/split-lines (slurp file))
       :private? (= "rw-------" (java.nio.file.attribute.PosixFilePermissions/toString
                                     (java.nio.file.Files/getPosixFilePermissions (.toPath file)
                                       (make-array java.nio.file.LinkOption 0))))}
      (finally
        (java.nio.file.Files/deleteIfExists (.toPath file))
        (java.nio.file.Files/deleteIfExists dir)))))

(deftrifecta private-coordinator-log
  hive-mcp.server.log-sink-test/file-write-contract
  {:golden-path "test/golden/server/private-coordinator-log.edn"
   :cases {:one ["[boot-timing] addon a 10 ms"]
           :append ["first" "second"]}
   :gen (gen/vector gen/string-alphanumeric 1 8)
   :pred (fn [out] (and (:private? out) (vector? (:lines out))))
   :num-tests 25
   :mutations [["drops-last-line"
                (fn [lines] {:lines (vec (butlast lines)) :private? true})]
               ["public-log-file"
                (fn [lines] {:lines lines :private? false})]]
   :assert (fn []
             (is (= {:lines ["first" "second"] :private? true}
                    (file-write-contract ["first" "second"]))))})

(defn boot-file-contract
  "Exercise the same filtered appender factory registered by server.core through a stub sink."
  [records]
  (let [s (sink/make-sink (->NullWriter) {:capacity 32 :start? false})
        append! (sink/boot-appender-fn s)]
    (doseq [[level args] records]
      (append! {:level level :?ns-str "hive-mcp.server.core"
                :vargs args :output_ (delay (str (first args)))}))
    (vec (:queue s))))

(deftrifecta boot-file-filter
  hive-mcp.server.log-sink-test/boot-file-contract
  {:golden-path "test/golden/server/boot-file-filter.edn"
   :cases {:safe [[:info ["[boot-timing] init :addon 12.0 ms"]]]
           :secret [[:info ["API_KEY=secret"]]
                    [:warn ["[boot-timing] password"]]
                    [:info ["[boot-timing] init :addon\nsecret"]]
                    [:info ["[boot-timing] init" "secret"]]]}
   :gen (gen/fmap (fn [s] [[:warn [(str "[boot-timing] " s)]]
                            [:info [(str "secret=" s)]]]) gen/string-alphanumeric)
   :pred empty?
   :num-tests 30
   :mutations [["unfiltered-appender" (fn [records] (mapv (comp first second) records))]]
   :assert (fn []
             (is (= ["[boot-timing] init :addon 12.0 ms"]
                    (boot-file-contract [[:info ["[boot-timing] init :addon 12.0 ms"]]
                                         [:info ["secret"]]]))))})

(deftest coordinator-appender-registration
  (require 'hive-mcp.server.core)
  (let [config @(requiring-resolve 'taoensso.timbre/*config*)
        append! (get-in config [:appenders :coordinator-file :fn])
        file-sink @(requiring-resolve 'hive-mcp.server.core/file-sink)
        before (sink/depth file-sink)
        formatted? (atom false)]
    (is (fn? append!))
    (is (false? (get-in config [:appenders :coordinator-file :async?])))
    (append! {:level :info :?ns-str "hive-mcp.server.core"
              :vargs ["unrelated secret"]
              :output_ (delay (reset! formatted? true) "unrelated secret")})
    (is (false? @formatted?) "disallowed payload must never be formatted")
    (is (= before (sink/depth file-sink)) "disallowed payload never reaches file sink")))

(deftest file-aliases-and-rotation
  (let [dir (java.nio.file.Files/createTempDirectory "hive-log-safe" (make-array java.nio.file.attribute.FileAttribute 0))
        path (.resolve dir "coordinator.log")
        other (.resolve dir "other")
        writer (sink/file-line-writer (.toFile path))
        no-links (make-array java.nio.file.LinkOption 0)]
    (try
      (java.nio.file.Files/createSymbolicLink path other (make-array java.nio.file.attribute.FileAttribute 0))
      (is (thrown? Exception (sink/write-line! writer "symlink")))
      (java.nio.file.Files/delete path)
      (java.nio.file.Files/createDirectory path (make-array java.nio.file.attribute.FileAttribute 0))
      (is (thrown? Exception (sink/write-line! writer "directory")))
      (java.nio.file.Files/delete path)
      (java.nio.file.Files/write other (.getBytes "sentinel" "UTF-8") (make-array java.nio.file.OpenOption 0))
      (java.nio.file.Files/createLink path other)
      (is (thrown? Exception (sink/write-line! writer "hardlink")))
      (is (= "sentinel" (slurp (.toFile other))))
      (java.nio.file.Files/delete path)
      (dotimes [_ 21] (sink/write-line! writer (apply str (repeat 200000 "x"))))
      (let [generations (map #(.resolve dir (str "coordinator.log" (when (pos? %) (str "." %)))) (range 4))]
        (is (every? #(java.nio.file.Files/exists % no-links) generations))
        (is (every? #(<= (java.nio.file.Files/size %) sink/max-file-bytes) generations))
        (is (not (java.nio.file.Files/exists (.resolve dir "coordinator.log.4") no-links))))
      (finally
        (with-open [stream (java.nio.file.Files/list dir)]
          (doseq [p (iterator-seq (.iterator stream))]
            (java.nio.file.Files/deleteIfExists p)))
        (java.nio.file.Files/deleteIfExists dir)))))

(deftest private-target-and-parent-alias
  (let [dir (java.nio.file.Files/createTempDirectory "hive-log-permissions" (make-array java.nio.file.attribute.FileAttribute 0))
        path (.resolve dir "coordinator.log")
        link (.resolveSibling dir (str (.getFileName dir) "-alias"))
        owner-only (java.nio.file.attribute.PosixFilePermissions/fromString "rw-------")
        public (java.nio.file.attribute.PosixFilePermissions/fromString "rw-r--r--")]
    (try
      (java.nio.file.Files/write path (.getBytes "original" "UTF-8") (make-array java.nio.file.OpenOption 0))
      (java.nio.file.Files/setPosixFilePermissions path public)
      (is (thrown? Exception (sink/write-line! (sink/file-line-writer (.toFile path)) "unsafe")))
      (is (= "original" (slurp (.toFile path))))
      (java.nio.file.Files/delete path)
      (sink/write-line! (sink/file-line-writer (.toFile path)) "safe")
      (is (= owner-only (java.nio.file.Files/getPosixFilePermissions path (make-array java.nio.file.LinkOption 0))))
      (java.nio.file.Files/createSymbolicLink link dir (make-array java.nio.file.attribute.FileAttribute 0))
      (is (thrown? Exception
                   (sink/write-line! (sink/file-line-writer (.toFile (.resolve link "coordinator.log"))) "unsafe")))
      (is (= "safe\n" (slurp (.toFile path))))
      (finally
        (java.nio.file.Files/deleteIfExists link)
        (java.nio.file.Files/deleteIfExists path)
        (java.nio.file.Files/deleteIfExists dir)))))
