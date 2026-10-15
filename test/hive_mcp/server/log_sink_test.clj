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
                :vargs args :output_ (delay "API_KEY=secret must never reach disk")}))
    (vec (:queue s))))

(deftrifecta boot-file-filter
  hive-mcp.server.log-sink-test/boot-file-contract
  {:golden-path "test/golden/server/boot-file-filter.edn"
   :cases {:safe [[:info [(sink/boot-event :addon :init 12.0 :ok)]]]
           :secret [[:info ["[boot-timing] init API_KEY=secret"]]
                    [:warn [(sink/boot-event :addon :init 12.0 :ok)]]
                    [:info [(sink/boot-event "API_KEY=secret" :init 12.0 :ok)]]
                    [:info [(sink/boot-event :addon :init 12.0 :bad)]]
                    [:info [(sink/boot-event :addon :init Double/NaN :ok)]]
                    [:info ["[boot-timing] init" "secret"]]]}
   :gen (gen/fmap (fn [s] [[:warn [(str "[boot-timing] " s)]]
                            [:info [(str "[boot-timing] init " s)]]]) gen/string-alphanumeric)
   :pred empty?
   :num-tests 30
   :mutations [["unfiltered-appender" (fn [records] (mapv (comp str first second) records))]]
   :assert (fn []
             (let [out (boot-file-contract [[:info [(sink/boot-event :hive-addon.loader/start :addon 12.0 :ok)]]
                                            [:info [(sink/boot-event "API_KEY=secret" :addon 12.0 :ok)]]])]
               (is (= 1 (count out)))
               (is (str/includes? (first out) "addon=hive-addon.loader/start addon-hash="))
               (is (not (str/includes? (first out) "secret")))))})

(deftest coordinator-appender-registration
  (require 'hive-mcp.server.core)
  (let [config @(requiring-resolve 'taoensso.timbre/*config*)
        original (get-in config [:appenders :coordinator-file])
        register! (requiring-resolve 'hive-mcp.server.core/register-coordinator-file!)
        dir (java.nio.file.Files/createTempDirectory "registered-boot-log" (make-array java.nio.file.attribute.FileAttribute 0))
        path (.resolve dir "coordinator.log")
        ;; Inject the disposable file writer into the actual core registration,
        ;; not a second, manually constructed appender.
        real-sink (sink/make-sink (sink/file-line-writer (.toFile path)))
        formatted? (atom false)]
    (try
      (is (fn? (:fn original)))
      (register! real-sink)
      (let [registered (get-in @(requiring-resolve 'taoensso.timbre/*config*)
                               [:appenders :coordinator-file])
            append! (:fn registered)]
        (is (false? (:async? registered)))
        (append! {:level :info :?ns-str "hive-mcp.server.core"
                  :vargs ["[boot-timing] init API_KEY=secret"]
                  :output_ (delay (reset! formatted? true) "API_KEY=secret")})
        (append! {:level :info :?ns-str "hive-mcp.server.core"
                  :vargs [(sink/boot-event :hive-addon.loader/start :init 12.0 :ok)]
                  :output_ (delay (reset! formatted? true) "API_KEY=secret")})
        (is (false? @formatted?) "neither rejected nor accepted payload can force output")
        (let [deadline (+ (System/currentTimeMillis) 2000)]
          (while (and (not (java.nio.file.Files/exists path (make-array java.nio.file.LinkOption 0)))
                      (< (System/currentTimeMillis) deadline))
            (Thread/sleep 10)))
        (is (java.nio.file.Files/exists path (make-array java.nio.file.LinkOption 0)))
        (when (java.nio.file.Files/exists path (make-array java.nio.file.LinkOption 0))
          (let [lines (str/split-lines (slurp (.toFile path)))]
            (is (= 1 (count lines)))
            (is (str/includes? (first lines) "addon=hive-addon.loader/start addon-hash="))
            (is (str/includes? (first lines) "phase=init elapsed-ms=12.0 status=ok"))
            (is (not (str/includes? (first lines) "secret"))))))
      (finally
        ((requiring-resolve 'taoensso.timbre/merge-config!)
         {:appenders {:coordinator-file original}})
        (sink/stop! real-sink)
        (java.nio.file.Files/deleteIfExists path)
        (java.nio.file.Files/deleteIfExists dir)))))

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

(defn parent-swap-contract
  "A concurrent rename/symlink swap of the parent cannot write through its alias."
  [_]
  (let [root (java.nio.file.Files/createTempDirectory "hive-log-swap" (make-array java.nio.file.attribute.FileAttribute 0))
        parent (.resolve root "hive-mcp")
        moved (.resolve root "moved")
        victim (.resolve root "victim")
        target (.resolve victim "coordinator.log")
        writer (sink/file-line-writer (.toFile (.resolve parent "coordinator.log")))
        attrs (make-array java.nio.file.attribute.FileAttribute 0)]
    (try
      (java.nio.file.Files/createDirectory victim attrs)
      (java.nio.file.Files/write target (.getBytes "untouched" "UTF-8") (make-array java.nio.file.OpenOption 0))
      (sink/write-line! writer "first")
      (java.nio.file.Files/move parent moved (make-array java.nio.file.CopyOption 0))
      (java.nio.file.Files/createSymbolicLink parent victim attrs)
      (let [refused? (try (sink/write-line! writer "second") false
                          (catch Exception _ true))]
        {:refused? refused? :victim (slurp (.toFile target))
         :moved (slurp (.toFile (.resolve moved "coordinator.log")))})
      (finally
        (doseq [p [parent (.resolve moved "coordinator.log") moved target victim root]]
          (java.nio.file.Files/deleteIfExists p))))))

(deftrifecta parent-swap-safety
  hive-mcp.server.log-sink-test/parent-swap-contract
  {:golden-path "test/golden/server/parent-swap-safety.edn"
   :cases {:swap :swap}
   :gen (gen/return :swap)
   :pred (fn [{:keys [refused? victim moved]}]
           (and refused? (= "untouched" victim) (= "first\n" moved)))
   :num-tests 5
   :mutations [["follows-swapped-parent" (fn [_] {:refused? false :victim "second" :moved "first\n"})]]
   :assert (fn [] (is (= {:refused? true :victim "untouched" :moved "first\n"}
                         (parent-swap-contract :swap))))})

(defn pinned-parent-contract
  "Rename the directory and install a symlink precisely between verified
   directory open and relative file open; the pinned handle stays on its inode."
  [_]
  (let [root (java.nio.file.Files/createTempDirectory "hive-log-pinned" (make-array java.nio.file.attribute.FileAttribute 0))
        parent (.resolve root "hive-mcp")
        moved (.resolve root "moved")
        victim (.resolve root "victim")
        target (.resolve victim "coordinator.log")
        attrs (make-array java.nio.file.attribute.FileAttribute 0)]
    (try
      (java.nio.file.Files/createDirectory victim attrs)
      (java.nio.file.Files/write target (.getBytes "untouched" "UTF-8") (make-array java.nio.file.OpenOption 0))
      (let [p (#'sink/private-file! (.toFile (.resolve parent "coordinator.log")))]
        (try
          (java.nio.file.Files/move parent moved (make-array java.nio.file.CopyOption 0))
          (java.nio.file.Files/createSymbolicLink parent victim attrs)
          (let [f (#'sink/open-verified-fd! p (:name p))]
            (try (#'sink/write-fd! f (.getBytes "pinned" "UTF-8"))
                 (finally (#'sink/close-fd! f))))
          {:victim (slurp (.toFile target))
           :moved (slurp (.toFile (.resolve moved "coordinator.log")))}
          (finally (#'sink/close-fd! p))))
      (finally
        (doseq [p [parent (.resolve moved "coordinator.log") moved target victim root]]
          (java.nio.file.Files/deleteIfExists p))))))

(deftrifecta pinned-parent-safety
  hive-mcp.server.log-sink-test/pinned-parent-contract
  {:golden-path "test/golden/server/pinned-parent-safety.edn"
   :cases {:swap :swap}
   :gen (gen/return :swap)
   :pred (fn [{:keys [victim moved]}]
           (and (= "untouched" victim) (= "pinned" moved)))
   :num-tests 5
   :mutations [["follows-alias-after-check" (fn [_] {:victim "pinned" :moved ""})]]
   :assert (fn [] (is (= {:victim "untouched" :moved "pinned"}
                         (pinned-parent-contract :swap))))})

(defn rotation-alias-contract
  "A symlink generation is never overwritten, and rotation after a parent
   swap remains within the pinned original directory."
  [_]
  (let [root (java.nio.file.Files/createTempDirectory "hive-log-rotation-swap" (make-array java.nio.file.attribute.FileAttribute 0))
        parent (.resolve root "hive-mcp")
        moved (.resolve root "moved")
        victim (.resolve root "victim")
        target (.resolve victim "coordinator.log")
        attrs (make-array java.nio.file.attribute.FileAttribute 0)
        writer (sink/file-line-writer (.toFile (.resolve parent "coordinator.log")))]
    (try
      (java.nio.file.Files/createDirectory victim attrs)
      (java.nio.file.Files/write target (.getBytes "untouched" "UTF-8") (make-array java.nio.file.OpenOption 0))
      (sink/write-line! writer "first")
      (let [p (#'sink/private-file! (.toFile (.resolve parent "coordinator.log")))]
        (try
          (java.nio.file.Files/move parent moved (make-array java.nio.file.CopyOption 0))
          (java.nio.file.Files/createSymbolicLink parent victim attrs)
          (#'sink/rotate-generations! p)
          (finally (#'sink/close-fd! p))))
      (let [rotated? (java.nio.file.Files/exists (.resolve moved "coordinator.log.1") (make-array java.nio.file.LinkOption 0))
            untouched? (= "untouched" (slurp (.toFile target)))]
        (java.nio.file.Files/deleteIfExists parent)
        (sink/write-line! (sink/file-line-writer (.toFile (.resolve moved "coordinator.log"))) "next")
        (java.nio.file.Files/deleteIfExists (.resolve moved "coordinator.log.1"))
        (java.nio.file.Files/createSymbolicLink (.resolve moved "coordinator.log.1") target attrs)
        (let [p (#'sink/private-file! (.toFile (.resolve moved "coordinator.log")))]
          (try
            {:rotated? rotated? :untouched? untouched?
             :refused? (try (#'sink/rotate-generations! p)
                            false (catch Exception _ true))}
            (finally (#'sink/close-fd! p)))))
      (finally
        (doseq [p [parent (.resolve moved "coordinator.log.1") (.resolve moved "coordinator.log") moved target victim root]]
          (java.nio.file.Files/deleteIfExists p))))))

(deftrifecta rotation-alias-safety
  hive-mcp.server.log-sink-test/rotation-alias-contract
  {:golden-path "test/golden/server/rotation-alias-safety.edn"
   :cases {:swap :swap}
   :gen (gen/return :swap)
   :pred (fn [{:keys [rotated? untouched? refused?]}] (and rotated? untouched? refused?))
   :num-tests 5
   :mutations [["name-based-rotation" (fn [_] {:rotated? false :untouched? false :refused? false})]]
   :assert (fn [] (is (= {:rotated? true :untouched? true :refused? true}
                         (rotation-alias-contract :swap))))})
