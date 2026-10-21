(ns hive-mcp.storage.contract-test
  "Contract facets for storage/: content hashing and the datalevin
   self-heal policy, plus the storage/ coverage gate.

   The recovery tests drive a driver-free ITxlogOps stub in a temp
   directory and restore the registered ops afterwards; the hash tests
   clear the mtime cache they fill."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.storage.file-hash :as fh]
            [hive-mcp.storage.recovery :as rec]
            [hive-mcp.test.coverage-margin :as margin]
            [hive-mcp.test.stub.txlog-ops :as stub]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg])
  (:import [java.security MessageDigest]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (fh/clear-hash-cache!)
                      (try (stub/with-stub-ops f) (finally (fh/clear-hash-cache!)))))

(defn- sha256 [s]
  (let [bs (.digest (MessageDigest/getInstance "SHA-256") (.getBytes (str s) "UTF-8"))]
    (apply str (map #(format "%02x" %) bs))))

(defn- tmp-dir []
  (doto (.toFile (java.nio.file.Files/createTempDirectory "m20-storage" (make-array java.nio.file.attribute.FileAttribute 0)))
    (.deleteOnExit)))

;;; ---------------------------------------------------------------------------
;;; file-hash
;;; ---------------------------------------------------------------------------

(deftrifecta-from-schema compute-hash-is-sha256-of-str
  #'fh/compute-hash
  {:in [:cat [:or :string :int :keyword :nil]]
   :out fh/Sha256Hex
   :rel (fn [[x] out] (= out (sha256 x)))
   :golden-path "test/golden/hive-mcp/storage/compute-hash.edn"
   :num-tests 200})

(golden/deftest-golden file-content-hash-arms-golden
  "test/golden/hive-mcp/storage/file-content-hash.edn"
  (let [f (io/file (tmp-dir) "a.txt")]
    (spit f "hello")
    {:present (fh/file-content-hash (str f))
     :cached  (fh/file-content-hash (str f))
     :absent  (fh/file-content-hash (str f ".missing"))
     :nil     (fh/file-content-hash nil)
     :cleared (fh/clear-hash-cache!)}))

;;; ---------------------------------------------------------------------------
;;; recovery
;;; ---------------------------------------------------------------------------

(def ^:private Msg
  [:enum "MDB_CORRUPTED" "txn-log corrupt" "MDB_BUSY" "lock held" "MDB_VERSION_MISMATCH" "boom" ""])

(def ^:private msgs-by-class
  "Msg members by the classification each one names on its own."
  {:wal-corrupt      ["MDB_CORRUPTED" "txn-log corrupt"]
   :version-mismatch ["MDB_VERSION_MISMATCH"]
   :lock-contention  ["MDB_BUSY" "lock held"]
   :unknown          ["boom" ""]})

(defn- open-failure
  "A throwable whose classification is `target`, or nil for :nil. The
   classifying message sits on the outer ex-info (with any Msg as its cause,
   so outer-wins precedence is exercised) or on the cause under a neutral
   outer message. :unknown keeps both messages neutral."
  [[target outer? i j free]]
  (let [pick    (fn [k n] (let [ms (msgs-by-class k)] (nth ms (mod n (count ms)))))
        neutral (pick :unknown j)]
    (case target
      :nil     nil
      :unknown (ex-info (pick :unknown i) {} (Exception. ^String neutral))
      (if outer?
        (ex-info (pick target i) {} (Exception. ^String free))
        (ex-info neutral {} (Exception. ^String (pick target i)))))))

(def ^:private OpenFailure
  "nil or a throwable. The generator picks the classification first: a
   uniform pick over two Msgs plus :maybe's nil drew :version-mismatch about
   17 times in 300, against a classify floor of 18."
  [:maybe {:gen/schema [:tuple [:enum :nil :wal-corrupt :version-mismatch :lock-contention :unknown]
                        :boolean nat-int? nat-int? Msg]
           :gen/fmap open-failure}
   [:fn (fn [t] (instance? Throwable t))]])

(deftrifecta-from-schema classification-walks-the-cause-chain
  #'rec/classify-open-failure
  {:in [:cat OpenFailure]
   :out rec/Classification
   :rel (fn [[t] out]
          (let [cls (fn [msg] (cond (re-find #"MDB_CORRUPTED|txn-log" msg) :wal-corrupt
                                    (re-find #"MDB_VERSION_MISMATCH" msg) :version-mismatch
                                    (re-find #"MDB_BUSY|lock" msg) :lock-contention))]
            (= out (if t
                     (or (cls (ex-message t)) (cls (ex-message (ex-cause t))) :unknown)
                     :unknown))))
   :classify (fn [_ out] out)
   :classify-domain rec/Classification
   :num-tests 300})

(deftest classification-coverage-has-margin
  ;; 4000 draws keep the measured share within a few points of the true one,
  ;; so this is stable without a pinned seed.
  (is (= {} (margin/thin-variants [:cat OpenFailure] #'rec/classify-open-failure
                                  (fn [_ out] out) rec/Classification 4000))
      "every classification is drawn at least half as often as an even split"))

(deftrifecta-from-schema quarantine-path-is-deterministic
  #'rec/quarantine-path
  {:in [:cat :string :int]
   :out :string
   :rel (fn [[p ts] out] (= out (str p ".corrupt." ts)))
   :num-tests 100})

(golden/deftest-golden truncate-tail-segments-golden
  "test/golden/hive-mcp/storage/truncate-tail.edn"
  (let [db (tmp-dir)
        seg (doto (io/file db "txlog") .mkdirs)]
    (spit (io/file seg "1.wal") "")
    (with-open [o (io/output-stream (io/file seg "2.wal"))] (.write o (byte-array 8)))
    (spit (io/file seg "3.wal") "garbage")
    (let [report (rec/truncate-tail! (str db) 42 (rec/current-txlog-ops))]
      (mapv (fn [r] (-> r (update :path #(.getName (io/file %)))
                        (update :backup #(some-> % io/file .getName))))
            report))))

(golden/deftest-golden heal-and-open-policies-golden
  "test/golden/hive-mcp/storage/heal-and-open.edn"
  (let [attempt (fn [policy msgs]
                  (let [left (atom msgs)
                        open (fn [] (if-let [m (first @left)]
                                      (do (swap! left rest) (throw (Exception. ^String m)))
                                      :conn))]
                    (try (rec/heal-and-open! {:policy policy :db-path (str (tmp-dir) "/db")} open)
                         (catch clojure.lang.ExceptionInfo e
                           (select-keys (ex-data e) [:err :classification :attempts])))))]
    (into (sorted-map)
          {:clean          (attempt nil [])
           :throw          (attempt {:strategy :throw} ["MDB_CORRUPTED"])
           :audit          (attempt {:strategy :audit} ["MDB_BUSY"])
           :truncate-empty (attempt {:strategy :truncate} ["MDB_CORRUPTED"])
           :quarantine-ok  (attempt {:strategy :quarantine} ["MDB_CORRUPTED"])
           :quarantine-lock (attempt {:strategy :quarantine} ["MDB_BUSY"])
           :chain          (attempt {:strategy [:truncate :quarantine]} ["MDB_CORRUPTED"])
           :exhausted      (attempt {:strategy :quarantine :max-attempts 1} ["MDB_CORRUPTED"])})))

(deftest quarantine-moves-the-directory-or-answers-nil
  (let [db (io/file (tmp-dir) "db")]
    (is (nil? (rec/quarantine! (str db))) "nothing to move")
    (.mkdirs db)
    (let [target (str db ".q")]
      (is (= target (rec/quarantine! (str db) target)))
      (is (not (.exists db)))
      (is (.exists (io/file target))))))

(deftest set-txlog-ops-round-trips
  (let [s (stub/->stub)]
    (is (identical? s (rec/set-txlog-ops! s)))
    (is (identical? s (rec/current-txlog-ops)))))

;;; ---------------------------------------------------------------------------
;;; Coverage gate
;;; ---------------------------------------------------------------------------

(coverage/deftest-contract-coverage storage-contracts
  {:paths ["src/hive_mcp/storage"]})

(deftest every-storage-contract-generates-both-sides
  (doseq [ns-sym '[hive-mcp.storage.file-hash hive-mcp.storage.recovery]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (testing (str ns-sym "/" fn-sym)
      (is (= 3 (count (mg/sample side {:size 3 :seed 7})))))))
