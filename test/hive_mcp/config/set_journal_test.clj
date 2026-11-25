(ns hive-mcp.config.set-journal-test
  "The config-set journal against a temp file: one line per attempt, the
   value redacted, the request identity recorded, and an I/O or redaction
   failure answered with false instead of an exception. The path and the
   redactor are arguments, so nothing is redefined."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.config.set-journal :as sj]
            [hive-mcp.config.test-support :as support]
            [hive-mcp.config.write-policy :as wp]
            [hive-mcp.context.request :as request]
            [hive-mcp.tools.consolidated.config :as config-tool]
            [hive-mcp.config.path :as config-path]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private cfg {:llm-providers {:openrouter {}}})

(defn- temp-path
  "A fresh journal path inside a fresh temp directory."
  []
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "config-set-journal" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (.deleteOnExit dir)
    (str (io/file dir "nested" "config-set-journal.ednl"))))

(defn- read-lines
  [path]
  (mapv edn/read-string (str/split-lines (slurp path))))

(defn- redact
  "The config tool's own redact-config (the redactor PR 2 wires in), run
   against an in-memory config so it never reads a file."
  [path v]
  (support/with-config cfg
    (@#'config-tool/redact-config (vec path) v)))

(defn- attempt
  [k v & {:as more}]
  (merge {:decision (wp/decide :warn k v cfg) :value v} more))

(deftest one-line-per-attempt-allowed-and-refused
  (let [path (temp-path)]
    (is (true? (sj/append! path redact (attempt "models.default-model" "a/b"))))
    (is (true? (sj/append! path redact (attempt "services.seal.gpg-bin" "/tmp/gpg"))))
    (is (true? (sj/append! path redact (attempt "foo.bar" 1))))
    (let [[allowed refused warned :as lines] (read-lines path)]
      (is (= 3 (count lines)))
      (is (= [:allow :refuse :would-refuse] (mapv :decision lines)))
      (is (= ["models.default-model" "services.seal.gpg-bin" "foo.bar"] (mapv :key lines)))
      (is (= [:models :default-model] (:path allowed)))
      (is (= :floor (:rule refused)))
      (is (= :not-allowlisted (:rule warned)))
      (is (every? #(= :warn (:mode %)) lines))
      (is (every? #(inst? (:at %)) lines)))))

(deftest a-sensitive-value-is-redacted
  (let [path (temp-path)]
    (sj/append! path redact (attempt "models.extra" {:api-key "sk-live-123" :name "m"}))
    (sj/append! path redact (attempt "secrets.openrouter" "sk-live-456"))
    (let [text (slurp path)
          [m s] (read-lines path)]
      (is (not (str/includes? text "sk-live")) "no secret reaches the file")
      (is (= "m" (get-in m [:value :name])))
      (is (= :set (get-in m [:value :api-key :state])))
      (is (= :set (get-in s [:value :state]))))))

(deftest the-request-identity-is-recorded
  (let [path (temp-path)
        ctx  (assoc (request/make-request-ctx {:caller-id "asserted" :session-id "s-1"})
                    :identity {:caller-id "ling-7" :verified? true})]
    (request/with-request-context ctx
      (sj/append! path redact (assoc (attempt "models.default-model" "x")
                                     :agent-id "ling-7"
                                     :identity (sj/request-identity))))
    (sj/append! path redact (assoc (attempt "models.default-model" "y")
                                   :identity (sj/request-identity)))
    (let [[inside outside] (read-lines path)]
      (is (= {:verified-caller-id "ling-7" :caller-id "ling-7" :verified? true
              :session-id "s-1" :agent-id "ling-7"}
             (select-keys inside [:verified-caller-id :caller-id :verified?
                                  :session-id :agent-id])))
      (is (= {:verified-caller-id nil :caller-id nil :verified? false :session-id nil}
             (select-keys outside [:verified-caller-id :caller-id :verified? :session-id]))))))

(deftest an-io-failure-does-not-throw
  (let [blocker (java.io.File/createTempFile "config-set-journal" ".blocker")
        path    (str (io/file blocker "journal.ednl"))]
    (.deleteOnExit blocker)
    (testing "the parent is a regular file, so the write cannot land"
      (is (false? (sj/append! path redact (attempt "models.default-model" "a/b")))))
    (testing "a redactor that throws is survived too"
      (is (false? (sj/append! (temp-path) (fn [_ _] (throw (ex-info "boom" {})))
                              (attempt "models.default-model" "a/b")))))))

(deftest default-path-sits-beside-the-process-config
  (is (= (io/file (.getParentFile (io/file config-path/config-path)) sj/journal-file-name)
         (io/file (sj/default-path))))
  (testing "a test JVM runs from its own config, so it never names the operator's journal"
    (when (config-path/test-jvm?)
      (is (not= (io/file (.getParentFile (io/file config-path/operator-config-path))
                         sj/journal-file-name)
                (io/file (sj/default-path)))))))

(deftest a-non-canonical-key-never-journals-its-value
  (let [path     (temp-path)
        calls    (atom 0)
        counting (fn [p v] (swap! calls inc) (redact p v))]
    (doseq [k ["Secrets.openrouter" ":secrets.x" "SECRETS.openrouter-api-key" "secrets..x"]]
      (is (true? (sj/append! path counting (attempt k "sk-live-789")))))
    (let [lines (read-lines path)]
      (is (= 4 (count lines)))
      (is (every? #(= :non-canonical (:rule %)) lines))
      (is (every? #(= sj/non-canonical-placeholder (:value %)) lines))
      (is (not (str/includes? (slurp path) "sk-live")))
      (is (zero? @calls) "the redactor is never asked to guess a path"))))

(deftest a-long-value-is-truncated
  (let [path (temp-path)
        big  (apply str (repeat (* 4 sj/max-value-chars) "x"))]
    (sj/append! path redact (attempt "models.default-model" big))
    (let [{:keys [value]} (first (read-lines path))]
      (is (true? (:truncated value)))
      (is (= (count (pr-str big)) (:chars value)))
      (is (= sj/max-value-chars (count (:prefix value))))))
  (is (= "short" (sj/bound-value "short"))))

(deftest concurrent-appends-never-interleave
  (let [path (temp-path)
        n    64
        big  (apply str (repeat 1500 "y"))
        futs (doall (for [i (range n)]
                      (future (sj/append! path redact
                                          (attempt "models.default-model" (str i "-" big))))))]
    (is (every? true? (map deref futs)))
    (let [lines (read-lines path)]
      (is (= n (count lines)) "one whole, readable line per append")
      (is (= (set (map str (range n)))
             (set (map #(first (str/split (:value %) #"-")) lines)))))))

(deftest a-full-journal-is-rotated
  (let [path (temp-path)
        f    (io/file path)]
    (io/make-parents f)
    (with-open [raf (java.io.RandomAccessFile. f "rw")]
      (.setLength raf (inc sj/max-journal-bytes)))
    (is (true? (sj/append! path redact (attempt "models.default-model" "a/b"))))
    (is (= 1 (count (read-lines path))) "the new journal starts with this line")
    (is (= (inc sj/max-journal-bytes) (.length (io/file (str path ".1")))))))
