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

(defn- fill!
  "Make the journal file at `path` one byte past the rotation size, its
   first bytes `marker` so the file can be told apart after it moves."
  [path marker]
  (let [f (io/file path)]
    (io/make-parents f)
    (spit f marker)
    (with-open [raf (java.io.RandomAccessFile. f "rw")]
      (.setLength raf (inc sj/max-journal-bytes)))
    f))

(defn- rotated-files
  "Every rotated journal beside `path`, by name."
  [path]
  (let [f (io/file path)]
    (->> (.listFiles (.getParentFile f))
         (filter #(and (not= (.getName f) (.getName %))
                       (str/starts-with? (.getName %) "config-set-journal.")))
         (sort-by #(.getName %)))))

(defn- head [^java.io.File f n]
  (with-open [r (io/reader f)]
    (let [buf (char-array n)] (.read r buf 0 n) (String. buf))))

(deftest a-full-journal-is-rotated-to-a-timestamped-name
  (let [path (temp-path)]
    (fill! path "first")
    (is (true? (sj/append! path redact (attempt "models.default-model" "a/b"))))
    (is (= 1 (count (read-lines path))) "the new journal starts with this line")
    (let [[r :as rs] (rotated-files path)]
      (is (= 1 (count rs)))
      (is (re-matches #"config-set-journal\.\d{8}T\d{6}Z\.ednl" (.getName r)))
      (is (= (inc sj/max-journal-bytes) (.length r)))
      (is (= "first" (head r 5))))))

(deftest rotation-never-deletes-or-replaces
  (let [path (temp-path)
        f    (io/file path)
        now  (java.time.Instant/parse "2026-11-22T04:30:00Z")
        taken (sj/rotated-file f now 0)]
    (testing "a file already holding the rotation name is left alone"
      (io/make-parents taken)
      (spit taken "older history")
      (fill! path "second")
      (is (= (sj/rotated-file f now 1) (sj/rotate-if-full! f now)))
      (is (= "older history" (slurp taken))))
    (testing "rotating again in the same second takes the next free suffix"
      (fill! path "third")
      (is (= (sj/rotated-file f now 2) (sj/rotate-if-full! f now))))
    (is (= #{"older history" "second" "third"}
           (set (map #(str/replace (head % 13) "\u0000" "") (rotated-files path))))
        "every generation survives")
    (is (= ["config-set-journal.20261122T043000Z.ednl"
            "config-set-journal.20261122T043000Z-1.ednl"
            "config-set-journal.20261122T043000Z-2.ednl"]
           (mapv #(.getName (sj/rotated-file f now %)) [0 1 2])))
    (testing "a journal below the size is not rotated"
      (spit f "small")
      (is (nil? (sj/rotate-if-full! f now))))))

(deftest a-long-key-or-id-is-bounded
  (let [path (temp-path)
        big  (apply str (repeat (* 4096 sj/max-id-chars) "k"))
        ctx  (assoc (request/make-request-ctx {:caller-id big :session-id big})
                    :identity {:caller-id big :verified? true})]
    (request/with-request-context ctx
      (sj/append! path redact (assoc (attempt big "v")
                                     :agent-id big
                                     :identity (sj/request-identity))))
    (let [line (first (read-lines path))]
      (doseq [k [:key :agent-id :caller-id :verified-caller-id :session-id]]
        (is (str/ends-with? (get line k) (str "...[" (count big) " chars]")) (name k))
        (is (< (count (get line k)) (+ sj/max-id-chars 32)) (name k)))
      (is (< (count (slurp path)) (* 4 (+ sj/max-id-chars sj/max-value-chars)))
          "one line stays small however long the caller's strings are")))
  (is (nil? (sj/bound-str nil)))
  (is (= "short" (sj/bound-str "short"))))
