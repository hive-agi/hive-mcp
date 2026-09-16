(ns hive-mcp.channel.dictionary-test
  "Contract tests for lossless dictionary encoding.

   The governing assertion is EXACT round-trip. A compression that is merely
   close is a different payload, so every property here compares bytes and
   none compares similarity."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.channel.dictionary :as dict]))

(def ^:private a-test-run
  (str/join "\n"
            (concat ["Running tests in #{test}"]
                    (repeat 40 "lein test hive-mcp.channel.drain-projection-test")
                    (repeat 40 "Ran 1 tests containing 3 assertions.")
                    ["0 failures, 0 errors."])))

(def ^:private gen-line
  (gen/not-empty gen/string-alphanumeric))

(def ^:private gen-log
  (gen/let [pool (gen/vector gen-line 1 5)
            n (gen/choose 1 40)
            idxs (gen/vector (gen/choose 0 (dec (count pool))) n)
            trailing gen/boolean]
    (str (str/join "\n" (map pool idxs))
         (when trailing "\n"))))

(deftest a-repetitive-dump-shrinks-and-round-trips
  (let [out (dict/compress a-test-run)]
    (is (dict/compressed? out))
    (is (= a-test-run (dict/decode out)) "byte-identical round trip")
    (is (< 0.5 (dict/ratio a-test-run))
        (str "ratio was " (dict/ratio a-test-run)))))

(deftest text-with-no-repetition-is-returned-unchanged
  (let [s "one\ntwo\nthree\n"
        out (dict/compress s)]
    (is (not (dict/compressed? out)))
    (is (identical? s out))))

(deftest a-source-containing-the-marker-is-left-alone
  (let [s (str "a" dict/marker "b\n" "a" dict/marker "b\n")]
    (is (= {} (:d (dict/encode s))))
    (is (= s (dict/decode (dict/encode s))))
    (is (= s (dict/decode (dict/compress s))))))

(deftest decode-is-total-over-plain-strings
  (is (= "" (dict/decode "")))
  (is (= "anything" (dict/decode "anything")))
  (is (= "x\n" (dict/decode {:d {} :t "x\n"}))))

(deftest whitespace-at-the-edges-survives
  (doseq [s ["" "\n" "\n\n\n"
             "a\n" "a" "\na"
             "dup\ndup\n" "dup\ndup"
             "crlf\r\ncrlf\r\n"
             "tail  \ntail  \n"]]
    (is (= s (dict/decode (dict/compress s))) (pr-str s))))

(deftest the-dictionary-only-holds-lines-that-repay-themselves
  (let [s (str/join "\n" (concat (repeat 20 "0123456789012345678901234567890123456789")
                                 (repeat 2 "ab")))
        {:keys [d]} (dict/encode s)]
    (is (= 1 (count d)) "the two-char line cannot repay a token")
    (is (= #{"0123456789012345678901234567890123456789"} (set (vals d))))))

(deftest short-repeated-units-are-declined-even-though-they-save-characters
  (testing "identifier-shaped repetition: char-positive, token-negative, so refused"
    (let [s (str/join "\n" (for [i (range 60)]
                             (str "hive-mcp.channel.drain-projection-test/case-" i)))
          out (dict/compress s)]
      (is (not (dict/compressed? out))
          "the repeated units here are 'channel' and 'projection', both under min-unit-chars")
      (is (= s (dict/decode out)))
      (is (zero? (dict/ratio s))))))

(deftest long-repeated-lines-are-still-encoded
  (testing "the log case: identical long lines recurring, as stack frames do across threads"
    (let [frame "\tat hive_mcp.channel.drain_projection$project_entry.invoke(drain_projection.clj:42)"
          s (str/join "\n" (mapcat (fn [i] [(str "Thread-" i " prio=5 tid=0x00007f runnable") frame frame])
                                   (range 30)))
          out (dict/compress s)]
      (is (dict/compressed? out))
      (is (= s (dict/decode out)))
      (is (< 0.2 (dict/ratio s)) (str "ratio was " (dict/ratio s))))))

(deftest a-repeated-multi-word-span-is-NOT-captured
  (testing "known gap: no pass detects a repeated span that is longer than a word and shorter than a line"
    (let [s (str/join "\n" (for [i (range 60)]
                             (str "at hive_mcp.channel.drain_projection$project_entry.invoke(f.clj:" i ")")))]
      (is (not (dict/compressed? (dict/compress s)))
          "the shared prefix spans several words, so lines differ and words differ")
      (is (= s (dict/decode (dict/compress s)))))))

(defspec round-trip-is-exact-for-log-shaped-text 300
  (prop/for-all [s gen-log]
    (= s (dict/decode (dict/compress s)))))

(defspec round-trip-is-exact-for-arbitrary-text 300
  (prop/for-all [s gen/string]
    (= s (dict/decode (dict/compress s)))))

(defspec compression-never-grows-the-payload 300
  (prop/for-all [s gen-log]
    (<= (count (pr-str (dict/compress s)))
        (count (pr-str s)))))

(defspec compress-is-idempotent-on-its-own-output 200
  (prop/for-all [s gen-log]
    (let [once (dict/compress s)]
      (= s (dict/decode (dict/compress (dict/decode once)))))))

(defspec a-ratio-is-a-fraction 200
  (prop/for-all [s gen-log]
    (let [r (dict/ratio s)]
      (and (<= 0.0 r) (< r 1.0)))))
