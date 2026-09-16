(ns hive-mcp.channel.axiom-projection-test
  "Contract tests for floor-lane compaction.

   The governing assertion is BYTE-IDENTITY of what survives. An axiom binds
   word for word, so a test that merely checks the output is 'about right' is
   worthless here: the only acceptable evidence is that every retained line
   appears in the output unchanged, character for character.

   The second assertion is the direction of the default. An unrecognised
   heading must be KEPT. The corpus has 124 bespoke headings and many are
   normative, so a classifier that guesses wrong must lose savings, never
   binding text."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.channel.axiom-projection :as ax]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def sample
  (str "# AXIOM: something inviolable\n"
       "\n"
       "Opening statement that must never be dropped.\n"
       "\n"
       "## INVIOLABLE RULE\n"
       "\n"
       "Never do the forbidden thing.\n"
       "\n"
       "## Why\n"
       "\n"
       "A long story about the incident that taught us this.\n"
       "More of the story.\n"
       "\n"
       "## How to apply\n"
       "\n"
       "Check the thing before acting.\n"))

(deftest the-rule-survives-and-the-story-does-not
  (let [out (ax/compact sample "id-1")]
    (testing "normative text is byte-identical"
      (is (str/includes? out "Never do the forbidden thing."))
      (is (str/includes? out "Check the thing before acting."))
      (is (str/includes? out "Opening statement that must never be dropped.")))
    (testing "the rationale body is gone but its heading remains"
      (is (not (str/includes? out "A long story about the incident")))
      (is (str/includes? out "## Why") "the heading is itself information"))
    (testing "the pointer says how to recover it"
      (is (str/includes? out "memory get id-1")))))

(deftest an-unrecognised-heading-is-kept
  (testing "keep-by-default: a bespoke heading must not be withheld"
    (doseq [h ["## Antipatterns: violations of the canon"
               "## Strict definitions, use the litmus column"
               "## The directive alone is NOT SUFFICIENT"
               "## Corollary"
               "## The tell"
               "## Enforcement"]]
      (is (not (ax/withhold? {:header h :body ["binding text"]}))
          (str "withheld a heading that may carry a rule: " h)))))

(deftest positively-recognised-rationale-is-withheld
  (doseq [h ["## Why" "## Why it bites" "## Why nobody saw it"
             "## Examples" "## Counterexamples" "## The measurement"
             "## Incident" "## Provenance" "## References"]]
    (is (ax/withhold? {:header h :body ["story"]})
        (str "should have been recognised as rationale: " h))))

(deftest a-headerless-section-is-never-withheld
  (is (not (ax/withhold? {:header nil :body ["# Title" "opening claim"]}))))

(deftest the-explicit-marker-overrides-an-unrecognised-heading
  (testing "the corpus can declare its own intent where a heading cannot"
    (is (ax/withhold? {:header "## Some bespoke heading"
                       :body [ax/rationale-marker "story here"]}))
    (is (not (ax/withhold? {:header "## Some bespoke heading"
                            :body ["ordinary text"]})))))

(deftest guard-blocks-become-pointers-that-name-the-rule
  (let [c (str "## Enforcement\n\n```guard-rule\n"
               "{:id :guard/no-test-run-on-live-nrepl :msg \"...\"}\n"
               "(lots more source)\n```\n")
        out (ax/strip-guard-blocks c "id-9")]
    (is (not (str/includes? out "lots more source")))
    (is (str/includes? out ":guard/no-test-run-on-live-nrepl")
        "the reader must still be able to name the rule that fires")
    (is (str/includes? out "hive-spi.guard"))
    (is (str/includes? out "memory get id-9"))
    (is (str/includes? out "## Enforcement") "the section itself is normative and stays")))

(deftest an-axiom-with-no-rationale-is-returned-unchanged
  (let [c "# AXIOM: terse\n\n## The rule\n\nDo the thing.\n"]
    (is (= c (:C (ax/compact-entry {:id "x" :T "axiom" :C c})))
        "nothing to withhold, so nothing may change")
    (is (nil? (:compacted (ax/compact-entry {:id "x" :T "axiom" :C c}))))))

(deftest compaction-never-grows-an-entry
  (let [tiny {:id "t" :T "axiom" :C "## Why\n\nx\n"}]
    (is (<= (count (str (:C (ax/compact-entry tiny))))
            (count (str (:C tiny)))))))

(def gen-line
  "A body line that is never mistaken for a markdown heading."
  (gen/such-that #(not (re-find #"^#{1,4}\s" %))
                 (gen/not-empty gen/string-alphanumeric)))

(deftest llmlingua-marks-the-rule-as-uncompressible-and-the-story-as-cheap
  (let [c (str "# AXIOM: terse\n\n"
               "## The rule\n\nDo the thing.\n\n"
               "## Why\n\nBecause of a long story.\n")
        out (ax/llmlingua c)]
    (is (str/includes? out "<llmlingua, compress=False>\n## The rule")
        "the binding section refuses compression outright")
    (is (str/includes? out "<llmlingua, rate=0.4>\n## Why")
        "the rationale is offered a rate instead")
    (is (= c (ax/strip-llmlingua out)) "tagging is exactly reversible")))

(deftest the-headerless-preamble-is-never-marked-compressible
  (let [c "# AXIOM: terse\n\nThe opening statement.\n\n## Why\n\nStory.\n"
        out (ax/llmlingua c)]
    (is (str/starts-with? out "<llmlingua, compress=False>\n# AXIOM: terse"))
    (is (= c (ax/strip-llmlingua out)))))

(deftest llmlingua-and-compact-agree-on-what-is-rationale
  (let [c (str "# AXIOM: t\n\n## The rule\n\nKeep.\n\n"
               "## Why\n\nDrop.\n\n## Examples\n\nAlso drop.\n")
        tagged (ax/llmlingua c)
        rated (count (re-seq #"<llmlingua, rate=" tagged))
        compacted (ax/compact c "id")]
    (is (= 2 rated) "Why and Examples, the same two compact withholds")
    (is (not (str/includes? compacted "Drop.")))
    (is (str/includes? tagged "Drop.")
        "tagging DELEGATES the decision, it does not take it")))

(deftest a-custom-rate-is-honoured
  (is (str/includes? (ax/llmlingua "# t\n\n## Why\n\nx\n" 0.15)
                     "<llmlingua, rate=0.15>")))

(defspec tagging-is-exactly-reversible 200
  (prop/for-all [title (gen/not-empty gen/string-alphanumeric)
                 rule-lines (gen/vector gen-line 1 5)
                 why-lines (gen/vector gen-line 1 5)
                 trailing gen/boolean]
    (let [c (str "# " title "\n\n## The rule\n\n"
                 (str/join "\n" rule-lines)
                 "\n\n## Why\n\n" (str/join "\n" why-lines)
                 (when trailing "\n"))]
      (= c (ax/strip-llmlingua (ax/llmlingua c))))))

(defspec every-source-line-survives-tagging-byte-identical 200
  (prop/for-all [title (gen/not-empty gen/string-alphanumeric)
                 body (gen/vector gen-line 1 8)]
    (let [c (str "# " title "\n\n## The rule\n\n" (str/join "\n" body) "\n")
          out (ax/llmlingua c)]
      (every? #(str/includes? out %) body))))

(defspec compaction-is-exact-identity-when-nothing-is-withheld 200
  (prop/for-all [title (gen/not-empty gen/string-alphanumeric)
                 body (gen/vector gen-line 1 8)
                 trailing gen/boolean]
    (let [c (str "# " title "\n\n## The rule\n\n"
                 (str/join "\n" body)
                 (when trailing "\n"))]
      (= c (ax/compact c "id")))))

(defspec every-kept-line-survives-byte-identical 200
  (prop/for-all [title (gen/not-empty gen/string-alphanumeric)
                 rule-lines (gen/vector gen-line 1 6)
                 why-lines (gen/vector gen-line 1 6)]
    (let [c (str "# " title "\n\n## The rule\n\n"
                 (str/join "\n" rule-lines)
                 "\n\n## Why\n\n" (str/join "\n" why-lines) "\n")
          out (ax/compact c "id")]
      (and (every? #(str/includes? out %) rule-lines)
           (str/includes? out title)
           (str/includes? out "## The rule")))))

(defspec sections-partition-every-line 200
  (prop/for-all [ls (gen/vector (gen/one-of [gen-line (gen/return "## A heading")]) 0 25)]
    (let [c (str/join "\n" ls)
          secs (ax/sections c)
          back (mapcat (fn [s] (if (:header s) (cons (:header s) (:body s)) (:body s))) secs)]
      (= (if (str/blank? c) [""] ls) (vec back)))))
