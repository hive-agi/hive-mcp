(ns hive-mcp.channel.axiom-projection
  "Compaction for the FLOOR lane, where the entry may not be summarised.

   An axiom is delivered word for word. That constrains how its text may be
   rewritten; it does not require shipping every byte of the entry that
   surrounds the rule. Two things travel today that need not:

   1. RATIONALE. The `## Why`, the incident, the measurement. Binding force
      lives in the rule, not in the story of how it was learned.
   2. GUARD SOURCE. A fenced ```guard-rule``` block is the SOURCE of a rule
      that `hive-spi.guard` fires on tool call one whether or not the text ever
      reached the agent. The mechanism is the enforcement; the listing is a
      copy of it.

   Both are replaced by a POINTER naming what was withheld and how to pull it,
   so the agent knows the section exists and can fetch it.

   ## The safety rule that shapes this namespace

   Retained text is BYTE-IDENTICAL. Nothing is reflowed, re-headed, summarised
   or re-encoded. Only whole sections and whole fenced blocks are removed, each
   replaced by one pointer line. A section is withheld only when it is
   POSITIVELY recognised as rationale; an unrecognised header is KEPT. The
   corpus has 124 distinct bespoke headers, many of them normative in
   substance (\"antipatterns\", \"strict definitions\", \"the directive alone is
   not sufficient\"), so keep-by-default is the only defensible default: a
   misjudgement then costs savings rather than binding text.

   Pure, deterministic, no IO."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:const rationale-marker
  "An explicit opt-in marker an author puts on the first line of a section to
   declare it rationale, whatever its heading says.

   This is the migration path to the savings a heading whitelist cannot reach:
   the corpus states its own intent instead of a classifier guessing it."
  "<!-- rationale -->")

(def explanatory-words
  "Words that mark a heading as narrative rather than normative.

   Deliberately small. Every addition widens what may be withheld, so a word
   earns its place only when a heading containing it could not plausibly carry
   a rule."
  #{"example" "examples" "counterexample" "counterexamples"
    "incident" "postmortem" "timeline" "provenance"
    "measurement" "measured" "crossrefs" "references" "reference"
    "background" "history"})

(defn normalise-header
  "HEADER reduced to lowercase words, for matching only. Never used to rewrite
   the heading that is emitted."
  [header]
  (-> (str header)
      (str/replace #"^#{1,4}\s*" "")
      str/lower-case
      (str/replace #"[^a-z ]" " ")
      (str/replace #"\s+" " ")
      str/trim))

(defn explanatory-header?
  "True when HEADER positively reads as rationale.

   `why...` is the one prefix rule: a section that opens by asking why is
   explaining, not binding. Everything else must hit `explanatory-words`."
  [header]
  (let [h (normalise-header header)]
    (boolean
     (or (str/starts-with? h "why")
         (some explanatory-words (str/split h #" "))))))

(defn sections
  "CONTENT split into [{:header <line or nil> :body [lines]}].

   The leading run before the first heading is a section with a nil header and
   is never withheld: it carries the title and the opening statement."
  [content]
  (let [lines (vec (str/split-lines (str content)))
        idx (vec (keep-indexed (fn [i l] (when (re-find #"^#{1,4}\s" l) i)) lines))]
    (if (empty? idx)
      [{:header nil :body lines}]
      (into (if (zero? (first idx))
              []
              [{:header nil :body (subvec lines 0 (first idx))}])
            (for [[i j] (partition 2 1 (concat idx [(count lines)]))]
              {:header (nth lines i) :body (subvec lines (inc i) j)})))))

(defn withhold?
  "True when SECTION is rationale: either explicitly marked, or positively
   recognised by its heading. A headerless section is never withheld."
  [{:keys [header body]}]
  (boolean
   (and header
        (or (some #(str/includes? (str %) rationale-marker) (take 2 body))
            (explanatory-header? header)))))

(def ^:const llmlingua-rate
  "Rate offered for a rationale span. Normative spans are never given a rate;
   they are marked compress=False, which is a refusal, not a low number."
  0.4)

(def ^:const llmlingua-close "</llmlingua>")

(def ^:private llmlingua-tag-re #"</?llmlingua(,[^>]*)?>")

(defn- llmlingua-open
  [rationale? rate]
  (if rationale?
    (str "<llmlingua, rate=" rate ">")
    "<llmlingua, compress=False>"))

(defn llmlingua
  "CONTENT annotated with LLMLingua-2 per-segment control tags.

   Normative spans are wrapped `<llmlingua, compress=False>` and rationale
   spans `<llmlingua, rate=N>`, using the same split `compact` withholds by.
   Every source line survives in order and unchanged; only tag lines are
   added, so `strip-llmlingua` is an exact inverse.

   This hands an external compressor the safety decision already made here,
   rather than letting it rediscover which spans are binding."
  ([content] (llmlingua content llmlingua-rate))
  ([content rate]
   (let [src (str content)
         out (->> (sections src)
                  (mapcat (fn [{:keys [header body] :as section}]
                            (concat [(llmlingua-open (withhold? section) rate)]
                                    (when header [header])
                                    body
                                    [llmlingua-close])))
                  (str/join "\n"))]
     (if (str/ends-with? src "\n")
       (str out "\n")
       out))))

(defn strip-llmlingua
  "Inverse of `llmlingua`: drop the control-tag lines and return the source."
  [tagged]
  (let [src (str tagged)
        out (->> (str/split-lines src)
                 (remove #(re-matches llmlingua-tag-re %))
                 (str/join "\n"))]
    (if (str/ends-with? src "\n")
      (str out "\n")
      out)))

(defn strip-guard-blocks
  "TEXT with every fenced ```guard-rule``` block replaced by a one-line pointer.

   The guard id inside the block is carried into the pointer when one is
   present, so the reader can still name the rule that will fire."
  [text id]
  (str/replace (str text) #"(?s)```guard-rule.*?```"
               (fn [block]
                 (let [gid (second (re-find #"(:guard/[A-Za-z0-9._-]+)" block))]
                   (str "[guard-rule" (when gid (str " " gid))
                        " enforced by hive-spi.guard on tool call one;"
                        " source: memory get " id "]")))))

(defn compact
  "CONTENT with rationale sections and guard sources replaced by pointers.

   Every retained line is emitted unchanged. A withheld section keeps its
   HEADING, because the heading is often itself a claim, and loses only its
   body.

   The trailing newline is preserved. `split-lines` discards it, so rejoining
   would otherwise shorten content that had nothing withheld at all, which both
   breaks byte-identity and makes a no-op look like a saving."
  [content id]
  (let [src (str content)
        out (->> (sections src)
                 (map (fn [{:keys [header body] :as sec}]
                        (let [lines (if header (cons header body) body)]
                          (if (withhold? sec)
                            [header (str "[rationale withheld, " (count body)
                                         " lines; pull with: memory get " id "]")]
                            lines))))
                 (apply concat)
                 (str/join "\n")
                 (#(strip-guard-blocks % id)))]
    (if (and (str/ends-with? src "\n") (not (str/ends-with? out "\n")))
      (str out "\n")
      out)))

(defn compact-entry
  "ENTRY with its `:C` compacted.

   Idempotent by flag: an entry already carrying `:compacted` is returned as
   is. This matters because the drain writes the projected buffer back, so
   compaction runs again on every later drain; without the flag a withheld
   section would be re-withheld and its pointer line rewritten with a new line
   count, which would make the text drift on each pass.

   Returns ENTRY unchanged when compaction would not make it smaller, so the
   pointer can never cost more than the text it replaced."
  [entry]
  (if (:compacted entry)
    entry
    (let [c (str (:C entry))
          out (compact c (:id entry))]
      (if (< (count out) (count c))
        (assoc entry :C out :compacted true)
        entry))))
