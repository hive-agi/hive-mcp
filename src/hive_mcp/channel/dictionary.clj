(ns hive-mcp.channel.dictionary
  "Lossless dictionary encoding for repetitive tool output.

   `compress` returns either an encoded map or the input string unchanged,
   whichever serializes smaller. `decode` accepts both, so
   `(decode (compress s))` is `s` for every string, byte for byte.

   Repetition is the only source of savings here, which is why this is aimed
   at test runs, carto dumps and build logs and never at memory entries.

   Pure, deterministic, no IO. Plan: memory 20260916002132-51330a59, step-6."
  (:require [clojure.string :as str]
            [malli.core :as m]))

(def ^:const marker
  "Sentinel bounding a meta-token. A source containing it is left unencoded."
  "")

(def ^:private est-token-chars 5)

(def ^:private est-entry-overhead 6)

(def Dictionary
  "Meta-token -> the line, word or alphanumeric run it stands for."
  [:map-of :string :string])

(def Encoded
  "An encoded payload: its dictionary and its tokenised text."
  [:map {:closed true}
   [:d Dictionary]
   [:t :string]])

(def Compressed
  "What `compress` returns: an encoded payload, or the original string."
  [:or Encoded :string])

(defn- token
  [n]
  (str marker n marker))

(defn- lines-of
  [s]
  (str/split s #"\n" -1))

(defn- worth-encoding
  "Units of `units` that repay a dictionary entry, most profitable first."
  [units]
  (->> (frequencies units)
       (keep (fn [[unit freq]]
               (let [gain (- (* (dec freq) (count unit))
                             (* (inc freq) est-token-chars)
                             est-entry-overhead)]
                 (when (and (> freq 1) (pos? gain))
                   [unit gain]))))
       (sort-by (comp - second))
       (mapv first)))

(def ^:private word-re
  #"\d+|\s+|\S+")

(def ^:private subword-re
  #"\d+|\s+|[A-Za-z0-9]+|[^\sA-Za-z0-9]+")

(defn- pass
  "One dictionary pass over `text`, splitting on `split-re` and numbering new
   tokens from `start`. Returns [text' unit->token]. `split-re` matches a
   token before anything else, so an earlier pass's output is never split."
  [text split-re start]
  (let [parts (vec (re-seq split-re text))
        units (worth-encoding
               (remove #(or (str/blank? %) (str/includes? % marker)) parts))
        unit->token (zipmap units (map #(token (+ start %)) (range)))]
    [(apply str (map #(unit->token % %) parts)) unit->token]))

(defn encode
  "Encode `s` into a dictionary and tokenised text in three coarse-to-fine
   passes: whole repeated lines, then repeated whitespace-delimited words,
   then repeated alphanumeric runs inside what is left. Returns the identity
   encoding when `s` contains `marker` or holds nothing worth encoding."
  [s]
  (let [src (str s)]
    (if (str/includes? src marker)
      {:d {} :t src}
      (let [lines (lines-of src)
            line->token (zipmap (worth-encoding lines) (map token (range)))
            after-lines (str/join "\n" (map #(line->token % %) lines))
            [after-words word->token] (pass after-lines word-re
                                            (count line->token))
            [text subword->token] (pass after-words subword-re
                                        (+ (count line->token)
                                           (count word->token)))]
        {:d (into {}
                  (map (fn [[unit tok]] [tok unit]))
                  (concat line->token word->token subword->token))
         :t text}))))

(def ^:private token-re
  #"\d+")

(defn decode
  "Reconstruct the source of `x`, which may be an encoded payload or an
   already-plain string. One pass: a dictionary value never holds a token."
  [x]
  (if (string? x)
    x
    (let [{:keys [d t]} x
          text (str t)]
      (if (empty? d)
        text
        (str/replace text token-re #(get d % %))))))

(defn compress
  "Encode `s` when that serializes smaller than `s` itself, else return `s`."
  [s]
  (let [src (str s)
        encoded (encode src)]
    (if (< (count (pr-str encoded)) (count (pr-str src)))
      encoded
      src)))

(defn compressed?
  "True when `x` is an encoded payload rather than a plain string."
  [x]
  (map? x))

(defn ratio
  "Fraction of the serialized size `compress` removes from `s`. 0.0 when it
   declines to encode."
  [s]
  (let [before (count (pr-str (str s)))
        after (count (pr-str (compress s)))]
    (if (zero? before)
      0.0
      (max 0.0 (- 1.0 (/ (double after) before))))))

(m/=> encode [:=> [:cat :any] Encoded])
(m/=> decode [:=> [:cat Compressed] :string])
(m/=> compress [:=> [:cat :any] Compressed])
(m/=> compressed? [:=> [:cat :any] :boolean])
(m/=> ratio [:=> [:cat :any] [:double {:min 0.0 :max 1.0}]])
