(ns hive-mcp.tools.catchup.budget
  "Byte budget for the inline part of a catchup answer.

   A catchup answer is a vector of MCP text blocks, one JSON object per
   block. Every coordinator recycle pays for the whole answer in its
   context, so the inline part is kept under a byte budget:

   - Pinned blocks (header, axiom-review, kanban, forge-inbox) always stay
     inline and whole: they are what an agent needs to resume.
   - Every other block is sheddable. While the answer is over budget, the
     next sheddable block (bulkiest and least needed first, see
     `shed-rank`) is moved to the context store and replaced inline by a
     stub: its name, its byte size, a ref, the call that fetches it, when
     the ref expires, the fallback for a dead ref, and a short summary
     (counts, ids, short scalars, the hint).
   - If the stubs alone still overflow, their summaries are dropped in the
     same order. The refs always stay.
   - When anything was shed, one small `inline-budget` block after the
     header says what was shed and how to fetch it.

   Axioms, principles and priority conventions are not in the answer at
   all: they drain through the ---MEMORY--- piggyback, which this budget
   does not touch."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.dns.result :refer [rescue]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-inline-budget
  "Default UTF-8 byte budget for the inline part of a catchup answer."
  9000)

(def config-key
  "Config key (config get/set) that overrides `default-inline-budget`."
  "catchup.inline-budget-bytes")

(def pinned-blocks
  "Blocks that never leave the inline answer. forge-inbox lists the PRs and
   issues owing a maintainer reply, so it stays inline with the kanban."
  #{"header" "axiom-review" "kanban" "forge-inbox" "inline-budget"})

(def shed-rank
  "Shedding order for known blocks: lower sheds first. A block not named
   here ranks `unknown-rank`."
  {"kg-digest"     0
   "kg-insights"   1
   "context"       2
   "recent-wraps"  3
   "tool-friction" 5
   "carto-status"  6
   "meta"          7})

(def unknown-rank
  "Rank of a contributed block that `shed-rank` does not name."
  4)

(def ref-ttl-ms
  "How long a shed block stays fetchable from the context store (1 h,
   the same TTL catchup gives its context-refs)."
  3600000)

(defn utf8-bytes
  "UTF-8 byte length of `s`."
  [s]
  (alength (.getBytes (str s) java.nio.charset.StandardCharsets/UTF_8)))

(defn answer-bytes
  "Total UTF-8 bytes of the :text of every block."
  [blocks]
  (reduce + 0 (map (comp utf8-bytes :text) blocks)))

(defn- parse-block
  "The block's JSON object, or nil when its text is not one."
  [block]
  (rescue nil (let [v (json/read-str (str (:text block)))]
                (when (map? v) v))))

(defn block-name
  "The `_block` name of a text block, or nil."
  [block]
  (get (parse-block block) "_block"))

(def ^:private summary-string-cap 80)
(def ^:private summary-hint-cap 160)
(def ^:private summary-ids 5)

(def id-keys
  "Keys a list row's identifier is read from, first present wins: \"id\",
   else a forge \"ref\" (owner/repo#n), a \"number\", a carto \"qn\" or a
   \"name\"."
  ["id" "ref" "number" "qn" "name"])

(defn- cut [s cap]
  (if (<= (count s) cap) s (str (subs s 0 cap) " …")))

(defn- row-id
  "The identifier of a list row: the value of its first present `id-keys`."
  [row]
  (when (map? row)
    (some #(let [x (get row %)] (when (some? x) x)) id-keys)))

(defn summarize
  "A short, shape-preserving summary of a parsed JSON value: scalars and
   short strings stay, long strings are cut, sequences become their count
   (plus the first ids when their entries carry one of `id-keys`), maps
   recurse to `depth` and then become their key count. `_block` is dropped;
   a string `hint` is kept, cut to about 160 chars."
  ([v] (summarize v 2))
  ([v depth]
   (cond
     (or (nil? v) (number? v) (boolean? v)) v
     (string? v) (cut v summary-string-cap)
     (sequential? v) (let [ids (keep row-id v)]
                       (cond-> {"count" (count v)}
                         (seq ids) (assoc "ids" (vec (take summary-ids ids)))))
     (map? v) (if (pos? depth)
                (let [hint (get v "hint")]
                  (cond-> (into {} (map (fn [[k x]] [k (summarize x (dec depth))]))
                                (dissoc v "_block" "hint"))
                    (string? hint) (assoc "hint" (cut hint summary-hint-cap))))
                {"keys" (count v)})
     :else (str v))))

(defn fetch-call
  "The call that returns a shed block in full."
  [ref]
  (str "project command=\"context-get\" ctx_id=" ref))

(def fallback-call
  "What to do when a stub's ref is dead. The context store is in memory,
   evicts its oldest entry at 2000 entries and is wiped by a JVM restart, so
   `context-get` can answer {not-found true} before the TTL is up."
  (str "if context-get answers not-found, re-run "
       "project command=\"workflow catchup\" inline_budget=0"))

(defn expires-at
  "The ISO-8601 instant a ref stored at `now-ms` (epoch ms) expires:
   now + `ref-ttl-ms`."
  [now-ms]
  (str (java.time.Instant/ofEpochMilli (+ now-ms ref-ttl-ms))))

(defn- stub
  [name data ref bytes summary? expires]
  (cond-> {"_block"   name
           "shed"     true
           "bytes"    bytes
           "ref"      ref
           "fetch"    (fetch-call ref)
           "expires"  expires
           "fallback" fallback-call}
    summary? (assoc "summary" (summarize data))))

(defn- text-block [data] {:type "text" :text (json/write-str data)})

(defn- note-block
  [budget before after shed]
  (text-block
   {"_block"       "inline-budget"
    "budget"       budget
    "bytes-before" before
    "bytes-inline" after
    "over-budget"  (> after budget)
    "shed-blocks"  (mapv :name shed)
    "note"         (str "Blocks with shed:true were moved to the context store "
                        "(1h TTL); fetch one in full with its `fetch` call. "
                        "Axioms and priority conventions are unaffected: they "
                        "drain via ---MEMORY--- piggyback. inline_budget=0 "
                        "returns the full answer.")}))

(defn- with-note
  "Blocks of `items` with the note inserted after the header, and the
   note's own bytes counted in bytes-inline."
  [items budget before]
  (let [shed   (filterv :shed? items)
        blocks (mapv :block items)
        at     (or (first (keep-indexed (fn [i it] (when (= "header" (:name it)) (inc i))) items))
                   0)
        place  (fn [note] (vec (concat (subvec blocks 0 at) [note] (subvec blocks at))))
        base   (answer-bytes blocks)
        ;; Converge on a note whose bytes-inline counts the note itself.
        note   (loop [guess base n 0]
                 (let [note  (note-block budget before guess shed)
                       total (+ base (utf8-bytes (:text note)))]
                   (if (or (= total guess) (> n 3))
                     note
                     (recur total (inc n)))))]
    (place note)))

(defn fit-blocks
  "Keep the inline part of the catchup answer `blocks` under `budget`
   UTF-8 bytes. `put-ref!` is (fn [block-name data]) -> ref or nil; it
   stores a shed block's parsed JSON for later fetch. A block whose store
   fails stays inline. A budget of 0 or nil disables the trim (nil means
   `default-inline-budget`); an answer already under budget is returned
   unchanged. Pinned blocks are never shed, so a pinned part larger than
   the budget is answered whole and marked over-budget."
  [blocks {:keys [budget put-ref! now-ms]}]
  (let [budget (if (nil? budget) default-inline-budget budget)
        before (answer-bytes blocks)]
    (if (or (not (pos-int? budget)) (<= before budget))
      blocks
      (let [items (mapv (fn [b]
                          (let [d (parse-block b)]
                            {:block b :data d :name (get d "_block")
                             :bytes (utf8-bytes (:text b))}))
                        blocks)
            order (->> items
                       (keep-indexed (fn [i {:keys [name data]}]
                                       (when (and data name (not (pinned-blocks name)))
                                         [i (get shed-rank name unknown-rank)])))
                       (sort-by (fn [[i rank]] [rank i]))
                       (mapv first))
            ;; The note costs bytes too; reserve its size up front.
            reserve (utf8-bytes (:text (note-block budget before before
                                                   (mapv #(items %) order))))
            expires (expires-at (or now-ms (System/currentTimeMillis)))
            fits? (fn [its] (<= (+ (answer-bytes (map :block its)) reserve) budget))
            shed-one (fn [its i summary?]
                       (let [{:keys [name data bytes ref]} (its i)
                             ref (or ref (rescue nil (put-ref! name data)))]
                         (if (nil? ref)
                           its
                           (update its i assoc :shed? true :ref ref
                                   :block (text-block (stub name data ref bytes summary? expires))))))
            pass (fn [its summary?]
                   (reduce (fn [its i]
                             (if (fits? its)
                               (reduced its)
                               (if (or summary? (:shed? (its i)))
                                 (shed-one its i summary?)
                                 its)))
                           its order))
            shed (pass items true)
            shed (if (fits? shed) shed (pass shed false))]
        (if (some :shed? shed)
          (with-note shed budget before)
          blocks)))))

(defn- parse-budget
  [v]
  (cond
    (integer? v) v
    (string? v) (rescue nil (Long/parseLong (str/trim v)))
    :else nil))

(defn resolve-budget
  "The inline byte budget for a catchup call: the caller's `inline_budget`
   arg, else `config-value` (from `config-key`), else `default-inline-budget`.
   A negative or unparseable value is ignored. 0 disables the trim."
  [args config-value]
  (let [arg (some #(get args %) [:inline_budget :inline-budget "inline_budget"])
        ok  (fn [v] (let [n (parse-budget v)] (when (and n (>= n 0)) n)))]
    (or (ok arg) (ok config-value) default-inline-budget)))

;; =============================================================================
;; Contracts
;; =============================================================================

(def TextBlock
  [:map [:type [:= "text"]] [:text :string]])

(def FitOpts
  [:map
   [:budget {:optional true} [:maybe nat-int?]]
   [:put-ref! {:optional true} [:maybe ifn?]]
   [:now-ms {:optional true} [:maybe :int]]])

(m/=> utf8-bytes [:=> [:cat :any] nat-int?])
(m/=> answer-bytes [:=> [:cat [:sequential TextBlock]] nat-int?])
(m/=> block-name [:=> [:cat TextBlock] [:maybe :string]])
(m/=> summarize [:function
                 [:=> [:cat :any] :any]
                 [:=> [:cat :any :int] :any]])
(m/=> fetch-call [:=> [:cat :string] :string])
(m/=> expires-at [:=> [:cat :int] :string])
(m/=> fit-blocks [:=> [:cat [:sequential TextBlock] FitOpts] [:sequential TextBlock]])
(m/=> resolve-budget [:=> [:cat [:maybe :map] :any] nat-int?])
