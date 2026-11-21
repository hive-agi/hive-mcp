(ns hive-mcp.crystal.history
  "`session history` (SYNTHESIS-HISTORY slice (c)): walk the per-project chain
   of wrap summaries newest-first, from the latest summary of a project along
   the :supersedes edges slice (a) writes (new :supersedes old).

     Collect   ISummaryHistory/-summaries        summaries stored for a project
               ISummaryHistory/-superseded-by    ids an entry supersedes (KG read)
               ISummaryHistory/-entry            one entry by id
     Promote   latest-summary, walk-chain,       pure choices: the head, the bounded
               digest, clamp-limit               walk, the one-line digest
     Pipeline  history                           the walk over a port -> History
     Render    render                            compact text for a human

   The walk is bounded twice: by `limit` (clamped to [1, max-limit]) and by a
   visited set, so a cycle in the KG (which has no uniqueness or acyclicity
   constraint on edges) ends the walk instead of looping."
  (:require [clojure.string :as str]
            [hive-mcp.crystal.chain :as chain]
            [hive-mcp.crystal.consolidate :as consolidate]
            [hive-mcp.protocols.memory :as mem-proto]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Domain
;; =============================================================================

(def default-limit
  "Summaries returned when the caller names no limit."
  10)

(def max-limit
  "Hard ceiling on one walk, whatever the caller asks for."
  100)

(def Stop
  "Why a walk ended. :end no older summary, :limit the bound was reached with
   an older one still linked, :cycle every predecessor was already visited,
   :empty the project has no summary, :error a read failed."
  [:enum :end :limit :cycle :empty :error])

(def Walk
  [:map
   [:ids [:vector :string]]
   [:stop Stop]
   [:error {:optional true} :string]])

(def HistoryEntry
  [:map
   [:id :string]
   [:date [:maybe :string]]
   [:digest [:maybe :string]]
   [:missing {:optional true} :boolean]])

(def History
  [:map
   [:project-id :string]
   [:head [:maybe :string]]
   [:limit [:int {:min 1}]]
   [:count [:int {:min 0}]]
   [:stop Stop]
   [:entries [:vector HistoryEntry]]
   [:error {:optional true} :string]])

;; =============================================================================
;; Port
;; =============================================================================

(defprotocol ISummaryHistory
  (-summaries [this project-id]
    "Session-summary entries stored for project-id, in any order.")
  (-superseded-by [this id]
    "Ids of the entries `id` has a :supersedes edge to, in any order.")
  (-entry [this id]
    "The stored entry for id, or nil."))

;; =============================================================================
;; Promote (pure)
;; =============================================================================

(defn clamp-limit
  "The walk bound for a caller-supplied LIMIT: `default-limit` when absent or
   unreadable, else LIMIT clamped to [1, max-limit]. A numeric string is read."
  [limit]
  (let [n (cond
            (integer? limit) limit
            (number? limit)  (long limit)
            (string? limit)  (parse-long (str/trim limit))
            :else            nil)]
    (if (nil? n)
      default-limit
      (-> n (max 1) (min max-limit)))))

(m/=> clamp-limit [:=> [:cat :any] [:int {:min 1 :max 100}]])

(defn latest-summary
  "Id of the newest session summary of PROJECT-ID among CANDIDATES (greatest
   id: ids are yyyyMMddHHmmss-prefixed), or nil. A store is not ordered, so
   the max is taken here."
  [project-id candidates]
  (reduce (fn [best {:keys [id] :as c}]
            (if (and (string? id)
                     (not (str/blank? id))
                     (chain/summary? c)
                     (chain/in-scope? project-id c)
                     (or (nil? best) (pos? (compare id best))))
              id
              best))
          nil
          candidates))

(m/=> latest-summary [:=> [:cat :string [:sequential chain/Candidate]] [:maybe :string]])

(defn walk-chain
  "Walk from START along NEXT-OF (id -> ids it supersedes), newest-first, at
   most LIMIT ids. Of several predecessors the greatest unvisited id is taken.
   Never throws: a NEXT-OF failure ends the walk with :stop :error.

   Bounded by LIMIT and by the visited set: when every predecessor of the
   current id was already visited the walk stops with :cycle."
  [next-of start limit]
  (if-not (string? start)
    {:ids [] :stop :empty}
    (loop [ids [start] visited #{start} cur start]
      (let [r (try {:nexts (->> (next-of cur) (filter string?) distinct (sort #(compare %2 %1)))}
                   (catch Throwable t {:error (or (ex-message t) (str (class t)))}))]
        (if-let [e (:error r)]
          {:ids ids :stop :error :error e}
          (let [nexts (:nexts r)
                pick  (first (remove visited nexts))]
            (cond
              (empty? nexts)         {:ids ids :stop :end}
              (nil? pick)            {:ids ids :stop :cycle}
              (>= (count ids) limit) {:ids ids :stop :limit}
              :else (recur (conj ids pick) (conj visited pick) pick))))))))

(m/=> walk-chain [:=> [:cat ifn? [:maybe :string] [:int {:min 1}]] Walk])

(def ^:private digest-width 120)

(defn- noise-line?
  "A line that is structure, not content: a markdown header, a bold-only
   header line, a rule."
  [l]
  (boolean
   (or (str/starts-with? l "#")
       (re-matches #"\*\*[^*]+\*\*:?" l)
       (re-matches #"[-*_=]{3,}" l))))

(defn digest
  "One line summarising a wrap's CONTENT: the first line that is not a header,
   with list markers and bold markers dropped and whitespace collapsed, cut to
   120 chars. nil when CONTENT holds no such line."
  [content]
  (when (string? content)
    (when-let [line (->> (str/split-lines content)
                         (map str/trim)
                         (remove str/blank?)
                         (remove noise-line?)
                         first)]
      (let [clean (-> line
                      (str/replace #"^(?:[-*+]|\d+\.)\s+" "")
                      (str/replace "**" "")
                      (str/replace #"\s+" " ")
                      str/trim)]
        (when-not (str/blank? clean)
          (if (> (count clean) digest-width)
            (str (subs clean 0 (dec digest-width)) "…")
            clean))))))

(m/=> digest [:=> [:cat :any] [:maybe :string]])

;; =============================================================================
;; Pipeline
;; =============================================================================

(defn- supersedes-targets [port id]
  (vec (-superseded-by port id)))

(defn- entry-row
  "The HistoryEntry for ID. A summary that is gone from the store (expired,
   deleted) or unreadable is kept in the chain as `:missing true`: its edges
   outlive it, and dropping it would hide a gap."
  [port id]
  (let [e (try (-entry port id) (catch Throwable _unreadable nil))]
    (if (map? e)
      {:id id
       :date (consolidate/day-of (assoc e :id id))
       :digest (digest (:content e))}
      {:id id
       :date (consolidate/day-of {:id id})
       :digest nil
       :missing true})))

(defn history
  "The newest-first chain of wrap summaries of PROJECT-ID through PORT, at most
   LIMIT entries (clamped by `clamp-limit`). Returns a History; never throws.

   The head is the latest summary stored for the project; each next entry is
   the one the previous :supersedes."
  [port project-id limit]
  (let [limit (clamp-limit limit)
        base  {:project-id project-id :limit limit}]
    (try
      (let [head (latest-summary project-id (vec (-summaries port project-id)))
            {:keys [ids stop error]} (walk-chain (partial supersedes-targets port) head limit)
            entries (mapv (partial entry-row port) ids)]
        (cond-> (assoc base :head head :count (count entries) :stop stop :entries entries)
          error (assoc :error error)))
      (catch Throwable t
        (assoc base :head nil :count 0 :stop :error :entries []
               :error (or (ex-message t) (str (class t))))))))

(m/=> history [:=> [:cat :any :string :any] History])

;; =============================================================================
;; Render
;; =============================================================================

(defn- stop-note [{:keys [stop limit error]}]
  (case stop
    :end   "end of chain"
    :limit (str "limit " limit "; older summaries remain")
    :cycle "cycle guard: the next summary was already listed"
    :error (str "read error: " error)
    :empty "no summary"))

(defn render
  "Compact text for a History: one header line, then one line per summary,
   `<date>  <id>  <digest>`, newest first."
  [{:keys [project-id count entries stop] :as h}]
  (if (and (= :empty stop) (empty? entries))
    (str "session history " project-id ": no wrap summary stored")
    (str/join
     "\n"
     (cons (str "session history " project-id ": " count
                (if (= 1 count) " summary" " summaries")
                ", newest first (" (stop-note h) ")")
           (for [{:keys [id date digest missing]} entries]
             (str (or date "????-??-??") "  " id "  "
                  (cond missing "(missing from store)"
                        digest  digest
                        :else   "(no text)")))))))

(defn report
  "What `session history` answers: the History data plus its `render` under
   :text, so a caller gets both the shape and the compact view in one call."
  [port project-id limit]
  (let [h (history port project-id limit)]
    (assoc h :text (render h))))

(m/=> report [:=> [:cat :any :string :any] (into History [[:text :string]])])

(m/=> render [:=> [:cat History] :string])

;; =============================================================================
;; Boundary: the store + KG adapter
;; =============================================================================

(def ^:private summaries-limit 500)

(defn- supersedes-edge? [edge]
  (let [r (:kg-edge/relation edge)]
    (= "supersedes" (cond (keyword? r) (name r) (string? r) r :else nil))))

(defrecord StoreHistory [store edges-from]
  ISummaryHistory
  (-summaries [_ project-id]
    (mem-proto/query-entries store {:type          "note"
                                    :tags          [chain/summary-tag]
                                    :project-id    project-id
                                    :output-fields ["id" "project-id" "tags"]
                                    :order-by      [:created :desc]
                                    :limit         summaries-limit}))
  (-superseded-by [_ id]
    (->> (edges-from id)
         (filter supersedes-edge?)
         (keep :kg-edge/to)))
  (-entry [_ id]
    (mem-proto/get-entry store id)))

(defn store-history
  "The ISummaryHistory over the registered `store-key` memory store and the KG
   edge reader, or nil when either is absent."
  ([] (store-history :default))
  ([store-key]
   (when (mem-proto/store-set?)
     (when-let [edges-from (try (requiring-resolve 'hive-mcp.knowledge-graph.edges.queries/get-edges-from)
                                (catch Throwable _kg-absent-means-no-history nil))]
       (when-let [store (mem-proto/get-store store-key)]
         (->StoreHistory store edges-from))))))

(m/=> store-history [:function
                     [:=> [:cat] [:maybe [:fn #(satisfies? ISummaryHistory %)]]]
                     [:=> [:cat :keyword] [:maybe [:fn #(satisfies? ISummaryHistory %)]]]])
