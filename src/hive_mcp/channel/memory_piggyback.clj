(ns hive-mcp.channel.memory-piggyback
  "Memory piggyback channel for incremental delivery of axioms and conventions via cursor+budget drain."
  (:require [taoensso.timbre :as log]
            [hive-dsl.bounded-atom :refer [bounded-atom bput! bget bounded-swap!
                                           bclear! register-sweepable!]]
            [hive-dsl.context.identity :as ctx-id]
            [hive-spi.swarm.guards :as guards]
            [hive-mcp.channel.drain-rank :as rank]
            [hive-mcp.channel.drain-telemetry :as telemetry]
            [clojure.string :as str]
            [hive-mcp.channel.drain-projection :as proj]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const drain-char-budget
  "Max chars per drain batch. Kept modest so each tool response
   stays readable — full content absorbed over several calls."
  12000)

;; gc-fix-3: buffers — LRU eviction, 200 entries, 30min TTL
;; Piggyback buffers are ephemeral per-agent state. Orphaned buffers waste memory.
(defonce ^{:doc "Map of caller-id-key -> buffer state (session-scoped)."}
  buffers
  (bounded-atom {:max-entries 200
                 :ttl-ms 1800000    ;; 30 minutes
                 :eviction-policy :lru}))
(register-sweepable! buffers :piggyback-buffers)

(defn- format-entry
  "Convert a catchup entry to compact piggyback format.

   :A and :F carry the stored access count and the net helpful-minus-unhelpful
   feedback forward so `drain-rank/score` can read them without a store hit.
   Both are omitted when the source entry has neither."
  [entry]
  (let [access (:access-count entry)
        net (- (or (:helpful-count entry) 0) (or (:unhelpful-count entry) 0))]
    (cond-> {:id (:id entry)
             :T (or (:type entry) "note")
             :C (or (:content entry) (:preview entry) "")}
      (:severity entry) (assoc :S (:severity entry))
      (seq (:tags entry)) (assoc :tags (vec (:tags entry)))
      (and access (pos? access)) (assoc :A access)
      (not (zero? net)) (assoc :F net))))

(def CallerId
  "Caller identity as the route layer passes it (string or keyword). nil is
   accepted: ctx-id/parse-caller-id falls back to the default caller."
  [:maybe [:or :string :keyword]])

(def DrainCtx
  "Optional drain ctx: task tokens, pins, floor cap and wire policy override."
  [:maybe [:map
           [:tokens {:optional true} [:maybe [:or [:set :string] [:sequential :string]]]]
           [:pins {:optional true} :any]
           [:floor-cap {:optional true} [:maybe :int]]
           [:policy {:optional true} :any]]])

(def DrainResult
  "One drained batch plus the buffer position after it."
  [:map
   [:batch [:vector :map]]
   [:remaining [:int {:min 0}]]
   [:total [:int {:min 0}]]
   [:delivered [:int {:min 0}]]
   [:seq pos-int?]
   [:done {:optional true} [:= true]]
   [:context-refs {:optional true} :any]
   [:pull {:optional true} :any]])

(defn enqueue!
  "Enqueue entries into the memory piggyback buffer.

   MERGES with any existing buffer for the same key: entries already buffered or
   already delivered this session (matched by :id) are skipped, genuinely new
   entries are appended, and the drain cursor is preserved so nothing is re-sent
   and nothing queued is discarded.

   Supplying context-refs marks them undelivered so the next drain carries them.

   opts may carry :private? true when the enqueuer's view of memory is private
   to it (e.g. it is bound to an enclave). The flag is STICKY on the buffer: once
   private, the buffer stays private even after a later non-private enqueue, and
   `adopt-buffer!` will never hand it to another caller.

   Session-scoped: keyed by caller-id only (no project dimension)."
  ([caller-id entries]
   (enqueue! caller-id entries nil))
  ([caller-id entries context-refs]
   (enqueue! caller-id entries context-refs nil))
  ([caller-id entries context-refs opts]
   (let [buffer-key (ctx-id/caller-id-key (ctx-id/parse-caller-id caller-id))
         existing   (bget buffers buffer-key)
         formatted  (mapv format-entry entries)
         prior      (vec (:entries existing))
         known      (into (set (:sent existing)) (keep :id) prior)
         fresh      (filterv #(not (and (:id %) (contains? known (:id %)))) formatted)
         merged     (into prior fresh)
         cursor     (min (:cursor existing 0) (count merged))
         refs       (or context-refs (:context-refs existing))
         refs-delivered? (if (some? context-refs)
                           false (boolean (:refs-delivered? existing)))]
     (bput! buffers buffer-key
            (cond-> {:entries merged
                     :cursor cursor
                     :done (and (empty? (drop cursor merged))
                                (or (nil? refs) refs-delivered?))
                     :seq-num (:seq-num existing 0)
                     :sent (set (:sent existing))
                     :private? (boolean (or (:private? opts) (:private? existing)))}
              (some? refs)
              (assoc :context-refs refs :refs-delivered? refs-delivered?)))
     (log/info "memory-piggyback: enqueued" (count fresh) "new of" (count formatted)
               "offered for" buffer-key
               "- buffer now" (count merged) "entries, cursor" cursor
               (when (seq (:sent existing)) (str ", " (count (:sent existing)) " already delivered"))
               (when context-refs (str ", " (count context-refs) " context-refs"))))))

(m/=> enqueue! [:function
                [:=> [:cat CallerId [:maybe [:sequential :map]]] :nil]
                [:=> [:cat CallerId [:maybe [:sequential :map]] :any] :nil]
                [:=> [:cat CallerId [:maybe [:sequential :map]] :any [:maybe :map]] :nil]])

;; -----------------------------------------------------------------------------
;; drain! stages (CPPB): collect -> promote -> pipeline -> boundary
;; -----------------------------------------------------------------------------

(def ^:private DrainCollected
  "Collect stage output: the projected buffer and the candidate batch."
  [:map
   [:entries [:sequential :map]]
   [:total [:int {:min 0}]]
   [:pending [:maybe [:sequential :map]]]
   [:ranked [:maybe :map]]
   [:candidate [:sequential :map]]])

(def ^:private DrainPlan
  "Fixed facts of one drain: buffer position, next seq, refs and budget."
  [:map
   [:cursor [:int {:min 0}]]
   [:total [:int {:min 0}]]
   [:new-seq pos-int?]
   [:context-refs :any]
   [:budget [:int {:min 0}]]
   [:strict? :boolean]])

(def ^:private DrainStep
  "Promote stage output: what this drain delivers and where it leaves the buffer."
  [:map
   [:plan DrainPlan]
   [:batch [:vector :map]]
   [:send-refs? :boolean]
   [:new-cursor [:int {:min 0}]]
   [:done? :boolean]
   [:offers [:maybe :map]]])

(defn- fifo-candidates
  "Collect (FIFO lane): entries from `cursor` while their running pr-str size
   stays within `budget`. A first entry over budget is still taken unless
   `strict?`, so a non-strict drain always makes progress."
  [entries cursor budget strict?]
  (let [total (count entries)]
    (loop [batch [] chars 0 idx cursor]
      (if (>= idx total)
        batch
        (let [entry (nth entries idx)
              new-chars (+ chars (count (pr-str entry)))]
          (if (and (> new-chars budget) (or strict? (seq batch)))
            batch
            (recur (conj batch entry) new-chars (inc idx))))))))

(m/=> fifo-candidates
      [:=> [:cat [:sequential :map] [:int {:min 0}] [:int {:min 0}] :boolean]
       [:vector :map]])

(defn- collect-drain
  "Collect: resolve the wire policy, project the buffer, and pick the candidate
   batch. A non-empty (:tokens ctx) ranks the undrained tail two-lane via
   drain-rank/select-batch and reorders it; otherwise the batch is FIFO."
  [buf ctx budget strict?]
  (let [{:keys [cursor offers]} buf
        policy  (proj/resolve-policy (:policy ctx))
        entries (proj/project (:entries buf) {:policy policy :pins (:pins ctx)})
        total   (count entries)
        tokens  (:tokens ctx)
        pending (when (seq tokens) (subvec (vec entries) (min cursor total)))
        ranked  (when (seq pending)
                  (rank/select-batch pending {:tokens tokens :offers offers
                                              :pins (:pins ctx)
                                              :floor-cap (:floor-cap ctx)
                                              :char-budget budget :strict-budget? strict?}))]
    {:entries   (if ranked
                  (into (subvec (vec entries) 0 (min cursor total)) (:ordered ranked))
                  entries)
     :total     total
     :pending   pending
     :ranked    ranked
     :candidate (if ranked
                  (:batch ranked)
                  (fifo-candidates entries cursor budget strict?))}))

(m/=> collect-drain [:=> [:cat :map DrainCtx [:int {:min 0}] :boolean] DrainCollected])

(defn- drain-result
  "Pure: the wire result for delivering `batch` under `plan`, carrying the
   context-refs when `refs?` and the pull instruction when the batch holds
   pointers."
  [{:keys [cursor total new-seq context-refs]} batch refs?]
  (let [new-cursor (+ cursor (count batch))
        hint (proj/pull-hint batch)]
    (cond-> {:batch batch :remaining (- total new-cursor)
             :total total :delivered new-cursor :seq new-seq}
      (>= new-cursor total) (assoc :done true)
      refs? (assoc :context-refs context-refs)
      hint (assoc :pull hint))))

(m/=> drain-result [:=> [:cat DrainPlan [:vector :map] :boolean] DrainResult])

(defn- fits-budget?
  "True when the serialized result for `batch` (refs, pull hint, metadata and
   the 30 chars of MEMORY delimiters included) fits the plan's budget. A
   non-strict drain always fits."
  [plan batch refs?]
  (or (not (:strict? plan))
      (<= (+ 30 (count (pr-str (drain-result plan batch refs?)))) (:budget plan))))

(m/=> fits-budget? [:=> [:cat DrainPlan [:vector :map] :boolean] :boolean])

(defn- fit-batch
  "The longest prefix of `candidate` whose result fits a strict budget; the
   whole candidate when the budget is not strict."
  [plan candidate refs?]
  (if (:strict? plan)
    (loop [remaining candidate acc []]
      (if (and (seq remaining) (fits-budget? plan (conj acc (first remaining)) refs?))
        (recur (rest remaining) (conj acc (first remaining)))
        acc))
    candidate))

(m/=> fit-batch [:=> [:cat DrainPlan [:sequential :map] :boolean] [:vector :map]])

(defn- age-offers
  "Every entry of `pending` offered but not taken in `batch` ages by one."
  [offers pending batch]
  (let [taken (into #{} (keep :id) batch)]
    (reduce (fn [m e]
              (let [eid (:id e)]
                (if (and eid (not (contains? taken eid)))
                  (update m eid (fnil inc 0))
                  m)))
            (or offers {})
            pending)))

(m/=> age-offers [:=> [:cat [:maybe :map] [:sequential :map] [:vector :map]] :map])

(defn- promote-drain
  "Promote: settle the batch, whether the context-refs ride along, whether the
   buffer is exhausted, and the aged offers. Refs have priority over entries
   but are only sent when they really fit; an oversized ref waits."
  [buf collected budget strict?]
  (let [{:keys [cursor seq-num context-refs refs-delivered? offers]} buf
        {:keys [total pending ranked candidate]} collected
        plan {:cursor cursor :total total :new-seq (inc seq-num)
              :context-refs context-refs :budget budget :strict? strict?}
        refs-pending? (and (some? context-refs) (not refs-delivered?))
        send-refs? (boolean (and refs-pending? (fits-budget? plan [] true)))
        batch (fit-batch plan candidate send-refs?)
        new-cursor (+ cursor (count batch))]
    {:plan       plan
     :batch      batch
     :send-refs? send-refs?
     :new-cursor new-cursor
     :done?      (boolean (and (>= new-cursor total) (or (not refs-pending?) send-refs?)))
     :offers     (if ranked (age-offers offers pending batch) offers)}))

(m/=> promote-drain [:=> [:cat :map DrainCollected [:int {:min 0}] :boolean] DrainStep])

(defn- next-buffer
  "Pipeline: the buffer written back after `step`. On exhaustion it collapses
   to a content-free tombstone keeping the delivered ids under :sent."
  [buf collected step]
  (let [{:keys [sent context-refs refs-delivered?]} buf
        {:keys [plan new-cursor done? send-refs? offers]} step
        new-seq (:new-seq plan)]
    (cond-> (if done?
              {:entries [] :cursor 0 :done true :seq-num new-seq
               :sent (into (set sent) (keep :id) (:entries collected))}
              {:entries (:entries collected) :cursor new-cursor :done false
               :seq-num new-seq :sent (set sent)})
      (some? context-refs)
      (assoc :context-refs context-refs
             :refs-delivered? (or (boolean refs-delivered?) send-refs?))
      (some? offers) (assoc :offers offers))))

(m/=> next-buffer [:=> [:cat :map DrainCollected DrainStep] :map])

(defn- commit-drain!
  "Boundary: fold the drain into drain-telemetry, write the next buffer back
   and answer the wire result."
  [buffer-key buf collected step]
  (let [{:keys [plan batch send-refs?]} step
        taken (into #{} (keep :id) batch)]
    (telemetry/record! {:seq-num (:new-seq plan) :batch batch
                        :delivered-ids (into [] (keep :id) batch)
                        :offered-ids (when (:ranked collected)
                                       (into [] (comp (keep :id) (remove taken))
                                             (:pending collected)))})
    (bput! buffers buffer-key (next-buffer buf collected step))
    (drain-result plan batch send-refs?)))

(m/=> commit-drain! [:=> [:cat :any :map DrainCollected DrainStep] DrainResult])

(defn drain!
  "Drain next batch of entries within char budget for a caller session.

   Carries :context-refs forward across batches and attaches them to the first
   response after they are enqueued, marking them delivered.

   On exhaustion the buffer collapses to a content-free tombstone retaining the
   delivered ids under :sent, so a later enqueue! for the same caller can skip
   what this session already received.

   The 2-arity takes a drain ctx. A non-empty (:tokens ctx) selects the batch
   two-lane via drain-rank/select-batch, rewrites the undrained tail of the
   buffer into that order, and ages every offered-but-not-taken entry under
   :offers. ctx :pins and :floor-cap are forwarded to the ranker. ctx nil, or
   ctx without :tokens, is FIFO — identical to the 1-arity in both the returned
   batch and the buffer written back.

   The WIRE POLICY is resolved first and applied to the whole buffer, so the
   char budget is spent on what is actually sent. Under :index every pool entry
   becomes an id-and-title pointer while floor entries (axioms, pins) pass
   through whole; the projection is idempotent, so writing a projected buffer
   back is safe. ctx :policy overrides the configured one. When the batch
   carries pointers the result gains :pull, the instruction for turning them
   back into entries.

   Every drain folds its offered / delivered ids into `drain-telemetry`.

   Stages: `collect-drain` -> `promote-drain` -> `next-buffer` ->
   `commit-drain!` (the only effectful one)."
  ([caller-id] (drain! caller-id nil))
  ([caller-id ctx]
   (let [budget (max 0 (min drain-char-budget (or (:char-budget ctx) drain-char-budget)))
         strict? (some? (:char-budget ctx))
         buffer-key (ctx-id/caller-id-key (ctx-id/parse-caller-id caller-id))
         buf (when (pos? budget) (bget buffers buffer-key))]
     (when (and buf (not (:done buf)))
       (let [collected (collect-drain buf ctx budget strict?)
             step (promote-drain buf collected budget strict?)]
         (when (or (seq (:batch step)) (:send-refs? step))
           (commit-drain! buffer-key buf collected step)))))))

(m/=> drain! [:function
              [:=> [:cat CallerId] [:maybe DrainResult]]
              [:=> [:cat CallerId DrainCtx] [:maybe DrainResult]]])

(defn has-pending?
  "Check if a caller session has undrained memory entries."
  [caller-id]
  (let [buffer-key (ctx-id/caller-id-key (ctx-id/parse-caller-id caller-id))
        buf (bget buffers buffer-key)]
    (and (some? buf) (not (:done buf)))))

(m/=> has-pending? [:=> [:cat CallerId] :boolean])

(defn clear-buffer!
  "Clear buffer for a specific caller session. For testing."
  [caller-id]
  (let [buffer-key (ctx-id/caller-id-key (ctx-id/parse-caller-id caller-id))]
    (bounded-swap! buffers dissoc buffer-key)))

(m/=> clear-buffer! [:=> [:cat CallerId] :any])

(defn reset-all!
  "Reset all buffers. For testing.

   Guarded by `when-not-coordinator` — no-op when the live coordinator
   is running so test fixtures cannot wipe live piggyback buffers."
  []
  (guards/when-not-coordinator
   "channel.memory-piggyback/reset-all! blocked"
   (bclear! buffers)))

(m/=> reset-all! [:=> [:cat] :any])

(defn adopt-buffer!
  "Adopt an orphaned buffer from a previous coordinator instance.
   When a coordinator restarts (new instance-id), the old buffer is orphaned.
   This function finds a matching buffer from any coordinator instance
   and re-keys it to the new caller.

   A buffer enqueued under a private view (:private? on the buffer) is never
   adopted: its content is visible only to the caller that wrote it.

   Session-scoped: matches by caller prefix only, no project dimension.

   Returns true if a buffer was adopted, false otherwise."
  [new-caller-id]
  (let [new-key (ctx-id/caller-id-key (ctx-id/parse-caller-id new-caller-id))
        ;; Find orphaned coordinator buffer (any coordinator instance)
        donor (->> @(:atom buffers)
                   (filter (fn [[aid entry]]
                             (let [buf (:data entry)]
                               (and (not= aid new-key)
                                    (clojure.string/starts-with? (str aid) "coordinator:")
                                    (not (:private? buf))
                                    (not (:done buf))))))
                   first)]
    (when-let [[donor-key donor-entry] donor]
      (let [donor-buf (:data donor-entry)]
        (bounded-swap! buffers dissoc donor-key)
        (bput! buffers new-key donor-buf)
        (log/info "memory-piggyback: adopted buffer from" donor-key "->" new-key
                  "(" (- (count (:entries donor-buf)) (:cursor donor-buf)) "entries remaining)")
        true))))

(m/=> adopt-buffer! [:=> [:cat CallerId] [:maybe [:= true]]])

(defn evict-orphaned-buffers!
  "Evict coordinator buffers that have no matching active caller.
   Called during catchup to clean up buffers from dead bb-mcp processes.

   Takes an active caller-id (with instance suffix) and evicts
   coordinator buffers whose key doesn't match the active caller prefix.

   Returns count of evicted buffers."
  [active-caller-id]
  (let [orphaned (->> @(:atom buffers)
                      (filter (fn [[aid _entry]]
                                (and (clojure.string/starts-with? (str aid) "coordinator:")
                                     (not (clojure.string/starts-with? (str aid) (str active-caller-id "-"))))))
                      (map first)
                      vec)]
    (when (seq orphaned)
      (bounded-swap! buffers #(apply dissoc % orphaned))
      (log/info "memory-piggyback: evicted" (count orphaned) "orphaned buffers:" orphaned))
    (count orphaned)))

(m/=> evict-orphaned-buffers! [:=> [:cat :any] [:int {:min 0}]])
