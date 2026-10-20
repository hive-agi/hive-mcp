(ns hive-mcp.tools.migrate.kanban.pure
  "Pure helpers for the kanban cross-store migration. No IO, no side effects.
   Every fn here is data-in/data-out so trifecta tests can exercise the full
   surface without spinning up milvus or qdrant.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Predicates
;; =============================================================================

(defn task-type-kanban?
  "True iff the entry's :content map carries the kanban task-type marker.

   Tolerates both keyword and string keys because content travels through
   JSON-encoded payload at qdrant rest and clojure-printed maps in milvus
   legacy collections."
  [entry]
  (let [c (:content entry)]
    (boolean
     (or (= "kanban" (get c :task-type))
         (= "kanban" (get c "task-type"))))))

(defn full-payload?
  "True iff the entry has a non-empty :content map. A bare {:id ...} stub
   from a half-finished cutover counts as empty."
  [entry]
  (boolean (and entry (map? (:content entry)) (seq (:content entry)))))

(defn stub-row?
  "True iff ENTRY is a row with nothing in it to lose: no :content, or an
   empty opened map (a half-finished cutover). A string :content (a sealed
   envelope the reader could not open) is NOT a stub: it holds a card."
  [entry]
  (boolean
   (and (map? entry)
        (let [c (:content entry)]
          (or (nil? c) (and (map? c) (empty? c)))))))

;; =============================================================================
;; Outcome classification — single decision point, no IO
;; =============================================================================

(def classifier-outcome-types
  "Closed enum of classifier outcomes (pre-write). `classify-outcome`
   returns one of these. Use cases consume them and may transform
   `:ready-to-write` into a post-write outcome (`:written`, `:would-write`,
   `:failed`), or into `:unreadable` when the existence probe finds a row
   the read did not show (`mark-hidden`). `:unreadable` is reported, never
   written."
  #{:missing-from-source
    :not-task
    :already-full
    :unreadable
    :ready-to-write})

(def post-write-outcome-types
  "Outcomes emitted only after the writer runs. `:queued` is a write the
   target only ACCEPTED for replay (hive-qdrant's in-memory queue behind an
   open circuit): not durable, so the cursor must not move past it.
   `:unconfirmed` is a write the target acknowledged while the batch ran
   but could not show durable once the batch ended (`unconfirm-written`):
   it may live only in an in-memory fallback, so it is written again."
  #{:written :would-write :failed :queued :unconfirmed})

(def outcome-types
  "Union of all outcomes that may appear in a tally — classifier results
   plus post-write transformations."
  (into classifier-outcome-types post-write-outcome-types))

(defn classify-outcome
  "Decide what to do for an id given the (already-fetched) entries from
   each store. Pure: callers fetch then classify, never mixed.

   `source-entry`  — entry read from the migration source (milvus :default)
   `target-entry`  — entry read from the migration target (qdrant :kanban),
                     nil when the target answered no row for the id.

   Only an ABSENT target (nil) or a `stub-row?` (a bare id with no
   content, from a half-finished cutover) is :ready-to-write. Any other
   target row that is not an opened kanban card is :unreadable and is never
   written: add-entry! upserts by id, so writing would replace whatever the
   target holds (a card edited since it was migrated, still sealed because
   its key could not be resolved, or a different entry) with the stale
   source copy. A nil target can still be a row a visibility layer hid;
   `mark-hidden` settles that with an existence probe before any write.

   Returns a keyword from `classifier-outcome-types`."
  [source-entry target-entry]
  (cond
    (nil? source-entry)                    :missing-from-source
    (not (task-type-kanban? source-entry)) :not-task
    (and target-entry
         (task-type-kanban? target-entry)
         (full-payload? target-entry))     :already-full
    (or (nil? target-entry)
        (stub-row? target-entry))          :ready-to-write
    :else                                  :unreadable))

(defn unreadable-reason
  "Why an outcome is :unreadable, given the TARGET row the read returned:
   :unopenable when the read returned a row whose content is not an opened
   map (a sealed envelope whose key could not be resolved), :hidden when
   the read returned nothing but the existence probe found the id, else
   :not-a-card (an opened row that is not a kanban card)."
  [target-entry]
  (cond
    (nil? target-entry)                      :hidden
    (not (map? (:content target-entry)))     :unopenable
    :else                                    :not-a-card))

(defn probe-candidates
  "Ids of the :ready-to-write OUTCOMES whose target read was ABSENT (no
   row at all, `:target-absent? true`): only these can be rows a visibility
   layer hid, so only these need the existence probe."
  [outcomes]
  (into [] (comp (filter #(and (= :ready-to-write (:outcome %)) (:target-absent? %)))
                 (map :id))
        outcomes))

(defn mark-hidden
  "OUTCOMES with every :ready-to-write whose id is in PRESENT (the ids the
   existence probe found in the target) turned :unreadable / :hidden: the
   target holds a row the read could not see, and add-entry! would
   overwrite it."
  [outcomes present]
  (let [present (set present)]
    (mapv (fn [{:keys [id outcome] :as o}]
            (if (and (= :ready-to-write outcome) (contains? present id))
              (assoc o :outcome :unreadable :reason :hidden)
              o))
          outcomes)))

;; =============================================================================
;; Cursor / batch slicing
;; =============================================================================

(defn slice-batch
  "Return [batch new-cursor done?] for `cursor` advancing by `batch-size`
   over `ids` (vector). Never reads past the end. `done?` indicates the
   cursor reached or passed `(count ids)`."
  [ids cursor batch-size]
  (let [total (count ids)
        start (max 0 (min cursor total))
        end   (min (+ start batch-size) total)
        batch (if (< start end) (subvec ids start end) [])
        new-cursor end
        done?     (>= new-cursor total)]
    [batch new-cursor done?]))

(defn hold-cursor
  "[cursor done?] for a batch that started at START, given its final
   OUTCOMES (in batch order) and the END / DONE? `slice-batch` proposed.
   Any `:unconfirmed` outcome keeps the cursor AT START: the target lost
   its backend during the batch, so no write of it is known durable and
   the whole batch is read and written again. Otherwise the cursor stops
   AT the first `:queued` outcome, so that id and every one after it is
   read again on the next step (ids written after it then classify as
   :already-full); with nothing held the proposal stands."
  [start outcomes end done?]
  (if (some #(= :unconfirmed (:outcome %)) outcomes)
    [start false]
    (if-let [i (first (keep-indexed (fn [i o] (when (= :queued (:outcome o)) i))
                                    outcomes))]
      [(+ start i) false]
      [end done?])))

(defn needs-durable-ack?
  "True iff OUTCOMES carry a :written row: the batch has writes whose
   durability must be checked again once the batch is over."
  [outcomes]
  (boolean (some #(= :written (:outcome %)) outcomes)))

(defn unconfirm-written
  "OUTCOMES after the post-batch durability check. DURABLE? true leaves
   them as they are. Otherwise every :written becomes :unconfirmed: the
   target lost its backend at some point during the batch, and a store
   without a backend (hive-qdrant's fallback-atom) still answers add-entry!
   with the id, so no write of the batch can be told apart from one held
   in RAM only. `hold-cursor` then keeps the cursor at the batch start and
   the next step writes them again (add-entry! upserts by id)."
  [outcomes durable?]
  (if durable?
    outcomes
    (mapv (fn [o]
            (if (= :written (:outcome o))
              (assoc o :outcome :unconfirmed
                     :error "target not durable after the batch; write unconfirmed")
              o))
          outcomes)))

(defn unconfirmed-ids
  "Ids of the :unconfirmed OUTCOMES, in order."
  [outcomes]
  (into [] (comp (filter #(= :unconfirmed (:outcome %))) (map :id)) outcomes))

;; =============================================================================
;; Tally
;; =============================================================================

(def empty-tally
  "Zero-initialized per-outcome counts so callers can sum into a known shape."
  (zipmap outcome-types (repeat 0)))

(defn tally-outcomes
  "Frequency count of `:outcome` values across an outcomes seq. Always
   returns the full `empty-tally` shape so downstream stat-merging is
   total."
  [outcomes]
  (reduce (fn [acc o]
            (update acc (:outcome o) (fnil inc 0)))
          empty-tally
          outcomes))

(defn merge-tally
  "Sum two tally maps key-by-key. Missing keys default to 0."
  [a b]
  (reduce-kv (fn [m k v] (update m k (fnil + 0) v))
             a
             b))

;; =============================================================================
;; Folding a batch into saved state
;; =============================================================================

(def transient-outcome-types
  "Outcomes that never settle an id: `:queued` and `:unconfirmed` both hold
   the cursor at or before their id, so the id is classified again on the
   next step. They are reported per step (the batch result's :tally and the
   state's :last-step), never summed into the cumulative :stats."
  #{:queued :unconfirmed})

(defn settled-outcomes
  "The part of a batch's OUTCOMES (in batch order, the batch starting at
   index START of the id list) whose ids lie before CURSOR, the cursor
   `hold-cursor` returned. Those ids are final: the next step starts at
   CURSOR, so every id at or after it is read and classified again and must
   not be counted now."
  [start cursor outcomes]
  (let [os (vec outcomes)
        n  (max 0 (min (count os) (- cursor start)))]
    (subvec os 0 n)))

(defn- upsert-rows
  "ROWS (maps keyed by :id) with every id in SETTLED taken out, then the
   SETTLED outcomes classified K appended as KS: the last classification of
   an id wins, and an id that settled any other way (written, already full)
   leaves the list."
  [rows settled k ks]
  (let [ids (into #{} (map :id) settled)]
    (into (into [] (remove #(contains? ids (:id %))) rows)
          (comp (filter #(= k (:outcome %)))
                (map #(select-keys % ks)))
          settled)))

(defn fold-batch
  "The next migration state given state S, a batch result and the timestamp
   AT.

   Only the settled outcomes (`settled-outcomes`: the ids before the
   returned cursor) are folded: tallied into :stats, counted in
   :stats :scanned, and used to update :failed (`{:id :error}`) and
   :unreadable (`{:id :reason}`), both keyed by id. Outcomes at or after a
   held cursor are left out, since the next step classifies those ids
   again; a resume after a hold therefore counts each id once.

   `transient-outcome-types` are never cumulative stats (and are dropped
   from a state saved before this rule). The whole batch's tally, held part
   included, is kept as :last-step `{:at :start :cursor :settled :held
   :tally}`, replaced every step, so an operator can see what the last step
   queued or could not confirm."
  [s {:keys [start cursor outcomes tally done?]} at]
  (let [start   (or start cursor)
        settled (settled-outcomes start cursor outcomes)
        drop-tr #(apply dissoc % transient-outcome-types)]
    (-> s
        (assoc :cursor       cursor
               :last-step-at at
               :phase        (if done? :done :running)
               :last-step    {:at      at
                              :start   start
                              :cursor  cursor
                              :settled (count settled)
                              :held    (- (count outcomes) (count settled))
                              :tally   (or tally (tally-outcomes outcomes))})
        (update :stats #(drop-tr (merge-tally (or % {}) (tally-outcomes settled))))
        (update-in [:stats :scanned] (fnil + 0) (count settled))
        (update :failed upsert-rows settled :failed [:id :error])
        (update :unreadable upsert-rows settled :unreadable [:id :reason]))))

;; =============================================================================
;; Id deduplication
;; =============================================================================

(defn dedup-sorted-ids
  "Given a seq of seqs of ids, flatten + dedup + sort ascending. Used by
   the milvus adapter after fanning across per-dim collections."
  [id-seqs]
  (->> id-seqs (apply concat) distinct sort vec))

;; =============================================================================
;; Derived fields — never carried across stores
;; =============================================================================

(def derived-fields
  "Entry keys a store derives from :content (the embedder's input text and
   the vector). The migrator must neither persist them verbatim nor hand
   them to the target store: :embed-text is plaintext of the content, so a
   sealed target would store it next to the ciphertext. The target store's
   own decorator chain derives what it needs from :content."
  [:embed-text :embedding])

(defn strip-derived
  "ENTRY without the `derived-fields`. nil passes through."
  [entry]
  (when entry
    (apply dissoc entry derived-fields)))

;; =============================================================================
;; Write answers — what one add-entry! returned, as a writer result row
;; =============================================================================

(defn write-result
  "Fold the answer of one `add-entry!` call for ID into a writer result row
   `{:id :ok? :error}`. Only an id string (landed) is a success.

   A queue acknowledgement (`:queued? true`, as hive-qdrant answers while
   its circuit breaker is open) is NOT a durable write: that queue lives in
   RAM and dies with the process. It folds to `{:ok? false :queued? true}`,
   which the use case turns into a `:queued` outcome that holds the cursor
   so the id is written again on the next step. A failure map
   (`:success? false` or a non-nil `:error`) and any other value fail."
  [id answer]
  (cond
    (and (string? answer) (seq answer))
    {:id id :ok? true :error nil}

    (and (map? answer)
         (true? (:queued? answer))
         (not (false? (:success? answer)))
         (nil? (:error answer)))
    {:id id :ok? false :queued? true :error "queued for replay, not durable"}

    (map? answer)
    {:id id :ok? false :error (str (or (:error answer) "add-entry! refused"))}

    :else
    {:id id :ok? false :error (str "unexpected add-entry! answer: " (pr-str answer))}))
