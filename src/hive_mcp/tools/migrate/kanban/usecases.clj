(ns hive-mcp.tools.migrate.kanban.usecases
  "Use cases for the kanban cross-store migrator. Pure-as-possible:
   takes a `deps` map of port adapters, returns Result. All transitions
   emit hive-events on the side-channel — control flow is entirely the
   railway, never the event-loop.

   Deps shape:
     {:source-lister  IIdLister     ; e.g. milvus
      :source-reader  IEntryReader  ; e.g. milvus
      :target-reader  IEntryReader  ; e.g. qdrant
      :target-probe   IIdProbe      ; the target's id set, listed once per run
      :target-durability ITargetDurability ; gate: refuse a non-durable target
      :writer         IEntryWriter  ; e.g. qdrant
      :state          IState}"
  (:refer-clojure :exclude [run!])
  (:require [hive-dsl.result :as r]
            [hive-mcp.tools.migrate.kanban.events :as mig-events]
            [hive-mcp.tools.migrate.kanban.ports :as ports]
            [hive-mcp.tools.migrate.kanban.pure :as pure]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- now-iso [] (str (java.time.ZonedDateTime/now)))

;; =============================================================================
;; init-ids! — Phase A
;; =============================================================================

(defn init-ids!
  "Materialize the candidate id set from the source store and persist into
   migration state. Returns r/ok with `{:total :per-collection}` on success."
  [{:keys [source-lister state] :as _deps}]
  (mig-events/emit :kanban-mig/scan-started {:at (now-iso)})
  (r/let-ok [list-result   (ports/list-ids source-lister)
             current-state (ports/load-state state)]
    (let [ids       (:ids list-result)
          per-coll  (:per-collection list-result)
          new-state (-> current-state
                        (assoc :phase           :ids-listed
                               :all-ids         ids
                               :cursor          0
                               :phase-a-at      (now-iso)
                               :phase-a-summary per-coll))]
      (r/let-ok [_ (ports/save-state! state new-state)]
        (mig-events/emit :kanban-mig/ids-listed
                          {:total (count ids) :per-collection per-coll})
        (r/ok {:total          (count ids)
               :per-collection per-coll})))))

;; =============================================================================
;; migrate-batch! — single batch, no state mutation
;; =============================================================================

(defn- classify-each
  "Pure: derive {:id :outcome :source} for each id in the batch given
   pre-fetched source/target maps. A :ready-to-write row whose target read
   was absent carries `:target-absent? true` (the existence probe's
   candidates); an :unreadable one carries its `:reason`."
  [batch source-map target-map]
  (mapv (fn [id]
          (let [src     (get source-map id)
                tgt     (get target-map id)
                outcome (pure/classify-outcome src tgt)]
            (cond-> {:id id :outcome outcome :source src}
              (and (= :ready-to-write outcome) (not (contains? target-map id)))
              (assoc :target-absent? true)
              (= :unreadable outcome)
              (assoc :reason (pure/unreadable-reason tgt)))))
        batch))

(defn- new-held
  "Run-scoped cache of the target's id set: nil until the first batch that
   needs the existence probe lists it."
  []
  (atom nil))

(defn- held-set
  "r/ok of the target's id set, listed through TARGET-PROBE the first time
   and kept in HELD (an atom) after that, so a run lists the target once,
   not once per batch."
  [target-probe held]
  (if-some [s @held]
    (r/ok s)
    (r/let-ok [s (ports/held-ids target-probe)]
      (r/ok (reset! held (set s))))))

(defn- settle-hidden
  "OUTCOMES with every :ready-to-write whose target read was absent checked
   against the target's id set (HELD, listed once per run): an id the target
   holds (a row a visibility layer hid from the read) becomes :unreadable /
   :hidden and is not written. No candidates, no listing. A probe err is the
   batch's err: nothing is written on a guess."
  [target-probe held outcomes]
  (let [candidates (pure/probe-candidates outcomes)]
    (cond
      (empty? candidates)  (r/ok outcomes)
      (nil? target-probe)  (r/err :target-probe/missing {:n (count candidates)})
      :else
      (r/let-ok [present (held-set target-probe held)]
        (r/ok (pure/mark-hidden outcomes present))))))

(defn- note-written!
  "Add the ids FINAL-OUTCOMES wrote to HELD, once it has been listed: they
   are target rows now. An :unconfirmed id is taken OUT of HELD: it may
   exist only in the target's RAM, and a held id is never written again."
  [held final-outcomes]
  (let [ids   (into [] (comp (filter #(= :written (:outcome %))) (map :id)) final-outcomes)
        unids (pure/unconfirmed-ids final-outcomes)]
    (when (or (seq ids) (seq unids))
      (swap! held #(some-> % (into ids) (as-> s (apply disj s unids)))))))

(defn- check-durable
  "r/ok when the target can show its writes are durable (TARGET-DURABILITY,
   the ITargetDurability port), else its err. No port is an err too: the
   migrator does not write to a target it cannot vouch for."
  [target-durability]
  (if (nil? target-durability)
    (r/err :target-durability/missing
           {:message "no durability port for the migration target: refusing to write"})
    (ports/check-durable target-durability)))

(defn- writes-needed
  "Filter the outcomes whose source needs to be written to the target."
  [outcomes]
  (filter #(= :ready-to-write (:outcome %)) outcomes))

(defn- finalize-outcomes
  "Replace each :ready-to-write outcome with the post-write outcome
   (:written / :queued / :would-write / :failed) using the writer's per-id
   results. A row the target only queued for replay (`:queued? true`) is
   :queued, never :written: it is not durable yet.

   On dry-run, all :ready-to-write become :would-write."
  [outcomes write-results dry-run?]
  (let [by-id (into {} (map (juxt :id identity) write-results))]
    (mapv (fn [{:keys [id outcome] :as o}]
            (cond
              (not= :ready-to-write outcome) o
              dry-run?                       (assoc o :outcome :would-write)
              :else
              (let [w (get by-id id)]
                (cond
                  (:ok? w)     (assoc o :outcome :written)
                  ;; accepted for replay only (a RAM queue): not durable
                  (:queued? w) (assoc o :outcome :queued)
                  :else        (assoc o :outcome :failed
                                        :error   (or (:error w) "no-writer-result"))))))
          outcomes)))

(defn- empty-batch-result
  [cursor]
  (r/ok {:cursor    cursor
         :outcomes  []
         :tally     pure/empty-tally
         :done?     true}))

(defn migrate-batch!
  "Run one cursor-bounded batch. Reads source + target, classifies, probes
   the target for the ids its read found absent (a hidden row is not an
   absent one), writes the ready ones, returns outcomes + tally. Does NOT
   mutate state — the caller (`step!`) folds the result into state.

   Only an id the target truly has no row for (or a content-less stub row)
   is written: add-entry! upserts by id. A row the read could not open, or
   one the probe finds but the read did not show, is :unreadable, reported
   and left alone.

   The probe lists the target's ids once and keeps the set in `:held-ids`
   (an atom `run!` puts in DEPS so every batch of a run shares it; without
   one, this batch lists for itself). Ids written here join the set.

   A :queued outcome (accepted for replay, not durable) holds the cursor at
   that id (`pure/hold-cursor`), so the next step reads and writes it again.

   After a batch that wrote, the target's durability is checked AGAIN: a
   store that lost its backend mid-batch answers add-entry! with the id out
   of an in-memory fallback. If it is not durable now (or the check errs),
   every :written outcome becomes :unconfirmed (`pure/unconfirm-written`),
   leaves the held-id set, and the cursor stays at the batch start; the
   result carries `:not-durable` with the check's err.

   The result carries `:start`, the index the batch began at, so the fold
   can tell the settled part of the batch (before `:cursor`) from the held
   part."
  [{:keys [source-reader target-reader target-probe target-durability writer state held-ids]
    :as _deps}
   {:keys [batch-size dry-run?] :or {batch-size 50 dry-run? false}}]
  (r/let-ok [s (ports/load-state state)
             :let [held (or held-ids (new-held))
                   [batch new-cursor done?]
                   (pure/slice-batch (:all-ids s) (:cursor s) batch-size)
                   start (- new-cursor (count batch))]]
    (if (empty? batch)
      (empty-batch-result new-cursor)
      (do
        (mig-events/emit :kanban-mig/batch-started
                          {:size (count batch) :cursor (:cursor s)})
        (r/let-ok [source-map    (ports/read-by-ids source-reader batch)
                   target-map    (ports/read-by-ids target-reader batch)
                   outcomes      (settle-hidden target-probe held
                                                (classify-each batch source-map target-map))
                   :let [ready    (writes-needed outcomes)
                         entries  (mapv :source ready)]
                   write-results (cond
                                   (empty? ready) (r/ok [])
                                   dry-run?       (r/ok [])
                                   :else
                                   (ports/write-entries writer entries))
                   :let [written        (mapv #(dissoc % :target-absent?)
                                              (finalize-outcomes outcomes write-results dry-run?))
                         ack            (when (pure/needs-durable-ack? written)
                                          (check-durable target-durability))
                         durable?       (or (nil? ack) (r/ok? ack))
                         final-outcomes (pure/unconfirm-written written durable?)
                         _              (note-written! held final-outcomes)
                         [cursor' done?'] (pure/hold-cursor start final-outcomes new-cursor done?)
                         tally          (pure/tally-outcomes final-outcomes)
                         result         (cond-> {:start    start
                                                 :cursor   cursor'
                                                 :outcomes final-outcomes
                                                 :tally    tally
                                                 :done?    done?'}
                                          (not durable?) (assoc :not-durable ack))]]
          (mig-events/emit :kanban-mig/batch-completed
                            (select-keys result [:cursor :tally :done?]))
          (r/ok result))))))

;; =============================================================================
;; step! — one batch, fold into state
;; =============================================================================

(defn- not-durable-err
  "The :target/not-durable err a step returns after BATCH-RESULT's writes
   could not be confirmed durable. Names the check's own err as :cause."
  [{:keys [not-durable cursor outcomes]}]
  (r/err :target/not-durable
         {:message     (str "the migration target was not durable after the batch: "
                            (or (:message not-durable) (:error not-durable))
                            "; its writes are unconfirmed and the cursor stays at "
                            cursor)
          :cause       (:error not-durable)
          :cursor      cursor
          :unconfirmed (pure/unconfirmed-ids outcomes)}))

(defn step!
  "Run one batch and persist the new state. Returns r/ok of
   `{:state :batch-result}`.

   Unless `:dry-run?`, the target must first show its writes are durable
   (`:target-durability`, an ITargetDurability port); on its err nothing is
   read or written and that err is returned.

   The state folds only the part of the batch before the returned cursor
   (`pure/fold-batch`): ids at or after a held cursor (a :queued id and
   those after it, or a whole :unconfirmed batch) are classified again by
   the next step, so they are not counted now. The batch result's :tally
   (and the state's :last-step) still report the whole batch, :queued and
   :unconfirmed included; those two are per-step values, not cumulative
   :stats.

   A batch whose target was not durable once its writes were done (see
   `migrate-batch!`) still saves its state, with the cursor at the batch
   start, and then returns :target/not-durable, so a run stops there and
   the next one writes the batch again."
  [{:keys [state target-durability] :as deps} {:keys [dry-run?] :as opts}]
  (r/let-ok [_            (if dry-run? (r/ok nil) (check-durable target-durability))
             batch-result (migrate-batch! deps opts)
             s            (ports/load-state state)]
    (let [new-state (pure/fold-batch s batch-result (now-iso))]
      (r/let-ok [saved (ports/save-state! state new-state)]
        (if (contains? batch-result :not-durable)
          (not-durable-err batch-result)
          (r/ok {:state saved :batch-result batch-result}))))))

;; =============================================================================
;; run! — repeat step! until done / max-steps
;; =============================================================================

(defn run!
  "Loop step! up to `:max-steps` times or until the migration is :done.
   Stops on err, and with `:stopped :queued` after a step whose target only
   queued a write (its circuit is open: looping would re-queue the same id).
   Returns r/ok with `{:steps-run :stopped :state}`.

   Refuses to start (the durability port's err, nothing read or written)
   unless the target shows its writes are durable; each step checks again.
   Every step of the run shares one cache of the target's id set, so the
   target is listed at most once per run."
  [{:keys [target-durability] :as deps} {:keys [max-steps dry-run?] :or {max-steps 50} :as opts}]
  (r/let-ok [_ (if dry-run? (r/ok nil) (check-durable target-durability))]
    (let [deps (assoc deps :held-ids (new-held))]
      (loop [i 0]
        (let [result (step! deps opts)]
          (cond
            (r/err? result)
            result

            (-> result :ok :batch-result :done?)
            (r/ok {:steps-run (inc i)
                   :stopped   :done
                   :state     (-> result :ok :state)})

            (pos? (get-in result [:ok :batch-result :tally :queued] 0))
            (do
              (mig-events/emit :kanban-mig/run-done
                                {:steps-run (inc i) :stopped :queued})
              (r/ok {:steps-run (inc i)
                     :stopped   :queued
                     :state     (-> result :ok :state)}))

            (>= (inc i) max-steps)
            (do
              (mig-events/emit :kanban-mig/run-done
                                {:steps-run (inc i) :stopped :max-steps})
              (r/ok {:steps-run (inc i)
                     :stopped   :max-steps
                     :state     (-> result :ok :state)}))

            :else
            (recur (inc i))))))))

;; =============================================================================
;; Contracts
;; =============================================================================

(def Result
  "A hive-dsl Result: {:ok value} or {:error category ...}."
  [:or [:map [:ok :any]] [:map [:error :keyword]]])

(def Deps
  "The wired port map (`api/make-deps`); each use case reads only its slots."
  [:map
   [:source-lister {:optional true} :any]
   [:source-reader {:optional true} :any]
   [:target-reader {:optional true} :any]
   [:target-probe {:optional true} :any]
   [:target-durability {:optional true} :any]
   [:writer {:optional true} :any]
   [:state {:optional true} :any]
   [:held-ids {:optional true} :any]])

(def RunOpts
  [:maybe
   [:map
    [:batch-size {:optional true} [:int {:min 1}]]
    [:max-steps {:optional true} [:int {:min 0}]]
    [:dry-run? {:optional true} :any]]])

(m/=> init-ids! [:=> [:cat Deps] Result])

(m/=> migrate-batch! [:=> [:cat Deps RunOpts] Result])

(m/=> step! [:=> [:cat Deps RunOpts] Result])

(m/=> status [:=> [:cat Deps] Result])

(m/=> run! [:=> [:cat Deps RunOpts] Result])

;; =============================================================================
;; status — read-only
;; =============================================================================

(defn status
  "Read-only progress snapshot. :stats counts each id the cursor has passed
   once; :last-step is the latest step's whole-batch report (its :tally
   includes the :queued / :unconfirmed ids that held the cursor)."
  [{:keys [state] :as _deps}]
  (r/let-ok [s (ports/load-state state)]
    (r/ok {:phase           (:phase s)
           :cursor          (:cursor s)
           :total           (count (:all-ids s))
           :remaining       (max 0 (- (count (:all-ids s)) (:cursor s)))
           :stats           (:stats s)
           :last-step       (:last-step s)
           :failed-count    (count (:failed s))
           :unreadable      (vec (:unreadable s))
           :phase-a-summary (:phase-a-summary s)
           :phase-a-at      (:phase-a-at s)
           :last-step-at    (:last-step-at s)})))
