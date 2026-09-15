(ns hive-mcp.tools.memory.crud.edit
  "Edit handler for memory entries.

   In-place mutation preserves entry ID, which preserves all KG edges
   (edges are keyed by entry ID, never content). Content changes trigger
   re-embedding via IMemoryStore/update-entry! — the Chroma backend's
   index-memory-entry! path re-embeds transparently on content change.

   Kanban move-to-status! already relies on the same primitive (see
   tools/memory_kanban.clj:292), so this handler just surfaces it to
   callers as a first-class MCP command.

   Batch edit runs sequentially for now. Addendum 20260423133956-0aa648bc
   tracks the single-Chroma-upsert + single-Datalevin-tx optimization as
   follow-up work; correctness ships first."
  (:require [hive-mcp.tools.memory.core :refer [with-store]]
            [hive-mcp.tools.memory.duration :as duration]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.memory.type-registry :as type-registry]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.memory.write-events :as write-events]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- normalize-type
  "Canonicalize a type parameter into its safe token form, auto-registering
   unknown-but-safe types with sane defaults (symmetric with the add path).
   Nil (no type change) passes through as nil."
  [t]
  (when t (type-registry/ensure-type! t)))

(defn- validate-type!
  "Throw ex-info with :invalid-type marker when the incoming type is unsafe
   (bad charset / oversized — see type-registry/safe-type?). Unknown-but-safe
   types are accepted (and auto-registered by normalize-type). Nil is allowed
   (means 'no type change')."
  [t]
  (when (and t (not (type-registry/valid-type? t)))
    (throw (ex-info (str "Invalid memory type: " (pr-str t)
                         ". Type must be a safe token — letters, digits, '_' or '-', "
                         "starting with a letter, max " type-registry/max-type-length
                         " chars.")
                    {:type :invalid-type :value t}))))

(def edit-params
  "Params an edit acts on."
  #{:id :type :content :find :replace :tags :duration :abstraction_level :reason})

(def envelope-params
  "Routing/transport params a call may carry that the edit itself ignores.
   Keys whose name starts with `_` (e.g. :_caller_id, :_caller_cwd) are
   transport-injected and are accepted as well."
  #{:command :tool :directory :agent_id :project_id :project-id
    :async :async-timeout-ms :timeout_ms :compact :depends_on})

(defn- injected-param?
  [k]
  (str/starts-with? (if (keyword? k) (name k) (str k)) "_"))

(defn unrecognised-params
  "Sorted vector of the param names in `params` that are neither edit params,
   envelope params, nor transport-injected. Empty when every key is known."
  [params]
  (->> (keys params)
       (remove #(or (edit-params %) (envelope-params %) (injected-param? %)))
       (map #(if (keyword? %) (name %) (str %)))
       sort
       vec))

(defn- unrecognised-params-message
  [unknown]
  (str "Unrecognised edit param(s): " (str/join ", " unknown)
       ". memory edit accepts: "
       (str/join ", " (sort (map name edit-params)))
       "."))

(defn- invalid-edit!
  [msg data]
  (throw (ex-info msg (assoc data :type :invalid-edit))))

(defn- find-replace
  "Replace the single occurrence of `find` in `text` with `replace`.
   Throws ex-info {:type :invalid-edit} when `find` occurs zero or more than
   one time (overlapping occurrences count)."
  [text find replace]
  (let [i (.indexOf ^String text ^String find)]
    (cond
      (neg? i)
      (invalid-edit! "find text not found in entry content" {:find find})

      (not (neg? (.indexOf ^String text ^String find (int (inc i)))))
      (invalid-edit! (str "find text matches more than once in entry content;"
                          " extend it until it is unique")
                     {:find find})

      :else
      (str (subs text 0 i) replace (subs text (+ i (count find)))))))

(defn- resolve-content
  "Effective new content of an edit: `:content` verbatim, or `:find`/`:replace`
   applied to the existing content (exact substring, unique match). Nil when
   the edit does not touch content. Throws ex-info {:type :invalid-edit} when
   :find/:replace are incomplete, non-string, blank :find, combined with
   :content, or :find is not a unique match."
  [existing {:keys [content find replace] :as params}]
  (let [find?    (contains? params :find)
        replace? (contains? params :replace)]
    (cond
      (not (or find? replace?))
      content

      (some? content)
      (invalid-edit! "content and find/replace are mutually exclusive" {})

      (not (and find? replace?))
      (invalid-edit! "find and replace must be given together" {})

      (not (and (string? find) (not (str/blank? find))))
      (invalid-edit! "find must be a non-blank string" {:find find})

      (not (string? replace))
      (invalid-edit! "replace must be a string" {:replace replace})

      :else
      (find-replace (or (:content existing) "") find replace))))

(defn- build-updates
  "Compute the partial-update map from the incoming edit params against the
   existing entry. Returns [updates content-changed?]."
  [existing {:keys [type tags duration abstraction_level] :as params}]
  (let [content (resolve-content existing params)
        ;; Same write gate as the add path: an edit may not launder an entry
        ;; into a human-gated type either. Parking rewrites BOTH the type and
        ;; the tags, merging onto the entry's existing tags when the edit did
        ;; not supply its own.
        {eff-type :type :keys [queued? gate requested]}
        (when type (type-registry/resolve-write-type type))

        new-type         (normalize-type (or eff-type type))
        gated-tags       (when queued?
                           (type-registry/queued-tags
                            (if (some? tags) tags (:tags existing))
                            gate requested))
        content-changed? (and (some? content) (not= content (:content existing)))
        requested-updates
        (cond-> {}
          new-type          (assoc :type new-type)
          content-changed?  (assoc :content content
                                   :content-hash (mem-proto/content-hash content))
          (some? tags)      (assoc :tags tags)
          ;; after the caller's tags, so parking always wins
          gated-tags        (assoc :tags gated-tags)
          duration          (assoc :duration duration
                                   :expires (or (duration/calculate-expires duration) ""))
          abstraction_level (assoc :abstraction-level abstraction_level))
        updates (cond-> requested-updates
                  (seq requested-updates)
                  (assoc :updated (mem-proto/iso-timestamp)))]
    [updates content-changed?]))

(defn- store-holding
  "Resolve the registered store that actually holds `id`, plus the entry it
   holds. The :default slot is probed first, then every other registered slot
   (:kanban, :carto, …), so an edit reaches an entry wherever it lives —
   matching the unified READ that `kanban get` already performs.

   Returns [store existing], or nil when no registered store has the id."
  [id]
  (let [default (mem-proto/get-store)
        others  (->> (mem-proto/registered-stores)
                     (remove (fn [[k _]] (= k :default)))
                     (map val))]
    (or (when-let [e (mem-proto/get-entry default id)]
          [default e])
        (some (fn [s]
                (when-let [e (try (mem-proto/get-entry s id)
                                  (catch Throwable _ nil))]
                  [s e]))
              others))))

(defn- updated-entry
  "Normalize the store-dependent return of `IMemoryStore/update-entry!`.

   Milvus/Chroma answer with the updated entry MAP; the qdrant store backing
   the :kanban slot answers with the entry ID STRING. Both are successes.
   Returns the updated entry map, or nil when the store signalled failure —
   an opaque value that is neither a clean map nor the entry's own id."
  [store id updated]
  (cond
    (and (map? updated) (not (:error updated))) updated
    (and (string? updated) (= updated id))      (mem-proto/get-entry store id)
    :else                                       nil))

(defn- apply-edit!
  "Apply a single edit to whichever registered store holds the entry. Returns a
   result map describing the outcome, or nil when no store has the id. Does not
   build MCP-shaped responses — leaves that to the caller so batch ops can
   aggregate cleanly. Announces a successful write through write-events."
  [{:keys [id reason] :as params}]
  (when-let [[store existing] (store-holding id)]
    (let [[updates content-changed?] (build-updates existing params)]
      (if (empty? updates)
        {:id id :noop true :existing existing}
        (let [raw     (mem-proto/update-entry! store id updates)
              updated (updated-entry store id raw)]
          (when-not updated
            (throw (ex-info (str "Memory store update failed for " id)
                            {:type :memory-update-failed
                             :id id
                             :result-type (some-> raw class str)})))
          (log/info "Memory edit:" id
                    "fields:" (vec (keys updates))
                    (when content-changed? "[re-embed]")
                    (when reason (str "reason:" reason)))
          (write-events/notify! :updated {:id          id
                                          :memory-type (:type updated)
                                          :tags        (:tags updated)
                                          :project-id  (:project-id updated)
                                          :fields      (vec (keys updates))})
          {:id               id
           :updated          updated
           :fields-changed   (vec (keys updates))
           :content-changed? content-changed?})))))

(defn handle-edit
  "Edit a memory entry in place. Entry ID is preserved, so all KG edges —
   keyed by entry ID — survive the edit untouched.

   Params:
     :id                 — required, entry to edit
     :type               — optional, new memory type (validated against registry)
     :content            — optional, new content (triggers re-embed)
     :find / :replace    — optional pair, exclusive with :content: replace the
                           one exact occurrence of :find in the content; zero
                           or multiple occurrences is an error
     :tags               — optional, replaces existing tags
     :duration           — optional, new TTL category
     :abstraction_level  — optional, new abstraction level 1-4
     :reason             — optional, logged for audit; stored later as KG
                           edit-event node once audit-trail slice lands

   Any other param (outside envelope-params and `_`-prefixed transport keys)
   is rejected with an error naming it; nothing is written."
  [{:keys [id type] :as params}]
  (cond
    (or (nil? id) (str/blank? id))
    (mcp-error "id is required (non-blank string)")

    (seq (unrecognised-params params))
    (mcp-error (unrecognised-params-message (unrecognised-params params)))

    :else
    (try
      (validate-type! type)
      (with-store
        (let [result (apply-edit! params)]
          (cond
            (nil? result)
            (mcp-json {:error "Entry not found" :id id})

            (:noop result)
            (mcp-json (assoc (fmt/entry->json-alist (:existing result))
                             :noop true
                             :edit_applied false))

            :else
            (mcp-json (assoc (fmt/entry->json-alist (:updated result))
                             :edit_applied  true
                             :fields_changed (:fields-changed result)
                             :content_changed (boolean (:content-changed? result)))))))
      (catch clojure.lang.ExceptionInfo e
        (if (= :invalid-type (:type (ex-data e)))
          (mcp-error (.getMessage e))
          (throw e))))))

(defn- batch-op-result
  [op]
  (try
    (when-let [unknown (seq (unrecognised-params op))]
      (invalid-edit! (unrecognised-params-message unknown) {}))
    (validate-type! (:type op))
    (if-let [r (apply-edit! op)]
      (cond
        (:noop r)
        {:id (:id op) :status :noop}

        :else
        {:id               (:id op)
         :status           :edited
         :fields-changed   (:fields-changed r)
         :content-changed? (boolean (:content-changed? r))})
      {:id (:id op) :status :not-found})
    (catch Throwable t
      (log/warn "Batch-edit op failed for" (:id op) ":" (.getMessage t))
      {:id (:id op) :status :error :error (.getMessage t)})))

(defn- tally-statuses
  [results]
  (let [status->count (frequencies (map :status results))]
    {:total     (count results)
     :edited    (get status->count :edited 0)
     :noop      (get status->count :noop 0)
     :not-found (get status->count :not-found 0)
     :errors    (get status->count :error 0)}))

(defn handle-batch-edit
  "Apply a batch of edits sequentially. Each operation has the same shape
   as handle-edit params. Returns a summary + per-op result vector.

   Params:
     :operations  — required, seq of {:id ... :type? ... :content? ... :tags? ...}
     :dry-run     — optional, validate + preview without writing (default false)
     :atomic      — accepted but not enforced yet — follow-up ships the single-tx
                    path per addendum 20260423133956-0aa648bc"
  [{:keys [operations dry-run]
    :or   {dry-run false}}]
  (cond
    (empty? operations)
    (mcp-error "operations is required (non-empty array of edit ops)")

    dry-run
    (mcp-json {:dry_run  true
               :op_count (count operations)
               :preview  (mapv (fn [op]
                                 (let [unknown (unrecognised-params op)]
                                   (cond-> (select-keys op [:id :type :content :find :replace
                                                            :tags :duration
                                                            :abstraction_level :reason])
                                     (seq unknown) (assoc :unrecognised_params unknown))))
                               operations)})

    :else
    (with-store
      (let [results (mapv batch-op-result operations)
            summary (tally-statuses results)]
        (mcp-json (assoc summary :results results))))))
