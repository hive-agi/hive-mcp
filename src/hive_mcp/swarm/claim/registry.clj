(ns hive-mcp.swarm.claim.registry
  "Span-granular claims on top of the existing claim rows.

   NO SCHEMA MIGRATION, AND WHY THAT IS SOUND
   ==========================================
   `:claim/file` carries `:db/unique :db.unique/identity` and holds a plain
   string. `span/key-of` produces a string too: the bare path for a whole-file
   claim, `path#qn` for a form claim. Writing the span key into that same
   attribute reuses the uniqueness that is already there, so the registry now
   enforces one claim per SPAN with no change to the schema and no rebuild.

   A legacy row keyed on the bare path keeps colliding exactly as it did, which
   is what makes the rollout safe in both directions: an old agent claiming a
   file still blocks every span inside it, because `overlap` treats a :file
   span as a blanket.

   THE KEY DELIBERATELY IGNORES MODE
   =================================
   `path#qn` is the key for BOTH a :body and a :signature claim on that qn.
   That looks lossy and is load-bearing: if the mode were in the key, a :body
   and a :signature claim on the same form would hash to different keys and
   both could be held at once, which is precisely the collision the registry
   exists to stop. Mode travels beside the key in `:claim/mode` and is read
   back for conflict reasoning, never for identity.

   WHAT THIS IS NOT
   ================
   It is not a mutex. Nothing here stops a write. Carto's structural address
   refuses a stale fingerprint on its own, so a lost update is already
   impossible; these rows exist so two agents find out they are aimed at the
   same form before they spend the tokens, and so a refusal can name the form
   and the holder instead of the file."
  (:require [hive-mcp.swarm.claim.graph :as graph]
            [hive-mcp.swarm.claim.span :as span]
            [hive-mcp.swarm.datascript.lings :as lings]
            [taoensso.timbre :as log]
            [clojure.string :as str]
            [hive-mcp.swarm.logic :as logic]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Reading held claims back as spans
;; =============================================================================

(defn row->span
  "Recover a span from a stored claim row.

   `:claim/qn` and `:claim/mode` are read when present. When they are not, the
   row predates spans (or was written by a file-granular caller) and the key is
   parsed instead: everything after the last # is the qn. Parsing is the
   fallback, not the contract, so a row that carries its own fields is always
   believed over the shape of its key."
  [row]
  (let [k     (or (:claim/file row) "")
        qn    (:claim/qn row)
        mode  (:claim/mode row)
        [file parsed-qn] (if-let [i (str/last-index-of k "#")]
                           [(subs k 0 i) (subs k (inc i))]
                           [k nil])
        qn'   (or qn parsed-qn)]
    (assoc (if qn'
             (span/form-span file qn' (or mode :body))
             (span/file-span file))
           :claim/slave (:claim/slave row)
           :claim/key   k)))

(defn held-spans
  "Every live claim as a span carrying its holder."
  []
  (mapv (fn [row]
          (row->span (cond-> row
                       ;; get-all-claims projects :file/:slave-id rather than
                       ;; the raw attributes, so accept both shapes.
                       (and (:file row) (not (:claim/file row)))
                       (assoc :claim/file (:file row))

                       (and (:slave-id row) (not (:claim/slave row)))
                       (assoc :claim/slave (:slave-id row)))))
        (lings/get-all-claims)))

;; =============================================================================
;; Conflict check
;; =============================================================================

(defn conflicts-for
  "Held claims that block `wanted` for `slave-id`, each with a reason.

   `scope` selects the carto index used for the signature-versus-caller rule.
   nil, or a host without carto, drops that rule and keeps the local ones."
  ([wanted slave-id] (conflicts-for wanted slave-id nil))
  ([wanted slave-id scope]
   (span/conflicts (graph/callers-fn scope) (held-spans) wanted slave-id)))

(defn available?
  "True when nothing blocks `wanted` for `slave-id`."
  ([wanted slave-id] (available? wanted slave-id nil))
  ([wanted slave-id scope]
   (empty? (conflicts-for wanted slave-id scope))))

;; =============================================================================
;; Acquire and release
;; =============================================================================

(defn claim-span!
  "Record a claim on `s` for `slave-id`, unconditionally.

   The caller is responsible for having checked conflicts; `acquire!` is the
   version that does both under one lock."
  [s slave-id & [{:keys [task-id prior-hash]}]]
  (let [sp  (span/span s)
        k   (span/key-of sp)]
    (log/debug "claiming span" k "for" slave-id)
    (lings/claim-file! k slave-id {:task-id task-id :prior-hash prior-hash})))

(defn release-span!
  "Release the claim on `s`."
  [s]
  (lings/release-claim! (span/key-of s)))

(def ^:private claim-lock
  "THE claim lock, resolved once and shared.

   This is deliberately the very atom `coordinator/atomic-claim-files!` locks,
   not a second one. Two mutexes guarding one invariant is not mutual
   exclusion: while both the file-granular and the span-granular path are live,
   a claim taken through either has to serialize against the other, and they
   only do if they contend on one object. Held identical across calls, which
   `registry-test` asserts rather than assumes."
  (logic/get-logic-db-atom))

(defn acquire!
  "Check every span in `wanted` and claim them all, or claim none.

   All-or-nothing on purpose: a partial acquisition leaves an agent holding
   half a refactor and blocking somebody else with it.

   Returns {:acquired? bool :conflicts [...] :spans-claimed n}. Each conflict
   carries :file :qn :mode :held-by :reason plus a rendered :message, so the
   refusal tells an agent which form to wait on rather than which file."
  [wanted slave-id & [{:keys [task-id scope]}]]
  (let [spans (mapv span/span wanted)]
    (if (empty? spans)
      {:acquired? true :conflicts [] :spans-claimed 0}
      (locking claim-lock
        (let [callers (graph/callers-fn scope)
              held    (held-spans)
              found   (vec (mapcat #(span/conflicts callers held % slave-id) spans))]
          (if (seq found)
            (do (log/info "claim refused for" slave-id "on" (count spans)
                          "spans:" (count found) "conflicts")
                {:acquired?     false
                 :spans-claimed 0
                 :conflicts     (mapv #(assoc % :message (span/explain %)) found)})
            (do (doseq [s spans]
                  (claim-span! s slave-id {:task-id task-id}))
                {:acquired?     true
                 :conflicts     []
                 :spans-claimed (count spans)})))))))
