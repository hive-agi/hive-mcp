(ns hive-mcp.swarm.claim.span
  "What a swarm agent claims, narrowed from a file to a span of one.

   WHY THIS EXISTS
   ===============
   `:claim/file` carries `:db/unique :db.unique/identity`, so the claim registry
   admits exactly ONE claim per file. Two lings touching two unrelated defns in
   a 900-line namespace serialize against each other for no reason, and the
   usual escape (a worktree per ling) trades the contention for a merge.

   Carto already knows where a form starts and stops, and already refuses a
   write whose fingerprint went stale. What was missing is a claim key at that
   same granularity. A span is that key.

   SAFETY VERSUS COORDINATION
   ==========================
   These are different jobs and only one of them is this namespace's.

   SAFETY is already solved: `carto write-form` resolves a structural address
   `@qn~fingerprint` and REFUSES to apply when the form it fingerprinted has
   changed underneath (see `sad-stale-fingerprint-address-is-refused`). A lost
   update cannot happen whether or not anybody claimed anything.

   COORDINATION is what spans buy: two agents finding out BEFORE they spend a
   model call that they are aiming at the same form, or at a form whose
   signature the other is about to change. Claims here are therefore ADVISORY.
   An expired or missing claim never makes a write unsafe, it only makes the
   collision expensive.

   THE THREE MODES
   ===============
   :file      the whole file, which is the old behaviour. Correct and necessary
              for a file carto has no index for: an .edn, a resource, a
              namespace mid-rename. A file claim conflicts with everything in
              that file, including every span inside it.
   :body      one form's interior. Conflicts only with another claim on the
              SAME qualified name. Two :body claims on different qns in one
              file do NOT conflict, which is the whole point.
   :signature the form's shape: arity, name, destructuring, the parts a caller
              can observe. Conflicts with :body and :signature on the same qn,
              AND with any claim on a CALLER of that qn, because changing an
              arity edits the callers whether or not the editor meant to.

   That last rule is why this reads the call graph. `callers-fn` is injected
   rather than required so the algebra stays pure and testable against a map,
   and so a caller can pass a carto lookup, a cached one, or a stub."
  (:require [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Schema
;; =============================================================================

(def modes
  "Claim modes, widest first. Order matters: `widen` takes the max."
  [:file :signature :body])

(def mode-rank
  "Higher rank subsumes lower. :file covers a whole file, so it outranks both
   form modes; :signature reaches callers, so it outranks :body."
  {:body 0 :signature 1 :file 2})

(def Span
  "A claimable region. `:qn` is nil exactly when `:mode` is :file."
  [:map
   [:span/file :string]
   [:span/mode (into [:enum] modes)]
   [:span/qn {:optional true} [:maybe :string]]])

;; =============================================================================
;; Construction
;; =============================================================================

(defn- clean-qn
  "A qualified name, or nil when there is not one. Whitespace is not a name:
   `not-empty` alone passes \"  \" through, which kept a :body mode alive with
   nothing to point at."
  [qn]
  (let [s (some-> qn str str/trim)]
    (when-not (str/blank? s) s)))

(defn file-span
  "The whole file. The old unit, kept because an unindexed file has no other."
  [file]
  {:span/file (str file) :span/mode :file :span/qn nil})

(defn form-span
  "One form in one file. `mode` is :body or :signature."
  ([file qn] (form-span file qn :body))
  ([file qn mode]
   {:span/file (str file)
    :span/mode (if (contains? mode-rank mode) mode :body)
    :span/qn   (clean-qn qn)}))

(defn span
  "Coerce loose input into a Span. A map passes through, a bare string is a
   file claim, so every existing whole-file caller keeps working untouched."
  [x]
  (cond
    (map? x)    (let [m    (update x :span/file str)
                      qn   (clean-qn (:span/qn m))
                      mode (if (and (nil? qn) (not= :file (:span/mode m)))
                             ;; A form mode with no qn cannot name a form, so it
                             ;; degrades to the whole file rather than claiming
                             ;; nothing. Silently claiming less than asked is the
                             ;; one failure mode that loses an edit.
                             :file
                             (or (:span/mode m) :body))]
                  (assoc m :span/qn qn :span/mode mode))
    (string? x) (file-span x)
    :else       (file-span (str x))))

(defn key-of
  "Stable identity string for a span, for use as a registry unique key.
   A file claim keys on the path alone, so it still collides with the legacy
   `:claim/file` rows written before spans existed."
  [s]
  (let [{:span/keys [file qn mode]} (span s)]
    (if (or (= :file mode) (str/blank? (str qn)))
      file
      (str file "#" qn))))

(defn widen
  "The narrower of two modes loses. Used when one agent holds a span and asks
   to escalate it, so an escalation never silently narrows."
  [a b]
  (if (>= (get mode-rank a -1) (get mode-rank b -1)) a b))

;; =============================================================================
;; Conflict
;; =============================================================================

(defn- same-file? [a b]
  (= (:span/file a) (:span/file b)))

(defn- file-mode? [s]
  (= :file (:span/mode s)))

(defn- signature? [s]
  (= :signature (:span/mode s)))

(defn overlap
  "Why spans `a` and `b` collide, or nil when they do not.

   `callers-fn` maps a qualified name to a collection of qualified names that
   call it. It is consulted ONLY for a :signature span, because only a
   signature change reaches a caller. Passing nil disables the graph rule and
   leaves the purely local ones, which is the honest degradation when the carto
   index is cold: fewer conflicts reported, never a wrong one.

   Returns {:reason kw :qn s} so a refusal can say WHICH form and WHY, rather
   than the file-level 'someone else is in this file' that taught everyone to
   ignore it."
  [callers-fn a b]
  (let [a (span a) b (span b)]
    (when (same-file? a b)
      (let [qa (:span/qn a) qb (:span/qn b)]
        (cond
          ;; A whole-file claim is a blanket: it covers every span inside.
          (or (file-mode? a) (file-mode? b))
          {:reason :file-claim
           :qn     (or qa qb)}

          (= qa qb)
          {:reason :same-form :qn qa}

          ;; Changing a's signature rewrites a's callers. If b sits on one of
          ;; them, the two edits land in the same text.
          (and (signature? a) callers-fn qa qb
               (contains? (set (callers-fn qa)) qb))
          {:reason :signature-vs-caller :qn qa :caller qb}

          (and (signature? b) callers-fn qb qa
               (contains? (set (callers-fn qb)) qa))
          {:reason :signature-vs-caller :qn qb :caller qa}

          :else nil)))))

(defn conflicts?
  "True when two spans cannot be held at once by different agents."
  [callers-fn a b]
  (some? (overlap callers-fn a b)))

(defn conflicts
  "Every held claim that blocks `wanted` for `slave-id`.

   `held` is a collection of maps carrying a span plus `:claim/slave`. A claim
   the requesting slave already holds is never a conflict with itself, which is
   what makes a re-claim idempotent and a retry cheap."
  [callers-fn held wanted slave-id]
  (let [w (span wanted)]
    (vec
     (for [h    held
           :let [hs  (span h)
                 why (when (not= (:claim/slave h) slave-id)
                       (overlap callers-fn hs w))]
           :when why]
       (merge {:file    (:span/file hs)
               :qn      (:span/qn hs)
               :mode    (:span/mode hs)
               :held-by (:claim/slave h)}
              why)))))

(defn explain
  "One line a refused agent can act on, naming the form and the reason."
  [{:keys [file qn held-by reason caller]}]
  (case reason
    :file-claim
    (str held-by " holds the whole of " file
         ", so no span inside it is free. That claim is either pre-span or the"
         " file is not indexed.")

    :same-form
    (str held-by " is already editing " qn " in " file ".")

    :signature-vs-caller
    (str held-by " is changing the signature of " qn ", which rewrites its"
         " caller " caller ". Wait for that claim, or take " qn " instead.")

    (str held-by " holds a conflicting claim on " (or qn file) ".")))
