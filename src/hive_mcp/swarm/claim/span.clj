(ns hive-mcp.swarm.claim.span
  "What a swarm agent claims, narrowed from a file to a span of one.

   Compat shim: moved to hive-agent.swarm.claim.span in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.claim.span" sym))

(defn- impl!
  "impl, after making sure the host adapters fill the swarm port slots.
   The adapters are resolved late: a static require would close a load
   cycle through the events adapter. A failure here (for instance a call
   made while that cycle is still loading) leaves the ports on their
   noops until a later call installs them."
  [sym]
  (try ((requiring-resolve 'hive-mcp.swarm.adapters.boot/ensure!))
       (catch Throwable _ nil))
  (impl sym))

;; Value schemas for the shim contracts. They transcribe the docstrings of
;; hive-agent.swarm.claim.span, where the behaviour lives. `Span` below is the
;; addon's own value and is nil without hive-agent, so the contracts cannot
;; name it; SpanShape is the same shape, owned here.

(def Mode
  "Claim modes, widest first."
  [:enum :file :signature :body])

(def SpanShape
  "A claimable region. `:span/qn` is nil exactly when `:span/mode` is :file."
  [:map
   [:span/file :string]
   [:span/mode Mode]
   [:span/qn {:optional true} [:maybe :string]]
   [:span/repo {:optional true} [:maybe :string]]
   [:span/rel {:optional true} :string]
   [:span/worktree {:optional true} [:maybe :string]]])

(def SpanLike
  "What every span fn coerces: a bare path is a file claim, a map a span. The
   map is LOOSE: `span` defaults a missing mode to :body (or :file when there
   is no qn) and reads a blank qn as none, so neither key is required."
  [:or :string
   [:map
    [:span/file :string]
    [:span/mode {:optional true} [:maybe :keyword]]
    [:span/qn {:optional true} [:maybe :string]]
    [:span/repo {:optional true} [:maybe :string]]
    [:span/rel {:optional true} :string]
    [:span/worktree {:optional true} [:maybe :string]]]])

(def Held
  "Held claims: spans (or paths) carrying their holder in :claim/slave."
  [:sequential [:or :string [:map
                             [:span/file :string]
                             [:span/mode {:optional true} Mode]
                             [:span/qn {:optional true} [:maybe :string]]
                             [:claim/slave {:optional true} [:maybe :string]]]]])

(def CallersFn
  "qn -> the qualified names that call it. nil disables the signature rule."
  [:=> [:cat :string] [:vector :string]])

(def Ctx
  "A span context: the injected path resolver and the anchoring directory."
  [:map
   [:resolver [:fn {:gen/elements [(constantly nil)]} ifn?]]
   [:cwd [:maybe :string]]])

(def Reason
  "Why two spans collide."
  [:enum :file-claim :same-form :signature-vs-caller :cross-worktree])

(def Overlap
  "`overlap`: the reason, the form it names, and for a cross-worktree overlap
   the underlying reason and :severity :warning."
  [:map
   [:reason Reason]
   [:qn [:maybe :string]]
   [:caller {:optional true} :string]
   [:underlying {:optional true} Reason]
   [:severity {:optional true} [:enum :warning]]])

(def Conflict
  "One collision against a held claim: where, who holds it, and why."
  [:map
   [:file :string]
   [:qn [:maybe :string]]
   [:mode Mode]
   [:held-by [:maybe :string]]
   [:reason Reason]])

(def Assessment
  "`assess`: blocking collisions and cross-worktree warnings."
  [:map
   [:conflicts [:vector Conflict]]
   [:warnings [:vector Conflict]]])

(def Span @(impl 'Span))

(defn conflicts
  {:arglists '([callers-fn held wanted slave-id])}
  [& args]
  (apply (impl! 'conflicts) args))

(m/=> conflicts [:function
                 [:=> [:cat [:maybe CallersFn] Held SpanLike [:maybe :string]] [:vector Conflict]]
                 [:=> [:cat [:maybe CallersFn] Held SpanLike [:maybe :string] [:maybe Ctx]]
                  [:vector Conflict]]])

(defn conflicts?
  {:arglists '([callers-fn a b])}
  [& args]
  (apply (impl! 'conflicts?) args))

(m/=> conflicts? [:=> [:cat [:maybe CallersFn] SpanLike SpanLike] :boolean])

(defn explain
  {:arglists '([{:keys [file qn held-by reason caller]}])}
  [& args]
  (apply (impl! 'explain) args))

;; every key is read optionally: an unknown reason still renders a line
(m/=> explain [:=> [:cat [:map
                          [:file {:optional true} [:maybe :string]]
                          [:qn {:optional true} [:maybe :string]]
                          [:held-by {:optional true} [:maybe :string]]
                          [:reason {:optional true} [:maybe :keyword]]
                          [:caller {:optional true} [:maybe :string]]
                          [:underlying {:optional true} [:maybe :keyword]]
                          [:worktree {:optional true} [:maybe :string]]]]
               :string])

(defn file-span
  {:arglists '([file])}
  [& args]
  (apply (impl! 'file-span) args))

;; the file is str'd, so a path of any printable type is accepted
(m/=> file-span [:=> [:cat [:or :string :symbol]] SpanShape])

(defn form-span
  {:arglists '([file qn] [file qn mode])}
  [& args]
  (apply (impl! 'form-span) args))

;; an unknown mode falls back to :body; a blank qn reads as nil
(m/=> form-span [:function
                 [:=> [:cat [:or :string :symbol] [:maybe [:or :string :symbol]]] SpanShape]
                 [:=> [:cat [:or :string :symbol] [:maybe [:or :string :symbol]] [:maybe :keyword]]
                  SpanShape]])

(defn key-of
  {:arglists '([s])}
  [& args]
  (apply (impl! 'key-of) args))

(m/=> key-of [:=> [:cat SpanLike] :string])

(def mode-rank @(impl 'mode-rank))

(def modes @(impl 'modes))

(defn overlap
  {:arglists '([callers-fn a b])}
  [& args]
  (apply (impl! 'overlap) args))

(m/=> overlap [:=> [:cat [:maybe CallersFn] SpanLike SpanLike] [:maybe Overlap]])

(defn span
  {:arglists '([x])}
  [& args]
  (apply (impl! 'span) args))

;; coercion: a map passes through, a string is a file claim, anything else is
;; str'd into one, so the loose input is genuinely open
(m/=> span [:function
            [:=> [:cat [:or SpanLike :symbol :keyword :int]] SpanShape]
            [:=> [:cat [:maybe Ctx] [:or SpanLike :symbol :keyword :int]] SpanShape]])

(defn widen
  {:arglists '([a b])}
  [& args]
  (apply (impl! 'widen) args))

;; a mode outside mode-rank ranks -1, so either argument may be anything
(m/=> widen [:=> [:cat [:maybe :keyword] [:maybe :keyword]] [:maybe :keyword]])

(defn- optional-impl
  "The hive-agent span var `sym` when the LOADED version defines it, else nil.

   `impl` cannot answer this: when the namespace is on the classpath it
   requiring-resolves, which yields nil for a var that version lacks, but when
   the namespace is absent it hands back a stand-in that throws on call. A
   feature probe must say 'no' in both cases, so it checks presence first."
  [sym]
  (when (delegate/available? "hive-agent.swarm.claim.span")
    (requiring-resolve (symbol "hive-agent.swarm.claim.span" (name sym)))))

(defn context-for
  "A span context anchored at `cwd`, or nil when the loaded hive-agent predates
   worktree identity.

   Forward compatible on purpose: a hive-agent that ships
   `default-context` canonicalizes both sides of a comparison (so one file seen
   through two worktrees meets, as a warning); one that does not keeps today's
   path comparison, and nil is how that is spelled downstream."
  [cwd]
  (when-let [f (optional-impl 'default-context)]
    (f cwd)))

(m/=> context-for [:=> [:cat [:maybe :string]] [:maybe Ctx]])

(defn assess
  "{:conflicts [...] :warnings [...]} for `wanted` against `held`.

   With a `ctx` and a hive-agent that has `assess`, cross-worktree overlaps come
   back as :warnings instead of blocking. Otherwise every collision is a
   conflict and :warnings is empty, which is exactly the pre-worktree
   behaviour."
  [callers-fn held wanted slave-id ctx]
  (if-let [f (when ctx (optional-impl 'assess))]
    (f callers-fn held wanted slave-id ctx)
    {:conflicts (conflicts callers-fn held wanted slave-id)
     :warnings  []}))

(m/=> assess [:=> [:cat [:maybe CallersFn] Held SpanLike [:maybe :string] [:maybe Ctx]] Assessment])
