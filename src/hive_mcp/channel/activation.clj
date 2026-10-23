;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.channel.activation
  "Activation seam for the memory piggyback drain.

   `drain-ctx` builds the ctx that `memory-piggyback/drain!` hands to the
   ranker. Without a registered provider it is exactly `{:tokens <cues>}` —
   today's behaviour byte for byte. With one, the provider's `:pins` and
   `:floor-cap` are merged in and its `:tokens` are unioned onto the cues.

   DIP: this ns knows only the extension KEY and the shape of the answer. It
   never learns what a rule is, how eligibility is decided, or that core.logic
   exists — that lives behind `:memory/activation` (see
   `hive-knowledge.activation.addon`).

   Total: a provider that throws, hangs on a bad shape, or returns garbage
   degrades to the plain cue ctx. Activation must never be able to break a tool
   response."
  (:require [clojure.set :as set]
            [hive-mcp.extensions.registry :as ext]
            [taoensso.timbre :as log]
            [malli.core :as m]))

(def extension-key
  "Registry key an activation provider registers under. The provider is
   `(fn [activation-ctx] -> {:pins #{id} :tokens #{token} :floor-cap n})`.

   Deliberately NOT a place to add response content. An activation provider
   steers the memory DRAIN, and that is all this key means. Anything an addon
   wants to SAY in the response is a block emitter registered under `:block/*`
   and rendered by `hive-mcp.channel.blocks`, so the host never learns the name
   of what it is rendering."
  :memory/activation)

(defn- sane
  "Coerce a provider answer to the subset of keys the ranker accepts, dropping
   anything malformed. Returns nil when nothing usable survives."
  [answer]
  (when (map? answer)
    (let [pins (:pins answer)
          tokens (:tokens answer)
          cap (:floor-cap answer)]
      (cond-> nil
        (coll? pins) (assoc :pins (into #{} (filter string?) pins))
        (coll? tokens) (assoc :tokens (into #{} (filter string?) tokens))
        (and (integer? cap) (pos? cap)) (assoc :floor-cap cap)))))

(defn provider
  "The registered activation provider, or nil."
  []
  (ext/get-extension extension-key))

(m/=> provider [:=> [:cat] [:maybe ifn?]])

(def deadline-ms
  "Longest a tool response waits for the activation provider."
  200)

(defonce ^{:private true :doc "caller-id -> a provider answer that arrived after its deadline."}
  late-answers
  (atom {}))

(defonce ^{:private true :doc "caller-ids whose provider call is still running."}
  in-flight
  (atom #{}))

(defn- merge-answer
  "`base` drain ctx with a `sane` provider answer folded in."
  [base {:keys [pins tokens floor-cap]}]
  (cond-> base
    (seq tokens) (update :tokens set/union tokens)
    (seq pins)   (update :pins (fnil set/union #{}) pins)
    floor-cap    (assoc :floor-cap floor-cap)))

(defn- call-provider
  "The provider's sane answer, or nil when it throws or answers garbage."
  [f actx]
  (try
    (sane (f actx))
    (catch Throwable t
      (log/debug t "activation: provider failed; drain falls back to cues")
      nil)))

(defn drain-ctx
  "Drain ctx for one tool call: `{:tokens cues}` merged with whatever the
   activation provider contributes.

   `activation-ctx` is the read-only view a rule gets — tool name, the cue
   tokens harvested under `task-signal`'s allowlist, and the caller. Rules see
   the ALLOWLISTED cues only; raw tool args never reach them, so
   `task-signal/denied-arg-keys` governs activation input by construction.

   The provider gets `deadline-ms`. An answer that arrives later is kept for
   the same caller and folded into its NEXT drain, so a slow provider delays
   its pins by one call instead of delaying the response. While a caller's
   provider call is still running, no second one starts for that caller."
  [{:keys [tool-name cues caller-id] :as _activation-ctx}]
  (let [[old _] (swap-vals! late-answers dissoc caller-id)
        base    (cond-> {:tokens (or cues #{})}
                  (contains? old caller-id) (merge-answer (get old caller-id)))
        f       (provider)]
    (if-not f
      base
      (let [[running _] (swap-vals! in-flight conj caller-id)]
        (if (contains? running caller-id)
          base
          (let [actx {:tool-name tool-name :cues (or cues #{}) :caller-id caller-id}
                fut  (future
                       (try (call-provider f actx)
                            (finally (swap! in-flight disj caller-id))))
                ans  (deref fut deadline-ms ::late)]
            (if (= ::late ans)
              (do (future (when-let [a @fut] (swap! late-answers assoc caller-id a)))
                  base)
              (cond-> base ans (merge-answer ans)))))))))

(m/=> drain-ctx
      [:=> [:cat [:maybe [:map
                          [:tool-name {:optional true} :any]
                          [:cues {:optional true} [:maybe [:set :string]]]
                          [:caller-id {:optional true} :any]]]]
       [:map
        [:tokens [:set :string]]
        [:pins {:optional true} [:set :string]]
        [:floor-cap {:optional true} [:int {:min 1}]]]])
