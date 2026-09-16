(ns hive-mcp.channel.broadcast-policy
  "Is THIS broadcast justified? A pure decision, no I/O, no state.

   A broadcast multiplies one agent's sentence by the number of readers. In a
   wave of N members that is N copies of the same tokens, in N context windows,
   for a fact that usually concerned one peer. So a broadcast here is not a
   delivery mode a caller picks freely — it is an exception that has to be
   argued for, and the argument is a `:broadcast-reason` drawn from a closed
   set.

   ## Refusal DOWNGRADES, it never drops

   A refused broadcast is re-routed by its ordinary rules (directed :to if it
   has one, else its spawner). Dropping would destroy information because the
   sender phrased its address badly, which is a worse failure than the cost the
   policy exists to avoid. The verdict says what happened so a caller can tell
   the sender.

   ## Why the reason set is CLOSED

   An open set of reasons is a free-text field, and a free-text justification
   gate is satisfied by typing anything. The four admitted reasons are the four
   cases where a fact genuinely concerns every reader; anything outside them
   concerns somebody in particular, which is what `:to` is for.

   ## Two gates, because a closed set does not bound REPETITION

   The reason gate stops a caller inventing a justification. It does not stop
   one repeating an admissible one, and `:shared-discovery` five hundred times
   is five hundred admissible broadcasts. So a caller that tracks volume may
   pass `:recent-broadcasts`, and `decide` refuses on count alone once the
   budget is gone — see hive-mcp.channel.broadcast-ledger, which keeps that
   count per project over a sliding window.

   The gates are independent on purpose. Volume cannot admit what the reason
   set refused, and an admissible reason does not buy unlimited repetition.
   The one message exempt from volume is a halt, for the reason spelled out on
   `volume-exempt-reasons`."
  (:require [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def admissible-reasons
  "The closed set of arguments that justify reaching every reader.

   :halt                  stop work — a fact that makes every peer's current
                          work wrong or unsafe to continue.
   :membership            the roster changed. Peers address each other by id,
                          so who exists is the one fact every peer needs.
   :shared-discovery      a finding that invalidates an assumption the whole
                          wave shares (the build is broken, the API moved).
   :coordinator-directive the coordinator addressing its swarm. The one case
                          where the sender is above the readers rather than
                          beside them."
  #{:halt :membership :shared-discovery :coordinator-directive})

(defn- as-reason
  "Coerce a reason to a keyword, tolerating the string spelling the MCP
   transport produces. Anything unrecognisable stays nil."
  [x]
  (cond
    (keyword? x) x
    (and (string? x) (not (str/blank? x))) (keyword (str/replace x #"^:" ""))
    :else nil))

(defn admissible-reason?
  "Is `x` one of the arguments that justify a broadcast?"
  [x]
  (contains? admissible-reasons (as-reason x)))

(def volume-exempt-reasons
  "Arguments the VOLUME gate never refuses, however many went before.

   A halt is the message that makes every peer's current work wrong or unsafe
   to continue. Refusing it to save tokens spends the saving on work done
   against a premise already known to be false, which costs more than the
   broadcast did. The reason gate still applies: an inadmissible reason is
   still refused, so this exempts a halt from volume, not from argument.

   Deliberately NOT exempt: :shared-discovery and :membership, which are the
   repeatable ones, and the two a chatty swarm reaches for. Exempting them
   would leave the gate policing nothing."
  #{:halt})

(defn volume-exempt?
  "Is this an argument the volume gate must not refuse?"
  [x]
  (contains? volume-exempt-reasons (as-reason x)))

(def default-budget
  "Broadcasts admitted per window before the policy starts refusing on volume
   alone. Chosen to be small on purpose: a swarm that legitimately needs to tell
   everyone something more than a handful of times in one window is a swarm
   whose members should be addressing each other directly.

   A caller that does not track volume passes no `:recent-broadcasts` and this
   never bites — the reason gate still applies."
  8)

(defn decide
  "Decide how `msg` should be delivered.

   Returns {:verdict :direct | :spawner | :broadcast
            :reason  <keyword>       ; the admitted argument, when broadcasting
            :refused <keyword>}      ; why a requested broadcast was refused

   Verdicts:
     :broadcast  the message reaches every reader.
     :direct     it reaches the agent named by :to.
     :spawner    it reaches the agent that spawned the sender (the old default).

   `opts` may carry {:recent-broadcasts n :budget n} to apply the volume gate.
   Without them only the reason gate applies, which is the pure-function case
   every test exercises. Deciding WHETHER to meter a sender is the caller's
   judgement, not this namespace's: identity lives at the call site, and a
   coordinator addressing its own swarm is not the chatter the budget exists
   to stop. What is intrinsic to the MESSAGE is decided here, which is why the
   halt exemption is, and the coordinator one is not."
  ([msg] (decide msg nil))
  ([{:keys [broadcast? broadcast-reason to]} {:keys [recent-broadcasts budget]}]
   (let [budget (or budget default-budget)
         fallback (if to {:verdict :direct} {:verdict :spawner})]
     (cond
       (not broadcast?)
       fallback

       ;; A message that names a recipient AND asks for a broadcast is
       ;; self-contradictory. The specific address is the one the sender
       ;; actually thought about, so it wins.
       (some? to)
       (assoc fallback :refused :addressed-to-a-peer)

       (not (admissible-reason? broadcast-reason))
       (assoc fallback :refused (if (nil? broadcast-reason)
                                  :no-reason-given
                                  :reason-not-admissible))

       (and (number? recent-broadcasts)
            (>= recent-broadcasts budget)
            (not (volume-exempt? broadcast-reason)))
       (assoc fallback :refused :budget-exhausted)

       :else
       {:verdict :broadcast :reason (as-reason broadcast-reason)}))))

(defn admitted?
  "Did the policy let this message reach every reader?"
  [msg]
  (= :broadcast (:verdict (decide msg))))

(defn apply-policy
  "Rewrite `msg` so its routing fields say what the policy decided.

   A refused broadcast loses `:broadcast?` and gains `:broadcast-refused`, which
   is what a caller reports back to the sender. The message itself survives —
   see the namespace docstring on why refusal downgrades rather than drops."
  ([msg] (apply-policy msg nil))
  ([msg opts]
   (let [{:keys [verdict refused reason]} (decide msg opts)]
     (case verdict
       :broadcast (assoc msg :broadcast? true :broadcast-reason reason)
       (cond-> (dissoc msg :broadcast? :broadcast-reason)
         refused (assoc :broadcast-refused refused))))))
