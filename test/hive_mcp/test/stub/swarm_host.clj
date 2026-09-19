(ns hive-mcp.test.stub.swarm-host
  "Recording, scriptable stub for the swarm vessel: the `:dispatch`
   capability published under the `:swarm-host` key of
   hive-spi.editor.services.

   Swarm code emits hive-vessel op maps ({:op :swarm/status ...}) and calls
   (svc/invoke :swarm-host :dispatch op timeout-ms); a vessel addon
   (hive-emacs) lowers and executes them. This stub answers instead, so a
   handler runs with no editor and a test asserts on the ops it emitted.

   Contract (principle 20260919012710-55027e69):
     (dispatch op timeout-ms) => {:success :result :error :timed-out}
     {:op :swarm/available?}  => :result \"t\" when the swarm addon is loaded

   API:
     (->host respond)       RESPOND is (fn [op timeout-ms] envelope)
     (answering answers)    a RESPOND from {op-keyword answer}; an answer may
                            be a fn of the op map
     with-swarm-host        macro: publish a stub for a body, restore after
     (calls host)           recorded [op timeout-ms] pairs, oldest first
     (calls-of host k)      the recorded pairs whose :op is K
     (ops host)             the :op keywords, oldest first"
  (:require [hive-spi.editor.services :as svc]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def registry-key :vessel)

(def available
  "The :swarm/available? answer of a vessel whose swarm addon is loaded."
  {:success true :result "t" :timed-out false})

(def addon-unloaded
  "The :swarm/available? answer of a vessel whose swarm addon is not loaded."
  {:success true :result "nil" :timed-out false})

(defn ->host
  "A stub answering every dispatched op through RESPOND, (fn [op timeout-ms] envelope)."
  [respond]
  {:respond respond :calls (atom [])})

(defn- capability-fns
  [{:keys [respond calls]}]
  {:dispatch (fn [op timeout-ms]
               (swap! calls conj [op timeout-ms])
               (respond op timeout-ms))})

(defn calls
  "Recorded [op timeout-ms] pairs, oldest first."
  [host]
  @(:calls host))

(defn calls-of
  "The recorded [op timeout-ms] pairs whose :op is K."
  [host k]
  (filterv #(= k (:op (first %))) (calls host)))

(defn ops
  "The :op keywords dispatched, oldest first."
  [host]
  (mapv (comp :op first) (calls host)))

(defn install!
  "Publish HOST under the :swarm-host key, replacing whatever was there.
   Returns the previously registered capability map (nil when none)."
  [host]
  (let [prior (get (svc/registered) registry-key)]
    (svc/unregister-services! registry-key)
    (svc/register-services! registry-key (capability-fns host))
    prior))

(defn restore!
  "Put back PRIOR (a capability map from `install!`), or leave the key empty."
  [prior]
  (svc/unregister-services! registry-key)
  (when (seq prior) (svc/register-services! registry-key prior))
  nil)

(defn answering
  "A RESPOND fn from a map of op keyword -> answer. An answer may be a fn of
   the op map. :swarm/available? defaults to `available`; any other unlisted
   op answers a failure envelope."
  [answers]
  (fn [op _timeout-ms]
    (let [k (:op op)
          a (get answers k (if (= k :swarm/available?)
                             available
                             {:success false :error (str "unscripted op " k)}))]
      (if (fn? a) (a op) a))))

(defmacro with-swarm-host
  "Bind SYM to a stub built from RESPOND and published for BODY; the prior
   :swarm-host registration is restored afterwards."
  [[sym respond] & body]
  `(let [~sym   (->host ~respond)
         prior# (install! ~sym)]
     (try
       ~@body
       (finally
         (restore! prior#)))))
