;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.channel.row-transforms
  "Registered transforms over the HIVEMIND piggyback rows, as an OPEN set.

   ## Why this exists

   What a reader is shown of the hivemind is policy, and policy does not
   belong in the delivery loop. hive-mcp.channel.piggyback/get-messages owns
   the MECHANISM (cursors, project and context scoping, audience routing);
   everything that reshapes the rows a reader already earned is a transform,
   so a new policy is an extension plus a config entry, not a commit to the
   delivery loop.

   ## Core transforms

   Core's own default policy is a CORE transform, installed with
   `register-core-transform!`. Core transforms live here, not in the
   extension registry, so nothing that clears or rewrites that registry (an
   :hive/extensions halt, an extension that registers or deregisters the same
   key) can replace or remove them. They run FIRST, in installation order,
   before any registered transform, whatever their keys sort as. Their keys
   are RESERVED: an extension registered under a core key is not a
   transform and never runs.

   ## The contract for registered transforms

   Any other extension key whose namespace is \"hivemind.rows\" is a
   transform:

       (ext/register! :hivemind.rows/50-mine (fn [rows read-ctx] -> rows))

   - Transforms run in SORTED key order, after the core transforms; each sees
     the previous one's output.
   - `rows` is a vector of formatted piggyback rows
     {:a str :e str :m str ?:t ?:ctx ?:ref ?:n ?:deliberate? true :ts long}.
   - `read-ctx` is {:reader agent-id :project-id pid :session-id sid
     :context-id cid}.
   - An output is accepted only when it is a vector, every row is a map with
     string :a and :e, and it has no MORE rows than its input. Anything else
     is skipped: the input passes on unchanged and later transforms still
     run. Anything a transform throws is the same as a skip (an
     AssertionError or a NoClassDefFoundError from an extension's classpath
     included), except a VirtualMachineError such as OutOfMemoryError, which
     is not ours to contain and propagates.
   - A thrown step is logged at warn once per key when it starts failing, and
     at debug while it keeps failing; a contract-shape skip is logged at
     debug.
   - :ts and :deliberate? are visible to transforms and removed by the caller
     after the chain, so neither ever reaches the wire. A transform that wants
     to point at raw history writes its own :since <ts> (the earliest
     timestamp its row hides); :since is left alone.
   - Cursors are computed before the chain and never depend on it, and the
     coordinator's peer-traffic rows are appended after it and never pass
     through it.

   ## Pure and fast

   The chain runs synchronously inside every hivemind read, in the reader's
   thread, between reading the cursors and advancing them. Core does not
   time-box it: a transform must be PURE (no I/O, no blocking, no network or
   model call) and FAST. Slow work belongs off the read path: compute it in
   the background and deliver it later through :block/*.

   ## Rows are not a content channel

   A transform may drop, merge or rewrite the rows it is handed; it may not
   grow them. Content that is not a reshaped row (for instance something an
   extension finished computing after the row it concerns was already
   delivered) goes through the :block/* emitter seam, hive-mcp.channel.blocks,
   where the per-response budget applies to it."
  (:require [hive-mcp.extensions.registry :as ext]
            [taoensso.timbre :as log]))

(def key-namespace
  "Extension keys in this namespace are row transforms."
  "hivemind.rows")

(defonce ^{:private true
           :doc "Core transforms as an ordered vector of [key f]. Deliberately
                 NOT the extension registry: an :hive/extensions halt clears
                 that registry, and an extension may write or delete any key in
                 it, while core's default has to survive both."}
  core-steps
  (atom []))

(defn register-core-transform!
  "Install `f` as a core transform under `k`, a \"hivemind.rows\" key that
   becomes reserved. Idempotent: installing a key again replaces its fn in
   place and keeps its position. Core transforms run first, in installation
   order."
  [k f]
  (swap! core-steps
         (fn [steps]
           (if (some #(= k (first %)) steps)
             (mapv (fn [[sk sf]] (if (= sk k) [k f] [sk sf])) steps)
             (conj steps [k f]))))
  k)

(defn core-transform-keys
  "Reserved keys of the core transforms, in the order they run."
  []
  (mapv first @core-steps))

(defn transform-keys
  "Registered (non-core) row-transform keys, sorted, so the chain's order is
   deterministic rather than a function of registration order. A key a core
   transform reserved is never listed, even when an extension is registered
   under it."
  []
  (let [reserved (set (core-transform-keys))]
    (->> (ext/registered-keys)
         (filter #(= key-namespace (namespace %)))
         (remove reserved)
         sort
         vec)))

(defn- valid-row?
  [row]
  (and (map? row) (string? (:a row)) (string? (:e row))))

(defn- accepted?
  "Does `out` honour the contract against the rows it was handed?"
  [in out]
  (and (vector? out)
       (<= (count out) (count in))
       (every? valid-row? out)))

(defonce ^{:private true
           :doc "Keys whose step threw on its last run. Rate-limits the warning
                 to one per key per change of state; a key leaves the set on
                 its next clean run."}
  failing
  (atom #{}))

(defn- rethrow-fatal!
  "A VirtualMachineError (out of memory, an internal VM error) is not ours to
   contain. Every other Throwable is a skip."
  [^Throwable t]
  (when (instance? VirtualMachineError t)
    (throw t)))

(defn- skip
  "Log why the step under `k` was skipped and pass `rows` on. A step that
   threw is logged at warn the first time and at debug while it keeps
   failing. An InterruptedException is not ours to consume: the interrupt
   flag it cleared is set again, AFTER logging (a log appender may itself
   block and consume the flag), so the reader's thread still sees it."
  [rows k why ^Throwable t]
  (let [[old _] (swap-vals! failing conj k)]
    (if (contains? old k)
      (log/debug t "row-transforms:" k why "; skipped")
      (log/warn t "row-transforms:" k why "; skipped")))
  (when (instance? InterruptedException t)
    (.interrupt (Thread/currentThread)))
  rows)

(defn- apply-one
  "Run step `f` (under key `k`) over `rows`, or pass `rows` on untouched when
   it is missing, throws, or answers outside the contract. Only a
   VirtualMachineError propagates."
  [rows k f read-ctx]
  (if-not f
    rows
    (try
      (let [out (f rows read-ctx)]
        (when (contains? @failing k) (swap! failing disj k))
        (if (accepted? rows out)
          out
          (do (log/debug "row-transforms:" k "answered outside the contract; skipped")
              rows)))
      (catch Throwable t
        (rethrow-fatal! t)
        (skip rows k "threw" t)))))

(defn- run-steps
  "Reduce `rows` through the steps `steps-fn` answers, a seq of [k f],
   isolating each step. `steps-fn` is called inside the guard, so if listing
   the steps or walking them fails, answer `fallback`."
  [rows steps-fn read-ctx fallback]
  (try
    (reduce (fn [acc [k f]] (apply-one acc k f read-ctx)) rows (steps-fn))
    (catch Throwable t
      (rethrow-fatal! t)
      (skip fallback "chain" "failed" t))))

(defn apply-transforms
  "Thread `rows` through the core transforms, then through every registered
   transform in sorted key order. Each step is isolated: one that fails the
   contract or throws costs only its own step. If enumerating the registered
   transforms fails, the core transforms' output is still delivered. An
   empty read runs no transform. Only a VirtualMachineError propagates."
  [rows read-ctx]
  (let [rows (vec rows)]
    (if (empty? rows)
      rows
      (let [core-out (run-steps rows (fn [] @core-steps) read-ctx rows)]
        (run-steps core-out
                   (fn [] (mapv (juxt identity ext/get-extension) (transform-keys)))
                   read-ctx
                   core-out)))))
