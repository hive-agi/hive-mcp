(ns hive-mcp.hot.lifecycle.model
  "What a reloadable hive-mcp core SHOULD do, as pure functions over values.

   Pure stratum (CPPB). A model state is {:spec HostSpec :phases {id Phase}}.
   `step` is open over :op/kind: a new op is one `defmethod`, never an edit
   here (OCP). Invariants are an open registry too: a new property of the
   host is one `register-invariant!`.

   The semantics are the contract the probes on instance 2 established,
   stated as the behaviour after the reloadable-core fixes:
   - a core reload changes nothing a client can see;
   - an addon reload changes nothing a client can see, and needs a mounted
     addon;
   - evict releases an addon to stubs: its tools stay advertised, its phase
     goes :dormant. A pinned addon, a hook-only addon (no surface to stub)
     and an already dormant one are refused;
   - activate mounts a dormant addon; activating an active one is a no-op;
   - calling any tool of a dormant addon re-mounts it through its stub."
  (:require [clojure.set :as set]))

;; ── state ────────────────────────────────────────────────────────────────

(defn initial-state
  "Every addon of SPEC mounted, as a host looks right after boot."
  [spec]
  {:spec   spec
   :phases (into (sorted-map) (map (fn [id] [id :active])) (keys (:spec/addon-tools spec)))})

(defn advertised-tools
  "The tool table a correct host advertises: the core's tools and every
   addon's tools, dormant ones included (their stubs advertise them)."
  [{:keys [spec]}]
  (vec (sort (into (set (:spec/core-tools spec))
                   (mapcat val)
                   (:spec/addon-tools spec)))))

(defn observation
  "The Observation of a model state."
  [state]
  {:obs/tools  (advertised-tools state)
   :obs/phases (into (sorted-map) (:phases state))})

(defn- phase-of [state id] (get-in state [:phases id]))

(defn- known-addon? [state id] (contains? (:phases state) id))

;; ── step: open over :op/kind ─────────────────────────────────────────────

(defmulti step
  "[state op] -> [outcome state']. Dispatches on :op/kind."
  (fn [_state op] (:op/kind op)))

(defmethod step :default [state op]
  (throw (ex-info "no model for op kind; register a step method"
                  {:op op :known (sort (keys (methods step)))})))

(defmethod step :core-reload [state _op]
  [:applied state])

(defmethod step :addon-reload [state {:op/keys [addon]}]
  (if (= :active (phase-of state addon))
    [:applied state]
    [:refused state]))

(defn- evictable?
  "Why an addon may not be evicted, or nil when it may."
  [{:keys [spec] :as state} id]
  (cond
    (not (known-addon? state id))                       :unknown
    (not= :active (phase-of state id))                  :not-active
    (= :pinned (get-in spec [:spec/policy id]))         :pinned
    (contains? (:spec/hook-only spec) id)               :no-surface))

(defmethod step :evict [state {:op/keys [addon]}]
  (if (evictable? state addon)
    [:refused state]
    [:applied (assoc-in state [:phases addon] :dormant)]))

(defmethod step :activate [state {:op/keys [addon]}]
  (case (phase-of state addon)
    :dormant [:applied (assoc-in state [:phases addon] :active)]
    :active  [:noop state]
    [:refused state]))

(defmethod step :call [state {:op/keys [addon]}]
  (case (phase-of state addon)
    :dormant [:applied (assoc-in state [:phases addon] :active)]
    :active  [:noop state]
    [:refused state]))

(defn run
  "Fold SCRIPT over the model of SPEC. Returns a Trace."
  [spec script]
  (let [s0 (initial-state spec)]
    (loop [state s0, [op & more :as ops] script, steps []]
      (if (empty? ops)
        {:trace/baseline (observation s0) :trace/steps steps}
        (let [[outcome state'] (step state op)]
          (recur state' more (conj steps {:step/op op
                                          :step/outcome outcome
                                          :step/obs (observation state')})))))))

;; ── invariants: an open registry of named properties ─────────────────────

(defonce ^:private invariants (atom (sorted-map)))

(defn register-invariant!
  "Register invariant K: (fn [spec obs] -> nil | violation-map)."
  [k f]
  (swap! invariants assoc k f)
  k)

(defn registered-invariants [] (keys @invariants))

(defn violations
  "Every invariant violation of every observation in TRACE, as
   [{:invariant k :at step-index-or-:baseline :violation v}]."
  [spec {:trace/keys [baseline steps]}]
  (let [obs (cons [:baseline baseline] (map-indexed (fn [i s] [i (:step/obs s)]) steps))]
    (vec (for [[at o] obs
               [k f] @invariants
               :let [v (f spec o)]
               :when v]
           {:invariant k :at at :violation v}))))

(register-invariant! :core-tools-advertised
  (fn [spec {:obs/keys [tools]}]
    (let [missing (set/difference (:spec/core-tools spec) (set tools))]
      (when (seq missing) {:missing (sort missing)}))))

(register-invariant! :addon-tools-advertised-in-every-phase
  (fn [spec {:obs/keys [tools]}]
    (let [want    (into #{} (mapcat val) (:spec/addon-tools spec))
          missing (set/difference want (set tools))]
      (when (seq missing) {:missing (sort missing)}))))

(register-invariant! :no-duplicate-tools
  (fn [_spec {:obs/keys [tools]}]
    (let [dups (for [[t n] (frequencies tools) :when (> n 1)] t)]
      (when (seq dups) {:duplicates (sort dups)}))))

(register-invariant! :every-addon-has-a-phase
  (fn [spec {:obs/keys [phases]}]
    (let [missing (set/difference (set (keys (:spec/addon-tools spec))) (set (keys phases)))]
      (when (seq missing) {:missing (sort missing)}))))
