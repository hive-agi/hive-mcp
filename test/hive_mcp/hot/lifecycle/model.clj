(ns hive-mcp.hot.lifecycle.model
  "What a reloadable hive-mcp core SHOULD do, as pure functions over values.

   Pure stratum (CPPB). A model state is {:spec HostSpec :phases {id Phase}
   :policy {id Policy}}: an addon is PRESENT when it has a phase, and :policy
   holds runtime policy changes over the declared ones. `step` is open over
   :op/kind: a new op is one `defmethod`, never an edit here (OCP). Invariants
   are an open registry too: a new property of the host is one
   `register-invariant!`.

   The semantics are the contract the probes on instance 2 established,
   stated as the behaviour after the reloadable-core fixes:
   - a core reload changes nothing a client can see;
   - an addon reload changes nothing a client can see, and needs a mounted
     addon;
   - evict releases an addon to stubs: its tools stay advertised, its phase
     goes :dormant. A pinned addon, a hook-only addon (no surface to stub)
     and an already dormant one are refused;
   - activate mounts a dormant addon; activating an active one is a no-op;
   - calling any tool of a dormant addon re-mounts it through its stub;
   - pin changes a present addon's policy. :lazy on a hook-only addon is
     refused unless forced (it could be evicted and never woken);
   - eject plugs an addon OUT: it loses its phase and every tool it
     advertised. An unknown addon is refused, and so is one a present addon
     depends on, unless cascaded: the dependents are then remounted (active)
     without it;
   - inject plugs in an addon of the catalog (the injectable ones, and any
     boot addon once ejected) as active; one already present is a no-op."
  (:require [clojure.set :as set]))

;; ── state ────────────────────────────────────────────────────────────────

(defn catalog
  "Every addon SPEC's host could hold, id -> the tools it advertises: the
   addons mounted at boot and the ones an inject can plug in."
  [spec]
  (merge (:spec/injectable spec) (:spec/addon-tools spec)))

(defn- tools-of [spec id] (get (catalog spec) id #{}))

(defn initial-state
  "Every addon of SPEC mounted, as a host looks right after boot. :policy
   holds runtime policy changes only; an addon without one runs its declared
   policy."
  [spec]
  {:spec   spec
   :phases (into (sorted-map) (map (fn [id] [id :active])) (keys (:spec/addon-tools spec)))
   :policy {}})

(defn advertised-tools
  "The tool table a correct host advertises: the core's tools and the tools
   of every PRESENT addon, dormant ones included (their stubs advertise them).
   An ejected addon is not present, so none of its tools are advertised."
  [{:keys [spec phases]}]
  (vec (sort (into (set (:spec/core-tools spec))
                   (mapcat #(tools-of spec %))
                   (keys phases)))))

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

(defn- policy-of
  "ID's policy now: the last runtime change, else the declared one."
  [{:keys [spec] :as state} id]
  (or (get-in state [:policy id]) (get-in spec [:spec/policy id]) :eager))

(defn- hook-only? [{:keys [spec]} id] (contains? (:spec/hook-only spec) id))

(defn- dependents
  "The present addons that depend on ID, directly or through each other."
  [{:keys [spec phases]} id]
  (let [deps    (:spec/deps spec {})
        present (keys phases)]
    (loop [reached #{id}]
      (let [more (into reached
                       (filter #(some reached (get deps %)))
                       present)]
        (if (= more reached)
          (disj reached id)
          (recur more))))))

(defn- evictable?
  "Why an addon may not be evicted, or nil when it may."
  [state id]
  (cond
    (not (known-addon? state id))                       :unknown
    (not= :active (phase-of state id))                  :not-active
    (= :pinned (policy-of state id))                    :pinned
    (hook-only? state id)                               :no-surface))

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

(defn- pinnable?
  "Why POLICY may not be set on ID, or nil when it may. A surface-less addon
   made :lazy could be evicted and never woken, so that needs FORCE?."
  [state id policy force?]
  (cond
    (not (known-addon? state id))                          :unknown
    (and (= :lazy policy) (not force?) (hook-only? state id)) :no-surface))

(defmethod step :pin [state {:op/keys [addon policy force?]}]
  (if (pinnable? state addon policy force?)
    [:refused state]
    [:applied (assoc-in state [:policy addon] policy)]))

(defn- ejectable?
  "Why ID may not be ejected, or nil when it may. A present dependent blocks
   it unless CASCADE?, which remounts the dependents without it."
  [state id cascade?]
  (cond
    (not (known-addon? state id))                          :unknown
    (and (seq (dependents state id)) (not cascade?))       :has-dependents))

(defmethod step :eject [state {:op/keys [addon cascade?]}]
  (if (ejectable? state addon cascade?)
    [:refused state]
    (let [remounted (dependents state addon)]
      [:applied (-> state
                    (update :phases dissoc addon)
                    (update :phases #(reduce (fn [ps id] (assoc ps id :active)) % remounted))
                    (update :policy dissoc addon))])))

(defmethod step :inject [{:keys [spec] :as state} {:op/keys [addon]}]
  (cond
    (known-addon? state addon)               [:noop state]
    (contains? (catalog spec) addon)         [:applied (assoc-in state [:phases addon] :active)]
    :else                                    [:refused state]))

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

(defn- own-tools
  "The tools only ID brings: none of the core's, none a PRESENT addon of
   PHASES also advertises. An absent addon's own tools must not be served."
  [spec phases id]
  (let [shared (into (set (:spec/core-tools spec))
                     (comp (remove #{id}) (mapcat #(tools-of spec %)))
                     (keys phases))]
    (set/difference (tools-of spec id) shared)))

(register-invariant! :addon-tools-advertised-in-every-phase
  (fn [spec {:obs/keys [tools phases]}]
    (let [want    (into #{} (mapcat #(tools-of spec %)) (keys phases))
          missing (set/difference want (set tools))]
      (when (seq missing) {:missing (sort missing)}))))

(register-invariant! :ejected-addon-tools-never-advertised
  (fn [spec {:obs/keys [tools phases]}]
    (let [absent (remove #(contains? phases %) (keys (catalog spec)))
          leaked (into (sorted-set)
                       (comp (mapcat #(own-tools spec phases %)) (filter (set tools)))
                       absent)]
      (when (seq leaked) {:leaked (vec leaked)}))))

(register-invariant! :no-duplicate-tools
  (fn [_spec {:obs/keys [tools]}]
    (let [dups (for [[t n] (frequencies tools) :when (> n 1)] t)]
      (when (seq dups) {:duplicates (sort dups)}))))

;; Since eject, an addon is present exactly when it has a phase, so "every
;; addon has a phase" covers the PRESENT addons: each phase must belong to an
;; addon the spec knows. The other direction, an absent addon still showing
;; tools, is :ejected-addon-tools-never-advertised.
(register-invariant! :every-addon-has-a-phase
  (fn [spec {:obs/keys [phases]}]
    (let [phantom (set/difference (set (keys phases)) (set (keys (catalog spec))))]
      (when (seq phantom) {:phantom (sort phantom)}))))
