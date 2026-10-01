(ns hive-mcp.hot.lifecycle.domain
  "Value objects of the reloadable-core lifecycle: what a host is, what can be
   done to it, and what can be seen of it.

   Core stratum (CPPB): malli schemas only, no behaviour. The model, the port,
   the hosts and the runner all speak this vocabulary and nothing wider.

   An Op names a kind and, for addon ops, the addon, plus whatever its kind
   carries (:pin a :op/policy and :op/force?, :eject :op/cascade?). The set of
   kinds is OPEN: a kind exists when hive-mcp.hot.lifecycle.model registers a
   `step` method for it, and its shape is one `op-variant` method here. `Op`
   is the :multi over the variants registered at call time, so no enum has to
   be edited for the next op."
  (:require [malli.core :as m]))

(def AddonId
  "An addon's registered id, e.g. \"hive.rss\"."
  [:string {:min 1}])

(def ToolName
  "An advertised MCP tool name."
  [:string {:min 1}])

(def Phase
  "Where an addon is in its lifecycle. :active is mounted; :dormant is released
   to stubs that advertise its surface and re-mount it on first use."
  [:enum :active :dormant])

(def Policy
  "Lifecycle policy. :pinned refuses eviction unless forced."
  [:enum :eager :lazy :pinned])

(defmulti op-variant
  "The malli schema of an Op of KIND. OPEN: an op kind brings its own variant
   as one `defmethod`, and `Op` is the :multi over every registered variant,
   rebuilt per call so a variant registered later (or reloaded) is seen.
   :default is the shape every kind shares, used for a kind with no variant."
  identity)

(defmethod op-variant :default [_]
  [:map
   [:op/kind keyword?]
   [:op/addon {:optional true} AddonId]])

(defn- addon-op
  "The variant of a kind that names one addon and nothing else."
  [kind]
  [:map
   [:op/kind [:= kind]]
   [:op/addon AddonId]])

(defmethod op-variant :core-reload [_] [:map [:op/kind [:= :core-reload]]])
(defmethod op-variant :addon-reload [k] (addon-op k))
(defmethod op-variant :evict [k] (addon-op k))
(defmethod op-variant :activate [k] (addon-op k))
(defmethod op-variant :call [k] (addon-op k))
(defmethod op-variant :inject [k] (addon-op k))

(defmethod op-variant :pin [_]
  [:map
   [:op/kind [:= :pin]]
   [:op/addon AddonId]
   [:op/policy Policy]
   [:op/force? {:optional true} boolean?]])

(defmethod op-variant :eject [_]
  [:map
   [:op/kind [:= :eject]]
   [:op/addon AddonId]
   [:op/cascade? {:optional true} boolean?]])

;; unmount is eject on the railway (hive-addon plug-out!): same op shape.
(defmethod op-variant :unmount [_]
  [:map
   [:op/kind [:= :unmount]]
   [:op/addon AddonId]
   [:op/cascade? {:optional true} boolean?]])

(defn Op
  "One thing done to a host: the :multi over every registered `op-variant`,
   dispatching on :op/kind. A function, not a def, so it is never frozen at
   load (Capture-by-Var, 20260817195749-0d407e9c)."
  []
  (into [:multi {:dispatch :op/kind}]
        (concat (for [k (sort (remove #{:default} (keys (methods op-variant))))]
                  [k (op-variant k)])
                [[:malli.core/default (op-variant :default)]])))

(defn Script
  "Ops applied in order."
  []
  [:vector (Op)])

(def Outcome
  "What the host did with an op: changed something, deliberately declined it,
   or found nothing to do."
  [:enum :applied :refused :noop])

(def Observation
  "What can be seen of a host after an op. :obs/tools is the advertised tool
   table in advertised order, so a duplicate stays visible."
  [:map
   [:obs/tools [:vector ToolName]]
   [:obs/phases [:map-of AddonId Phase]]])

(defn Step
  "One op, what the host did with it, and what it looked like afterwards."
  []
  [:map
   [:step/op (Op)]
   [:step/outcome Outcome]
   [:step/obs Observation]])

(defn Trace
  "The baseline observation followed by one Step per op."
  []
  [:map
   [:trace/baseline Observation]
   [:trace/steps [:vector (Step)]]])

(def HostSpec
  "What a host is made of, as far as the lifecycle can see: the core's own
   tools, each mounted addon's advertised tools, which addons contribute hooks
   only (no surface to stub), and each addon's declared policy.

   Optional:
   - :spec/injectable  addons NOT mounted at boot that an inject can plug in,
                       with the tools they would advertise. An addon of
                       :spec/addon-tools is injectable again once ejected
                       (the host remembers where it came from).
   - :spec/deps        addon -> the addons it depends on. An addon with a
                       present dependent is not ejected without cascade.
   - :spec/inject-paths addon -> the checkout dir a live inject plugs it in
                       from. Boundary data: the model never reads it."
  [:map
   [:spec/core-tools [:set ToolName]]
   [:spec/addon-tools [:map-of AddonId [:set ToolName]]]
   [:spec/hook-only [:set AddonId]]
   [:spec/policy [:map-of AddonId Policy]]
   [:spec/injectable {:optional true} [:map-of AddonId [:set ToolName]]]
   [:spec/deps {:optional true} [:map-of AddonId [:set AddonId]]]
   [:spec/inject-paths {:optional true} [:map-of AddonId [:string {:min 1}]]]])

(defn valid-trace?
  "True when TRACE is a Trace over the op variants registered NOW."
  [trace]
  (m/validate (Trace) trace))
(def valid-spec? (m/validator HostSpec))
