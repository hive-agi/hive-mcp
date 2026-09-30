(ns hive-mcp.hot.lifecycle.domain
  "Value objects of the reloadable-core lifecycle: what a host is, what can be
   done to it, and what can be seen of it.

   Core stratum (CPPB): malli schemas only, no behaviour. The model, the port,
   the hosts and the runner all speak this vocabulary and nothing wider.

   An Op names a kind and, for addon ops, the addon. The set of kinds is OPEN:
   a kind exists when hive-mcp.hot.lifecycle.model registers a `step` method
   for it, so the schema keeps :op/kind a keyword rather than closing an enum
   the next op would have to edit."
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

(def Op
  "One thing done to a host."
  [:map
   [:op/kind keyword?]
   [:op/addon {:optional true} AddonId]])

(def Script
  "Ops applied in order."
  [:vector Op])

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

(def Step
  "One op, what the host did with it, and what it looked like afterwards."
  [:map
   [:step/op Op]
   [:step/outcome Outcome]
   [:step/obs Observation]])

(def Trace
  "The baseline observation followed by one Step per op."
  [:map
   [:trace/baseline Observation]
   [:trace/steps [:vector Step]]])

(def HostSpec
  "What a host is made of, as far as the lifecycle can see: the core's own
   tools, each addon's advertised tools, which addons contribute hooks only
   (no surface to stub), and each addon's policy."
  [:map
   [:spec/core-tools [:set ToolName]]
   [:spec/addon-tools [:map-of AddonId [:set ToolName]]]
   [:spec/hook-only [:set AddonId]]
   [:spec/policy [:map-of AddonId Policy]]])

(def valid-trace? (m/validator Trace))
(def valid-spec? (m/validator HostSpec))
