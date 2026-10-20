(ns hive-mcp.tools.swarm.jvm.orphan
  "Orphan process detection using composable predicates.

   Compat shim: moved to hive-agent.swarm.tools.jvm.orphan in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.tools.jvm.orphan" sym))

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

;; Value schemas for the shim contracts. They transcribe the docstrings and
;; bodies of hive-agent.swarm.tools.jvm.orphan, where the behaviour lives.

(def ProtectedTypes
  "Process type NAMES (strings) that are never orphans. Read with
   `contains?`, so any set works; the tool passes a set of strings."
  [:set :string])

(def DetectorOpts
  "The `& {:keys [protected-types true-orphans-only min-age-minutes]}` tail:
   empty, alternating key/value pairs, or one trailing map."
  (let [entry [:alt
               [:cat [:= :protected-types] ProtectedTypes]
               [:cat [:= :true-orphans-only] [:maybe :boolean]]
               [:cat [:= :min-age-minutes] number?]]]
    [:alt
     [:cat]
     [:+ entry]
     [:cat [:map
            [:protected-types {:optional true} ProtectedTypes]
            [:true-orphans-only {:optional true} [:maybe :boolean]]
            [:min-age-minutes {:optional true} number?]]]]))

(def TypedProcess
  "A process whose :type `protected-type?` reads with `name`."
  [:map [:type [:or :keyword :string]]])

(def AgedProcess
  "A process whose elapsed time the age check parses. An unparseable or
   missing etime counts as 0 minutes."
  [:map [:etime {:optional true} [:maybe :string]]])

(def EnrichableProcess
  "What `enrich-with-parent-info` reads: the parent pid to look up."
  [:map [:ppid {:optional true} [:maybe :string]]])

(def ParentMap
  "pid -> its parent pid and command name, as `get-all-process-parents`
   answers it."
  [:map-of :string [:map [:ppid :string] [:comm :string]]])

(def EnrichedProcess
  "A process with what is known about its parent assoc'd on."
  [:map
   [:parent-alive :boolean]
   [:parent-comm [:maybe :string]]
   [:parent-is-claude :boolean]
   [:truly-orphaned :boolean]])

(def DetectableProcess
  "What a detector and `identify-orphan` read: a type, the enrichment flags
   (read for truthiness) and the elapsed time."
  [:map
   [:type [:or :keyword :string]]
   [:truly-orphaned {:optional true} [:maybe :boolean]]
   [:parent-is-claude {:optional true} [:maybe :boolean]]
   [:parent-comm {:optional true} [:maybe :string]]
   [:ppid {:optional true} [:maybe :string]]
   [:etime {:optional true} [:maybe :string]]])

(def IdentifiedProcess
  "A process with its orphan verdict, age and the reason for the verdict."
  [:map
   [:orphan [:maybe :boolean]]
   [:age-minutes [:int {:min 0}]]
   [:reason :string]])

(def Detector
  "The predicate `orphan-detector` builds: process -> orphan verdict. The
   verdict is the process's :truly-orphaned flag, so it may be nil."
  [:=> [:cat DetectableProcess] [:maybe :boolean]])

(defn age-orphan?
  {:arglists '([proc min-age-minutes])}
  [& args]
  (apply (impl! 'age-orphan?) args))

(m/=> age-orphan? [:=> [:cat AgedProcess number?] :boolean])

(defn enrich-with-parent-info
  {:arglists '([proc all-parents])}
  [& args]
  (apply (impl! 'enrich-with-parent-info) args))

(m/=> enrich-with-parent-info [:=> [:cat EnrichableProcess ParentMap] EnrichedProcess])

(defn identify-orphan
  {:arglists '([proc & {:keys [protected-types true-orphans-only min-age-minutes], :or {protected-types #{}, true-orphans-only true, min-age-minutes 30}}])}
  [& args]
  (apply (impl! 'identify-orphan) args))

(m/=> identify-orphan [:=> [:cat DetectableProcess DetectorOpts] IdentifiedProcess])

(defn identify-orphans
  {:arglists '([procs & {:keys [protected-types true-orphans-only min-age-minutes], :or {protected-types #{}, true-orphans-only true, min-age-minutes 30}}])}
  [& args]
  (apply (impl! 'identify-orphans) args))

(m/=> identify-orphans [:=> [:cat [:sequential DetectableProcess] DetectorOpts] [:sequential IdentifiedProcess]])

(defn orphan-detector
  {:arglists '([& {:keys [protected-types true-orphans-only min-age-minutes], :or {protected-types #{}, true-orphans-only true, min-age-minutes 30}}])}
  [& args]
  (apply (impl! 'orphan-detector) args))

(m/=> orphan-detector
      [:=> [:cat DetectorOpts]
       [:fn {:gen/fmap (fn [_] (fn [proc] (:truly-orphaned proc)))
             :gen/schema :nil}
        ifn?]])

(defn protected-type?
  {:arglists '([proc protected-types])}
  [& args]
  (apply (impl! 'protected-type?) args))

(m/=> protected-type? [:=> [:cat TypedProcess ProtectedTypes] :boolean])

(defn truly-orphaned?
  {:arglists '([proc])}
  [& args]
  (apply (impl! 'truly-orphaned?) args))

;; Answers the :truly-orphaned flag as stored: nil when the process was never
;; enriched.
(m/=> truly-orphaned?
      [:=> [:cat [:map [:truly-orphaned {:optional true} [:maybe :boolean]]]] [:maybe :boolean]])
