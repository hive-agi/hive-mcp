(ns hive-mcp.tools.catchup.caps
  "Project-scoped display budgets; policy is supplied by an optional addon."
  (:require [hive-mcp.dns.result :refer [rescue]]
            [hive-mcp.tools.catchup.shapes :as shapes]
            [malli.core :as m]))

(def default-caps
  "Literal display budgets used when the :catchup/caps addon is absent."
  {:axioms 100
   :axiom-candidates 25
   :priority-principles 50
   :principles 50
   :priority-conventions 50
   :sessions 25
   :recent-wraps 10
   :decisions 50
   :conventions 50
   :snippets 20
   :expiring 20})

(defn resolve-caps
  "Call the optional project-id-keyed caps provider. Invalid or missing values
   fall back independently to the literal budgets; caller profile overrides
   are applied last. A provider failure never breaks catchup."
  [provider project-id profile]
  (let [provided (when provider (rescue nil (provider project-id)))
        profile-caps (:caps profile)
        valid? (fn [n] (and (integer? n) (<= 0 n 1000)))]
    (reduce-kv (fn [result k fallback]
                 (let [project-cap (get provided k)
                       profile-cap (get profile-caps k)]
                   (assoc result k (cond
                                     (valid? profile-cap) profile-cap
                                     (valid? project-cap) project-cap
                                     :else fallback))))
               {} default-caps)))

;; =============================================================================
;; Contracts
;; =============================================================================

(def Cap
  "One display budget: an entry count the bucket may show."
  [:int {:min 0 :max 1000}])

(def Caps
  "`resolve-caps`: a budget for every bucket `default-caps` names, no more."
  (into [:map {:closed true}] (map (fn [k] [k Cap])) (keys default-caps)))

(def CapsProvider
  "The optional :catchup/caps provider, (fn [project-id]) -> {bucket n}, or nil.
   The generator mixes valid, out-of-range and throwing providers."
  [:maybe [:fn {:gen/elements [(constantly {:axioms 5 :decisions 0})
                               (constantly {:axioms 2000 :sessions -1 :snippets 7})
                               (constantly nil)
                               (fn [_] (throw (ex-info "provider down" {})))]}
           ifn?]])

(def CallerProfile
  "A caller bundle profile; only :caps is read. nil means no profile."
  [:maybe [:map [:caps {:optional true}
                 [:maybe [:map-of {:gen/schema [:map-of [:enum :axioms :sessions :decisions :bogus]
                                                [:int {:min -5 :max 1005}]]}
                          :any :any]]]]])

(m/=> resolve-caps [:=> [:cat CapsProvider shapes/ProjectId CallerProfile] Caps])
