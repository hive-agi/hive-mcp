(ns hive-mcp.tools.catchup.caps
  "Project-scoped display budgets; policy is supplied by an optional addon."
  (:require [hive-mcp.dns.result :refer [rescue]]))

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
