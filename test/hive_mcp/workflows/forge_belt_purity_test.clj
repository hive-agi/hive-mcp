(ns hive-mcp.workflows.forge-belt-purity-test
  "Behavior checks for pure forge-belt dispatch and the injected strike clock."
  (:require [clojure.test.check.generators :as gen]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.workflows.forge-belt-defaults :as defaults]
            [hive-test.trifecta :refer [deftrifecta]]))

(defn dispatch-and-clock-case
  "Exercise registered predicates and spark with data and clock ports, not global time."
  [{:keys [enabled? timestamp]}]
  (defaults/register-forge-belt-defaults!)
  (try
    (let [q7 (ext/get-extension :fb/q7)
          q8 (ext/get-extension :fb/q8)
          start (ext/get-extension :fb/h1)
          spark (ext/get-extension :fb/h4)
          resources {:clock-fn (constantly timestamp)
                     :context-gather? enabled?
                     :agent-ops {:spawn-fn (constantly {:count 0})}
                     :kanban-ops {}
                     :config {}}
          data (start resources {:survey-result {:tasks []}
                                 :total-sparked 0})
          strike (spark resources data)]
      {:enabled (q7 data)
       :disabled (q8 data)
       :timestamp (:last-strike strike)
       :expected-enabled enabled?
       :expected-timestamp timestamp})
    (finally
      (doseq [key [:fb/q7 :fb/q8 :fb/h1 :fb/h4]]
        (ext/deregister! key)))))

(deftrifecta forge-belt-dispatch-clock
  hive-mcp.workflows.forge-belt-purity-test/dispatch-and-clock-case
  {:golden-path "test/golden/hive-mcp/forge-belt-dispatch-clock.edn"
   :cases {:enabled  {:enabled? true  :timestamp "2001-01-01T00:00:00Z"}
           :disabled {:enabled? false :timestamp "2040-12-31T23:59:59Z"}}
   :gen (gen/let [enabled? gen/boolean
                  timestamp (gen/elements ["2001-01-01T00:00:00Z"
                                           "2040-12-31T23:59:59Z"])]
          {:enabled? enabled? :timestamp timestamp})
   :pred (fn [{:keys [enabled disabled timestamp expected-enabled expected-timestamp]}]
           (and (= enabled expected-enabled)
                (= disabled (not expected-enabled))
                (= timestamp expected-timestamp)))
   :num-tests 50
   :mutations [["always-disabled" (fn [{:keys [timestamp]}]
                                    {:enabled false :disabled true
                                     :timestamp timestamp
                                     :expected-enabled true
                                     :expected-timestamp timestamp})]]})
