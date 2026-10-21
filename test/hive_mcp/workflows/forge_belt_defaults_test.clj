(ns hive-mcp.workflows.forge-belt-defaults-test
  "Facets for the default :fb/* forge-belt extension points. Everything this
   namespace does goes through what register-forge-belt-defaults! installs in
   the extension registry: the dispatch predicates (a trifecta over generated
   FSM data) and single / continuous strikes driven end to end with the
   agent-ops and kanban-ops resources supplied as recording stubs (a golden).
   The fixture records which :fb/* keys were registered before and puts the
   previous values back (or removes keys that were absent)."
  (:require [clojure.test :refer [use-fixtures]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.workflows.forge-belt-defaults :as defaults]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [malli.generator :as mg]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private fb-keys
  [:fb/q1 :fb/q2 :fb/q3 :fb/q4 :fb/q5 :fb/q6 :fb/q7 :fb/q8
   :fb/h1 :fb/h2 :fb/h3 :fb/h3.5 :fb/h4 :fb/h5 :fb/h6 :fb/h7
   :fb/s1 :fb/s2 :fb/compile :fb/run :fb/strike :fb/cont])

(defn- defaults-fixture [f]
  (let [prior (into {} (keep (fn [k] (when-let [v (ext/get-extension k)] [k v]))) fb-keys)]
    (defaults/register-forge-belt-defaults!)
    (try (f)
         (finally
           (doseq [k fb-keys]
             (if-let [v (get prior k)] (ext/register! k v) (ext/deregister! k)))))))

(use-fixtures :each defaults-fixture)

;; -----------------------------------------------------------------------------
;; register-forge-belt-defaults! and the predicates it installs
;; -----------------------------------------------------------------------------

(def ^:private BeltData
  [:map
   [:quenched? {:optional true} :boolean]
   [:continuous? {:optional true} [:maybe :boolean]]
   [:context-gather? {:optional true} :boolean]
   [:survey-result {:optional true} [:map [:count [:int {:min 0 :max 3}]]]]
   [:spark-result {:optional true}
    [:map [:count [:int {:min 0 :max 2}]] [:failed {:optional true} [:vector {:max 2} [:enum "t1" "t2"]]]]]])

(defn- predicate-table
  "Apply every installed :fb/q* predicate to `data`, as the FSM dispatch does."
  [data]
  (into (sorted-map) (map (fn [k] [k (boolean ((ext/get-extension k) data))]))
        [:fb/q1 :fb/q2 :fb/q3 :fb/q4 :fb/q5 :fb/q6 :fb/q7 :fb/q8]))

;; The subject is the registration itself: after register-forge-belt-defaults!
;; (applied again on each draw, it is idempotent) every predicate answers the
;; documented rule for the generated data.
;; register-forge-belt-defaults! takes no argument, so a trifecta over it
;; would generate nothing. The property is over the FSM data instead: for
;; generated data, every predicate the registration installed answers its
;; documented rule (BeltData's generator is derived from the schema).
(defspec registered-predicates-follow-their-rules 200
  (prop/for-all [data (mg/generator BeltData)]
    (let [tasks (get-in data [:survey-result :count] 0)
          spawned (get-in data [:spark-result :count] 0)
          failed (seq (get-in data [:spark-result :failed]))]
      (= (predicate-table data)
         (sorted-map
          :fb/q1 (true? (:quenched? data))
          :fb/q2 (pos? tasks)
          :fb/q3 (zero? tasks)
          :fb/q4 (true? (:continuous? data))
          :fb/q5 (not (:continuous? data))
          :fb/q6 (boolean (and (zero? spawned) failed))
          :fb/q7 (true? (:context-gather? data))
          :fb/q8 (not (true? (:context-gather? data))))))))

;; -----------------------------------------------------------------------------
;; Strikes through the registered :fb/strike and :fb/cont
;; -----------------------------------------------------------------------------

(defn- stub-resources
  "Resources a strike reads, every op a recording stub. `tasks` is what the
   kanban survey answers on each call, consumed in order (the last repeats)."
  [calls tasks & {:keys [spawn-fails?]}]
  (let [surveys (atom tasks)]
    {:directory "/tmp/forge-facet"
     :context-gather? false
     :config {:max-slots 2 :presets ["ling"]}
     :scope-fn (fn [dir] (str "scope:" dir))
     :clock-fn (constantly "2026-02-07T12:00:00Z")
     :agent-ops {:kill-fn (fn [dir pid] (swap! calls conj [:kill dir pid]) {:smited [] :failed [] :count 1})
                 :spawn-fn (fn [opts]
                             (swap! calls conj [:spawn (mapv :id (:tasks opts)) (:max_slots opts)])
                             (if spawn-fails?
                               {:spawned [] :failed (mapv :id (:tasks opts)) :count 0}
                               {:spawned (mapv :id (:tasks opts)) :failed [] :count (count (:tasks opts))}))
                 :dispatch-fn (fn [_ _] nil)
                 :wait-ready-fn (fn [_] {:ready? true})}
     :kanban-ops {:list-fn (fn [dir]
                             (let [ts (first @surveys)]
                               (when (next @surveys) (swap! surveys rest))
                               (swap! calls conj [:survey dir (count ts)])
                               {:tasks ts :count (count ts)}))
                  :update-fn (fn [_] nil)}}))

(defn- strike [k resources]
  (select-keys ((ext/get-extension k) resources)
               [:strike-count :total-smited :total-sparked :success :continuous?]))

(golden/deftest-golden forge-belt-strikes-golden
  "test/golden/hive-mcp/workflows/forge-belt-defaults-strikes.edn"
  (let [run (fn [k tasks & opts]
              (let [calls (atom [])
                    out (strike k (apply stub-resources calls tasks opts))]
                {:out out :calls @calls}))]
    {:registered (vec (sort (filter (set fb-keys) (ext/registered-keys))))
     :single-no-tasks (run :fb/strike [[]])
     :single-two-tasks (run :fb/strike [[{:id "t1"} {:id "t2"}]])
     :continuous-drains (run :fb/cont [[{:id "t1"}] [{:id "t2"}] []])
     :continuous-all-fail (run :fb/cont [[{:id "t1"}]] :spawn-fails? true)
     :predicates {:empty (predicate-table {})
                  :busy (predicate-table {:survey-result {:count 2} :continuous? true
                                          :spark-result {:count 0 :failed ["t1"]}})}}))
