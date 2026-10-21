(ns hive-mcp.workflows.wrap-session-contract-test
  "Contracts and hive-test facets for the wrap-session workflow.

   adopted-entry-ids and the crystal predicates are pure and get schema
   trifectas; every handler is driven through stub resources under
   instrumentation, one arm at a time (present / absent / throwing), and the
   whole FSM gets a golden over its terminal summary."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.workflows.schema :as wfs]
            [hive-mcp.workflows.wrap-session :as wrap]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  (set (for [s '[harvested? crystallized? crystal-error? handle-start handle-adopt
                 handle-gather handle-crystallize adopted-entry-ids handle-kg-edges
                 handle-notify handle-evict handle-end compile-wrap run-wrap run-wrap-session]]
         (ns-resolve 'hive-mcp.workflows.wrap-session s))))

(use-fixtures :each
  (fn [f]
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try (f) (finally (mi/unstrument!)))))

(defn- boom [& _] (throw (ex-info "boom" {})))

;; =============================================================================
;; Pure: adopted-entry-ids
;; =============================================================================

(def ^:private Id [:enum "e1" "e2" "e3"])

(def ^:private Adopted
  [:vector {:gen/max 4}
   [:map
    [:wrap-queue/created-ids {:optional true} [:maybe [:vector {:gen/max 3} [:maybe Id]]]]
    [:created-ids {:optional true} [:maybe [:vector {:gen/max 3} [:maybe Id]]]]]])

(deftrifecta-from-schema adopted-entry-ids-flatten
  hive-mcp.workflows.wrap-session/adopted-entry-ids
  {:in Adopted
   :out [:vector Id]
   :rel (fn [adopted out]
          (let [expect (->> adopted
                            (mapcat #(or (:wrap-queue/created-ids %) (:created-ids %)))
                            (remove nil?))]
            (and (= out (vec (distinct expect)))
                 (apply distinct? nil out))))
   :golden-path "test/golden/workflows/wrap-adopted-entry-ids.edn"
   :num-tests 200})

;; =============================================================================
;; Pure: the crystal predicates partition fatal vs continue
;; =============================================================================

(def ^:private CrystalData
  [:map
   [:crystal-result {:optional true}
    [:maybe [:map
             [:error {:optional true} [:maybe [:enum "x"]]]
             [:degraded {:optional true} [:maybe :boolean]]
             [:skipped {:optional true} [:maybe :boolean]]]]]])

(deftrifecta-from-schema crystal-error-is-fatal-only
  hive-mcp.workflows.wrap-session/crystal-error?
  {:in CrystalData
   :out :boolean
   :rel (fn [d out]
          (let [cr (:crystal-result d)]
            (= out (boolean (and (:error cr) (not (:degraded cr)) (not (:skipped cr)))))))
   :num-tests 200})

(deftest crystal-predicates
  (doseq [d (m/children (m/schema [:enum
                                   {} {:crystal-result {}} {:crystal-result {:error "x"}}
                                   {:crystal-result {:error "x" :degraded true}}
                                   {:crystal-result {:error "x" :skipped true}}]))]
    (when (:crystal-result d)
      (is (not= (boolean (wrap/crystallized? d)) (wrap/crystal-error? d))
          (str "crystallized? and crystal-error? partition " (pr-str d)))))
  (is (not (wrap/crystallized? {})) "no result: neither")
  (is (false? (wrap/crystal-error? {})))
  (is (true? (wrap/harvested? {:harvested {}})))
  (is (false? (wrap/harvested? nil))))

;; =============================================================================
;; Handlers, arm by arm
;; =============================================================================

(deftest start-and-adopt
  (let [out (wrap/handle-start {:scope-fn (constantly "p") :session-ref-fn identity
                                :parent-of {"c" "p"}}
                               {:agent-id "a" :directory "/d"})]
    (is (= {:agent-id "a" :directory "/d" :project-id "p" :parent-of {"c" "p"} :error nil
            :session-ref {:agent-id "a" :project-id "p"}}
           out)))
  (let [ref {:session/kind :coordinator}]
    (is (= [] (:adopted (wrap/handle-adopt {} {:session-ref ref}))) "no adopt-fn")
    (is (= [] (:adopted (wrap/handle-adopt {:adopt-fn (constantly [1])} {:session-ref {:session/kind :ling}})))
        "a ling adopts nothing")
    (is (= [1] (:adopted (wrap/handle-adopt {:adopt-fn (constantly [1])} {:session-ref ref}))))
    (is (= {:session-ref ref :adopted [] :adopt-degraded true :adopt-error "boom"}
           (wrap/handle-adopt {:adopt-fn boom} {:session-ref ref})))))

(deftest gather-and-crystallize
  (is (= {:n 1} (:harvested (wrap/handle-gather {:harvest-fn (constantly {:n 1})} {}))))
  (is (true? (get-in (wrap/handle-gather {:harvest-fn boom} {}) [:harvested :degraded])))
  (is (= [{:summary-id "s"} ["e"]]
         ((juxt :crystal-result :source-ids)
          (wrap/handle-crystallize {:crystallize-fn (constantly {:summary-id "s"})
                                    :source-ids-fn (constantly ["e"])} {}))))
  (let [out (wrap/handle-crystallize {:crystallize-fn boom} {})]
    (is (= {:skipped true :degraded true :error "boom" :stats {}} (:crystal-result out)))
    (is (not (wrap/crystal-error? out)) "a degraded crystallize is not fatal")))

(deftest kg-edges-legs-degrade-independently
  (let [data {:crystal-result {:summary-id "s"} :source-ids ["e1"]
              :adopted [{:created-ids ["c1"]}]}]
    (is (= [{:created-count 0 :skipped true} {:created-count 0 :skipped true}]
           ((juxt :kg-result :contains-result) (wrap/handle-kg-edges {} data))))
    (let [out (wrap/handle-kg-edges {:kg-edge-fn (fn [s ids _ _] {:created-count (count ids) :s s})
                                     :contains-edge-fn boom}
                                    data)]
      (is (= {:created-count 1 :s "s"} (:kg-result out)) "derived-from edges survive")
      (is (true? (get-in out [:contains-result :degraded]))))
    (is (= {:created-count 1}
           (:contains-result (wrap/handle-kg-edges {:contains-edge-fn (constantly {:created-count 1})} data))))))

(deftest notify-and-evict
  (is (true? (:notify-sent? (wrap/handle-notify {} {}))))
  (let [seen (atom nil)]
    (wrap/handle-notify {:notify-fn (fn [& a] (reset! seen a))}
                        {:agent-id "a" :project-id "p" :crystal-result {:session "s" :stats :bad}})
    (is (= ["a" "s" "p" {}] @seen) "non-map stats become {}"))
  (is (= [false "boom"] ((juxt :notify-sent? :notify-error) (wrap/handle-notify {:notify-fn boom} {}))))
  (is (true? (get-in (wrap/handle-evict {:evict-fn boom} {}) [:eviction :degraded]))
      "wrap's evict degrades instead of propagating"))

;; =============================================================================
;; Whole FSM
;; =============================================================================

(defn- full-resources []
  {:harvest-fn       (constantly {:progress-notes ["n"]})
   :crystallize-fn   (constantly {:summary-id "s1" :session "sess" :stats {:n 1}})
   :source-ids-fn    (constantly ["e1" "e2"])
   :kg-edge-fn       (fn [_ ids _ _] {:created-count (count ids)})
   :notify-fn        (constantly nil)
   :evict-fn         (constantly {:evicted 2})
   :scope-fn         (constantly "proj")})

(deftest-golden wrap-session-summary
  "test/golden/workflows/wrap-session-summary.edn"
  {:full     (wrap/run-wrap-session (full-resources) {:agent-id "a" :directory "/d"})
   :degraded (wrap/run-wrap-session (assoc (full-resources) :crystallize-fn boom :notify-fn boom)
                                    {:agent-id "a" :directory "/d"})})

(deftest runners
  (is (m/validate wfs/CompiledFsm (wrap/compile-wrap)))
  (is (= "proj" (:project-id (wrap/run-wrap (wrap/compile-wrap)
                                            (assoc (full-resources) :agent-id "r" :directory "/r"))))
      "agent-id and directory fall back to resources")
  (testing "no agent-id routes to ::fsm/error, which throws"
    (is (thrown? clojure.lang.ExceptionInfo (wrap/run-wrap-session {:harvest-fn (constantly {})})))))
