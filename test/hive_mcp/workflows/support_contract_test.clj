(ns hive-mcp.workflows.support-contract-test
  "Contracts and hive-test facets for the shared workflow FSM primitives.

   Pure seams (resolve-session-identity, the trace interceptors, now-str with an
   injected clock, always) get schema trifectas. The boundary envelope and the
   kanban markers are driven arm by arm under instrumentation: each policy arm
   (nil / fatal / continue) and each guard arm (present / absent) is asserted
   to produce its own shape."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.workflows.schema :as wfs]
            [hive-mcp.workflows.support :as support]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  #{#'support/boundary-step #'support/now-str #'support/shout! #'support/always
    #'support/trace-log-enter #'support/trace-log-exit #'support/default-handle-error
    #'support/resolve-session-identity #'support/handle-evict
    #'support/mark-task-inprogress! #'support/mark-tasks-inprogress!})

(use-fixtures :each
  (fn [f]
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try (f) (finally (mi/unstrument!)))))

;; =============================================================================
;; resolve-session-identity — data-then-resources fallback
;; =============================================================================

(def ^:private Dir [:maybe [:enum "/w/a" "/w/b"]])
(def ^:private Agent [:maybe [:enum "ling-1" "coordinator"]])

(def ^:private IdentityArgs
  [:cat
   [:map [:directory {:optional true} Dir] [:agent-id {:optional true} Agent]]
   [:map [:directory {:optional true} Dir] [:agent-id {:optional true} Agent]]
   [:maybe [:map
            [:derive-project? {:optional true} :boolean]
            [:project-default {:optional true} [:maybe [:enum "unknown"]]]]]])

(deftrifecta-from-schema resolve-session-identity-fallback
  hive-mcp.workflows.support/resolve-session-identity
  {:in IdentityArgs
   :out support/SessionIdentity
   :rel (fn [[resources data opts] out]
          (let [derive? (get opts :derive-project? true)]
            (and (= (:agent-id out) (or (:agent-id data) (:agent-id resources)))
                 (= (:directory out) (or (:directory data) (:directory resources)))
                 ;; no :scope-fn in resources => the scope fn answers nil
                 (= (:project-id out) (when derive? (:project-default opts))))))
   :golden-path "test/golden/workflows/support-session-identity.edn"
   :num-tests 200})

(deftest resolve-session-identity-calls-scope-fn-only-when-deriving
  (let [calls (atom 0)
        res   {:scope-fn (fn [d] (swap! calls inc) (str "p:" d))}]
    (is (= {:agent-id nil :directory "/w" :project-id "p:/w"}
           (support/resolve-session-identity res {:directory "/w"})))
    (is (= {:agent-id nil :directory "/w" :project-id nil}
           (support/resolve-session-identity res {:directory "/w"} {:derive-project? false})))
    (is (= 1 @calls) "derive-project? false never calls scope-fn")
    (testing "rescue-scope? turns a throwing scope-fn into the default"
      (is (= "unknown"
             (:project-id (support/resolve-session-identity
                           {:scope-fn (fn [_] (throw (ex-info "down" {})))}
                           {:directory "/w"}
                           {:rescue-scope? true :project-default "unknown"})))))))

;; =============================================================================
;; Trace interceptors and the clock seam
;; =============================================================================

(def ^:private TraceIn
  [:cat
   [:map
    [:current-state-id [:enum :a :b :hive.events.fsm/start]]
    [:data [:map [:trace-log {:optional true} [:vector {:gen/max 3} wfs/TraceEntry]]]]]
   [:map]])

(deftrifecta-from-schema trace-log-enter-appends
  hive-mcp.workflows.support/trace-log-enter
  {:in TraceIn
   :out wfs/TracedFsm
   :rel (fn [[fsm _] out]
          (let [before (get-in fsm [:data :trace-log] [])
                after  (get-in out [:data :trace-log])]
            (and (= (count after) (inc (count before)))
                 (= before (pop after))
                 (= {:state (:current-state-id fsm) :direction :enter}
                    (dissoc (peek after) :at)))))
   :num-tests 100})

(deftrifecta-from-schema trace-log-exit-appends
  hive-mcp.workflows.support/trace-log-exit
  {:in TraceIn
   :out wfs/TracedFsm
   :rel (fn [[fsm _] out]
          (let [after (get-in out [:data :trace-log])]
            (and (= (inc (count (get-in fsm [:data :trace-log] []))) (count after))
                 (= :exit (:direction (peek after)))
                 (= (:current-state-id fsm) (:state (peek after))))))
   :num-tests 100})

(deftest now-str-reads-the-injected-clock
  (is (= "2026-01-01T00:00:00Z"
         (support/now-str {:clock-fn (constantly (java.time.Instant/parse "2026-01-01T00:00:00Z"))})))
  (is (= "tick" (support/now-str {:clock-fn (constantly "tick")})))
  (is (string? (support/now-str nil)) "absent clock falls back to Instant/now")
  (is (= {:state :s :at "T" :direction :enter}
         (-> (support/trace-log-enter {:current-state-id :s} {:clock-fn (constantly "T")})
             (get-in [:data :trace-log 0])))))

(deftrifecta-from-schema always-is-true
  hive-mcp.workflows.support/always
  {:in :any
   :out [:= true]
   :num-tests 50})

;; =============================================================================
;; boundary-step — every arm
;; =============================================================================

(deftest boundary-step-arms
  (let [boom (fn [_] (throw (ex-info "boom" {})))]
    (testing "guard false takes the absent leg"
      (is (= {:x 1 :absent true}
             (support/boundary-step {:x 1} {:present? false :run boom
                                            :absent #(assoc % :absent true)}))))
    (testing "absent defaults to identity"
      (is (= {:x 1} (support/boundary-step {:x 1} {:present? false :run boom}))))
    (testing "success leg"
      (is (= {:x 2} (support/boundary-step {:x 1} {:run #(update % :x inc)}))))
    (testing "nil policy propagates the throw"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"boom"
                            (support/boundary-step {} {:run boom :policy nil}))))
    (testing "fatal stamps a top-level :error over the PRE-effect data"
      (is (= {:x 1 :phase :p :error "Step failed: boom"}
             (support/boundary-step {:x 1} {:run boom :policy support/fatal
                                            :spec {:log-msg "m" :label "Step"
                                                   :degrade-keys {:phase :p}}}))))
    (testing "continue merges the delta and never stamps :error"
      (let [out (support/boundary-step {:x 1} {:run boom :policy support/continue
                                               :spec {:log-msg "m"
                                                      :delta (fn [ex] {:why (ex-message ex)})}})]
        (is (= {:x 1 :why "boom"} out))
        (is (not (contains? out :error)))))
    (testing "a non-policy is rejected by the contract"
      (is (thrown? Exception (support/boundary-step {} {:run identity :policy :nope}))))))

(deftest handle-evict-arms
  (is (= {:eviction {:evicted 0 :skipped true}} (support/handle-evict {} {} nil)))
  (is (= {:agent-id "a" :eviction {:evicted 3}}
         (support/handle-evict {:evict-fn (fn [_] {:evicted 3})} {:agent-id "a"} nil)))
  (is (thrown? Exception
               (support/handle-evict {:evict-fn (fn [_] (throw (ex-info "x" {})))} {} nil))
      "nil policy: complete-session's fatal evict propagates")
  (let [out (support/handle-evict {:evict-fn (fn [_] (throw (ex-info "x" {})))} {} support/continue)]
    (is (= {:evicted 0 :skipped true :degraded true :error "x"} (:eviction out)))
    (is (m/validate wfs/Eviction (:eviction out)))))

(deftest shout-and-error-factory
  (let [seen (atom [])]
    (is (nil? (support/shout! {} "a" :start "m")) "no shout-fn: nil")
    (support/shout! {:shout-fn (fn [& args] (swap! seen conj args))} "a" :start "m")
    (is (= [["a" :start "m"]] @seen))
    (is (nil? (support/shout! {:shout-fn (fn [& _] (throw (ex-info "x" {})))} "a" :start "m"))
        "a throwing shout-fn is rescued"))
  (let [h (support/default-handle-error "Wrap failed" [:k])]
    (is (ifn? h))
    (let [ex (try (h {} {:error "e" :data {:agent-id "a" :k 1 :z 2}}) nil
                  (catch clojure.lang.ExceptionInfo e e))]
      (is (= "Wrap failed" (ex-message ex)))
      (is (= {:agent-id "a" :data {:k 1} :error "e"} (ex-data ex))))))

(deftest kanban-markers
  (let [seen (atom [])
        upd  (fn [m] (swap! seen conj (:task_id m)) :ok)]
    (is (nil? (support/mark-task-inprogress! upd "/d" nil)) "nil id is skipped")
    (is (= :ok (support/mark-task-inprogress! upd "/d" "t1")))
    (is (nil? (support/mark-tasks-inprogress! upd "/d" [{:id "t2"} {:id nil} {:id "t3"}])))
    (is (= ["t1" "t2" "t3"] @seen))
    (reset! seen [])
    (support/mark-tasks-inprogress! upd "/d" [{:id nil} {:id "t4"}] {:skip-nil? false})
    (is (= [nil "t4"] @seen) "skip-nil? false attempts every id")
    (is (nil? (support/mark-task-inprogress! (fn [_] (throw (ex-info "x" {}))) "/d" "t"))
        "a kanban failure is rescued")))
