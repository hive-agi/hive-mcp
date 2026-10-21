(ns hive-mcp.server.routes.middleware-test
  "Facets for hive-mcp.server.routes.middleware (cov-facets f07).

   spend-piggyback-budget is pure accounting and is driven from its contract.
   guard-raw-context is pinned on args that carry an explicit :directory (so no
   cwd or vessel lookup leaks in). The state-free wrappers (normalize,
   response, default-async-for-commands) are pinned over a recording stub
   handler. The wrappers that reach the guard seam, grants, NATS, the async
   registry and the slave registry are covered by their own suites
   (guard_middleware_test, piggyback_budget_test, activity_touch_test, ...)."
  (:require [hive-mcp.server.routes.middleware :as mw]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Chars [:int {:min -5 :max 400}])

(deftrifecta-from-schema spend-piggyback-budget-contract mw/spend-piggyback-budget
  {:in  [:cat [:int {:min 0 :max 400}] [:int {:min 0 :max 200}] Chars]
   :out [:map [:allowance [:int {:min 0}]] [:remaining [:int {:min 0}]]]
   :rel (fn [[remaining cap rendered] {:keys [allowance] :as out}]
          (and (= allowance (min remaining cap))
               (= (:remaining out) (max 0 (- remaining (max 0 rendered))))))
   :num-tests 300
   :golden-path "test/golden/hive-mcp/f07-middleware-spend-budget.edn"})

(defn- recording [seen]
  (fn [args] (swap! seen conj args) {:echo (:command args)}))

(deftest-golden middleware-wrappers-golden
  "test/golden/hive-mcp/f07-middleware-wrappers.edn"
  (let [seen  (atom [])
        h     (recording seen)
        async (mw/wrap-handler-default-async-for-commands h #{:ship-land})]
    {:normalize-map  ((mw/wrap-handler-normalize h) {:command "x"})
     :normalize-str  ((mw/wrap-handler-normalize (fn [_] "plain")) {})
     :normalize-seq  ((mw/wrap-handler-normalize (fn [_] (list {:type "text" :text "a"}))) {})
     :response       ((mw/wrap-handler-response (fn [_] [{:type "text" :text "r"}])) {})
     :async-calls    (do (async {:command "ship-land"})
                         (async {:command "status"})
                         (async {:command "ship-land" :async "false"})
                         (async {:command "status" :async true})
                         (async {})
                         @seen)
     :guard-context  [(mw/guard-raw-context "memory" {:directory "/w" :agent_id "a1"})
                      (mw/guard-raw-context "kanban" {:directory "/w" :_caller_id "ling-9"})]}))
