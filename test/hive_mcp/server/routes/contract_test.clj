(ns hive-mcp.server.routes.contract-test
  "Contract facets for server/routes.clj and the server/routes files batch
   m19 owns (identity, middleware, async-ack). async_task/ and async_tasks.clj
   belong to batch m20. The middleware is driven as pure handlers over stub
   handlers: no port is bound and no server is started. The surface registry
   is restored around every test."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.server.routes.async-ack :as ack]
            [hive-mcp.server.routes.identity :as id]
            [hive-mcp.server.routes.middleware :as mw]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.generator :as mg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-surfaces [t]
  (let [before (routes/registered-surfaces)]
    (try (t)
         (finally
           (doseq [id (keys (routes/registered-surfaces)) :when (not (contains? before id))]
             (routes/unregister-surface! id))))))

(use-fixtures :each own-surfaces)

;; =============================================================================
;; Gate
;; =============================================================================

;; routes.clj's re-exports are `(def x #'id/x)` var aliases, so calls through
;; them reach the contracted fns; the facade file is in the :paths.
(def ^:private batch-paths
  ["src/hive_mcp/server/routes.clj"
   "src/hive_mcp/server/routes/async_ack.clj"
   "src/hive_mcp/server/routes/identity.clj"
   "src/hive_mcp/server/routes/middleware.clj"])

(coverage/deftest-contract-coverage routes-contracts
  {:paths batch-paths
   :exempt {}})

(deftest the-gate-is-not-vacuous
  (is (= 46 (count (:universe (coverage/coverage {:paths batch-paths}))))))

(deftest every-routes-contract-generates-both-sides
  (doseq [ns-sym '[hive-mcp.server.routes hive-mcp.server.routes.async-ack
                   hive-mcp.server.routes.identity hive-mcp.server.routes.middleware]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

;; =============================================================================
;; async-ack
;; =============================================================================

(deftrifecta-from-schema ack-names-the-task-and-where-the-result-lands
  #'ack/ack
  {:in [:cat [:enum "atask-1" "atask-2"] [:enum "memory" "kg"] [:maybe [:enum 1000 60000]] :boolean]
   :out ack/Ack
   :rel (fn [[task tool t durable?] out]
          (and (= task (:task-id out)) (= tool (:tool out))
               (= t (:timeout-ms out))
               (= (not durable?) (false? (:durable out)))
               (= ack/result-via (:result-via out))))
   :classify (fn [[_ _ t d] _] [(some? t) d])
   :classify-domain #{[true true] [true false] [false true] [false false]}
   :golden-path "test/golden/hive-mcp/server/routes/ack.edn"
   :num-tests 200})

;; =============================================================================
;; identity: content helpers
;; =============================================================================

(def ^:private Item [:or [:map [:type [:= "text"]] [:text [:enum "a" "bb" ""]]]
                     [:map [:type [:= "image"]] [:data [:enum "x"]]]])

(deftrifecta-from-schema normalize-content-always-yields-an-array
  #'id/normalize-content
  {:in [:cat [:or [:vector {:max 3} Item] Item [:map [:content [:vector {:min 1 :max 2} Item]]] :string :int :nil]]
   :out [:sequential :any]
   :rel (fn [[r] out]
          (cond (sequential? r) (= out (vec r))
                (and (map? r) (:content r)) (= out (:content r))
                (map? r) (= out [r])
                :else (= out [{:type "text" :text (str r)}])))
   :classify (fn [[r] _] (cond (sequential? r) :seq (and (map? r) (:content r)) :response (map? r) :map :else :other))
   :classify-domain #{:seq :response :map :other}
   :golden-path "test/golden/hive-mcp/server/routes/normalize-content.edn"
   :num-tests 200})

(deftrifecta-from-schema wrap-delimited-block-appends-to-the-last-text
  #'id/wrap-delimited-block
  {:in [:cat [:vector {:max 3} Item] [:enum "MEMORY" "HIVEMIND"] [:maybe [:enum "body" ""]]]
   :out [:sequential :any]
   :rel (fn [[c tag body] out]
          (if (seq body)
            (str/includes? (apply str (keep :text out)) (str "---" tag "---\nbody\n---/" tag "---"))
            (= out c)))
   :golden-path "test/golden/hive-mcp/server/routes/wrap-delimited-block.edn"
   :num-tests 200})

(deftest wrap-delimited-block-never-merges-a-bare-map
  (is (= [{:type "text" :text "out\n\n---T---\nb\n---/T---"}]
         (id/wrap-delimited-block {:type "text" :text "out"} "T" "b")))
  (is (= [{:type "image"} {:type "text" :text "\n\n---T---\nb\n---/T---"}]
         (id/wrap-delimited-block [{:type "image"}] "T" "b")))
  (is (= 0 (id/find-last-text-idx [{:type "text"} {:type "text"}])))
  (is (nil? (id/find-last-text-idx [{:type "image"}])))
  (is (= [{:type "text" :text "x"}] (id/wrap-piggyback [{:type "text" :text "x"}] [])))
  (is (str/includes? (:text (first (id/wrap-memory-piggyback-content [{:type "text" :text "x"}] {:a 1}))) "---MEMORY---")))

(deftest-golden identity-extraction
  "test/golden/hive-mcp/server/routes/identity.edn"
  {:agent [(id/extract-agent-id {:agent_id "a"} "d") (id/extract-agent-id {:agent-id "b"} "d")
           (id/extract-agent-id {} "d")]
   :caller [(id/extract-caller-id {:_caller_id "ling-1" :agent_id "target"}) (id/extract-caller-id {})]
   :directory (id/extract-directory {:directory "/tmp/x"})
   :project (id/extract-project-id {:project_id "p1"})
   :caller-identity [(id/extract-caller-identity {:_caller_id "ling-1"}) (id/extract-caller-identity {})]
   :scope (id/extract-project-scope {:project_id "p1"})})

;; =============================================================================
;; middleware: pure wrappers over stub handlers
;; =============================================================================

(deftrifecta-from-schema spend-piggyback-budget-never-overspends
  #'mw/spend-piggyback-budget
  {:in [:cat [:int {:min -10 :max 40000}] [:int {:min 0 :max 20000}] [:int {:min -5 :max 40000}]]
   :out [:map [:allowance [:int {:min 0}]] [:remaining [:int {:min 0}]]]
   :rel (fn [[r cap used] out]
          (and (= (:allowance out) (max 0 (min r cap)))
               (= (:remaining out) (max 0 (- r (max 0 used))))))
   :golden-path "test/golden/hive-mcp/server/routes/spend-piggyback-budget.edn"
   :num-tests 300})

(deftest-golden content-wrappers-over-a-stub-handler
  "test/golden/hive-mcp/server/routes/content-wrappers.edn"
  (let [h (fn [args] (:ret args))]
    {:normalize [((mw/wrap-handler-normalize h) {:ret {:a 1}})
                 ((mw/wrap-handler-normalize h) {:ret "s"})]
     :response ((mw/wrap-handler-response h) {:ret [1]})
     :compress-off ((mw/wrap-handler-compress h) {:ret [{:type "text" :text "x"}]})
     :default-async [((mw/wrap-handler-default-async-for-commands identity #{:add}) {:command "add"})
                     ((mw/wrap-handler-default-async-for-commands identity #{:add}) {:command "add" :async "false"})
                     ((mw/wrap-handler-default-async-for-commands identity #{:add}) {:command "get"})]
     :not-async ((mw/wrap-handler-async h "t") {:ret 7})
     :lane ((mw/in-embed-lane :interactive (constantly :ran)))
     :nats ((mw/wrap-handler-nats-notify h "memory") {:command "get" :ret :ok})
     :retry ((mw/wrap-handler-retry h) {:ret :ok})
     :guard-ctx (dissoc (mw/guard-raw-context "t" {:_caller_id "c" :agent_id "a" :directory "/d"}) :input)}))

(deftest retry-rethrows-after-the-last-attempt
  (let [n (atom 0)
        h (mw/wrap-handler-retry (fn [_] (swap! n inc) (throw (IllegalStateException. "var not found"))))]
    (is (thrown? IllegalStateException (h {})))
    (is (= 3 @n))))

(deftest-golden activity-touch-owns-its-throttle-and-port
  "test/golden/hive-mcp/server/routes/activity-touch.edn"
  ;; The throttle atom is reset on both sides and the registry write port is
  ;; bound to a recording stub, so nothing reaches the swarm registry and no
  ;; stamp leaks into another test. Times are reduced to their presence.
  (let [touches (atom [])
        rows {"ling-a" {:slave/id "ling-a" :slave/alive? true}
              "ling-d" {:slave/id "ling-d" :slave/alive? false}}
        touch (fn [caller] (some? (mw/touch-caller-activity! rows caller)))]
    (try
      (mw/reset-activity-throttle!)
      (binding [mw/*touch-slave!* (fn [id updates]
                                    (swap! touches conj [id (sort (keys updates))])
                                    :written)]
        (let [calls {:first (touch "ling-a:1")
                     :throttled (touch "ling-a:2")
                     :dead (touch "ling-d:1")
                     :unknown (touch "nobody:1")
                     :nil-caller (touch nil)}
              after-reset [(mw/reset-activity-throttle!) (touch "ling-a:3")]
              failing (binding [mw/*touch-slave!* (fn [_ _] (throw (ex-info "down" {})))]
                        (mw/reset-activity-throttle!)
                        (mw/touch-caller-activity! rows "ling-a:4"))]
          {:calls calls
           :after-reset after-reset
           :failing-port failing
           :touches @touches}))
      (finally (mw/reset-activity-throttle!)))))

;; =============================================================================
;; routes: pure table helpers
;; =============================================================================

(def ^:private View [:maybe [:map-of [:enum "a" "b" "c"] [:enum 1 2]]])

(deftrifecta-from-schema changed-tool-names-diffs-two-views
  #'routes/changed-tool-names
  {:in [:cat View View]
   :out [:set :any]
   :rel (fn [[o n] out]
          (= out (set (filter #(not= (get o %) (get n %)) (concat (keys o) (keys n))))))
   :golden-path "test/golden/hive-mcp/server/routes/changed-tool-names.edn"
   :num-tests 200})

(deftrifecta-from-schema tool-table-keys-by-name-and-strips-handlers
  #'routes/tool-table
  {:in [:cat [:maybe [:vector {:max 3} [:map [:name [:enum "a" "b"]] [:handler [:enum :h1 :h2]] [:x :int]]]]]
   :out routes/Table
   :rel (fn [[ts] out]
          (and (= (set (keys out)) (set (map :name ts)))
               (every? (fn [[_ {:keys [tool]}]] (not (contains? tool :handler))) out)))
   :golden-path "test/golden/hive-mcp/server/routes/tool-table.edn"
   :num-tests 200})

(deftest notification-targets-need-a-change-and-a-notifier
  (let [surfaces {:a {:surface/notify! (fn [])} :b {}}]
    (is (= [:a] (routes/notification-targets ["x"] [:a :b] surfaces)))
    (is (= [] (routes/notification-targets [] [:a :b] surfaces)))))

(deftest surfaces-register-install-and-unregister
  (let [tools (atom {})
        ctx (atom nil)]
    (is (= ::t (routes/register-surface! ::t {:surface/kind :tools-atom :surface/tools-atom tools})))
    (is (contains? (routes/registered-surfaces) ::t))
    (is (= ::t (routes/unregister-surface! ::t)))
    (is (nil? (routes/refresh-tools! ctx)))
    (is (nil? (routes/debug-tool-handler ctx "x")))
    (reset! ctx {:tools (atom {"x" {:tool {:name "x"} :handler identity}})})
    (is (= "x" (:name (routes/debug-tool-handler ctx "x"))))))

(deftest make-tool-wraps-the-handler-and-advertises-async
  (let [t (routes/make-tool {:name "m19_probe" :description "d"
                             :inputSchema {:type "object" :properties {"q" {:type "string"}}}
                             :handler (fn [_] "ok")})]
    (is (m/validate routes/MadeTool t))
    (is (contains? (get-in t [:inputSchema :properties]) "async"))
    (is (fn? (:handler t)))))
