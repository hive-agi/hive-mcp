(ns hive-mcp.extensions.hot-stale-verb-test
  "Stub addon reload conformance: dispatch through the MCP table must see the
   new handler, and a failed refresh must not look like a successful reload."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.agent.registry :as agent-reg]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.extensions.reactive :as reactive]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.tools.consolidated.code :as code]
            [hive-mcp.tools.consolidated.hot :as hot]
            [hive-mcp.tools.composite :as composite]))

(defn scan-stub [_] {:text "old"})

(defn- stub-tool []
  {:name "stub-carto" :description "stub carto tool"
   :inputSchema {:type "object" :properties {"command" {:type "string"}}}
   :handler (composite/build-composite-handler "stub-carto")})

(defn dispatch-after-stub-reload
  "Mount a stub addon, build the MCP tool table, then simulate an in-place
   namespace reload by re-interning its handler var. No live server or addon
   namespace is loaded. Returns direct and dispatched responses."
  [generation]
  (let [original @#'scan-stub
        tool-name "stub-carto"
        addon-id ::stub-carto]
    (try
      (ext/contribute-commands! tool-name addon-id
                                {"scan-polyglot" {:handler #'scan-stub}})
      (let [table (routes/tool-table [(routes/make-tool (stub-tool))])
            handler (get-in table [tool-name :handler])
            params {:command "scan-polyglot"}]
        (alter-var-root #'scan-stub (constantly (fn [_] {:text generation})))
        {:direct (:text (scan-stub params))
         :mcp (get-in (handler params) [:content 0 :text])})
      (finally
        (alter-var-root #'scan-stub (constantly original))
        (ext/retract-commands! tool-name addon-id)))))

(deftrifecta stub-reload-reaches-mcp
  hive-mcp.extensions.hot-stale-verb-test/dispatch-after-stub-reload
  {:golden-path "test/golden/hot/stale-verb.edn"
   :cases {:new-handler "new" :another-handler "after-reload"}
   :gen (gen/elements ["new" "after-reload" "v3"])
   :pred (fn [{:keys [direct mcp]}] (= direct mcp))
   :num-tests 20
   :mutations [["captured-old-handler" (fn [_] {:direct {:text "new"}
                                                  :mcp {:text "old"}})]]})

(defn refresh-verdict
  "Run a registry refresh against an injected advertised-surface port."
  [fail?]
  (binding [agent-reg/*advertised-tools*
            (if fail?
              (fn [] (throw (ex-info "stub registry refused" {})))
              (fn [] [{:name "stub" :handler #'scan-stub}]))]
    (try
      (let [count' (agent-reg/refresh!)]
        {:ok? true :count count'})
      (catch Exception e
        {:ok? false :message (ex-message e)}))))

(deftrifecta failed-refresh-is-loud
  hive-mcp.extensions.hot-stale-verb-test/refresh-verdict
  {:golden-path "test/golden/hot/refresh-verdict.edn"
   :cases {:success false :failed true}
   :gen gen/boolean
   :pred (fn [result] (or (= {:ok? true :count 1} result)
                          (= {:ok? false :message "stub registry refused"} result)))
   :num-tests 20
   :mutations [["silently-return-zero" (fn [_] {:ok? true :count 0})]]})

(deftest failed-refresh-is-in-the-reload-report
  (let [failed (hot/report-with-surface
                {:ok? true :errors []}
                {:agent-tools {:ok? false :error "stub registry refused"}})]
    (testing "the failed agent registry read cannot leave hot reload green"
      (is (false? (:ok? failed)))
      (is (= ["tool registry refresh failed: stub registry refused"] (:errors failed)))))
  (let [good (hot/report-with-surface {:ok? true :errors []}
                                      {:agent-tools {:ok? true :count 1}})]
    (is (true? (:ok? good)))))

(deftest refresh-surface-exposes-the-failure
  (binding [agent-reg/*advertised-tools*
            (fn [] (throw (ex-info "stub registry refused" {})))]
    (let [report (reactive/refresh-agent-tools!)]
      (is (= {:ok? false :error "stub registry refused"} report)))))
