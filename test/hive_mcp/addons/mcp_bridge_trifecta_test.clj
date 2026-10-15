(ns hive-mcp.addons.mcp-bridge-trifecta-test
  "Trifecta for the MCP-bridge proxy layer (`hive-mcp.addons.mcp-bridge`,
   `hive-mcp.addons.stdio-bridge`).

   The middleware stack is pure `handler -> handler`, so it is driven through
   unary adapters over an ECHO handler: the golden pins what a composed stack
   does to a params map. The proxy handler is driven through a reified
   `IMcpBridge` (a port implementation) with canned `call-tool` answers, never
   a redef. Schemas come from the contracts; nothing is hand-described twice."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.protocol :as proto]
            [hive-mcp.addons.mcp-bridge :as bridge]
            [hive-mcp.addons.stdio-bridge :as stdio]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Middleware stack over an echo handler
;; =============================================================================

(def ^:private gen-key (gen/elements [:directory :agent_id :project_id :q :limit :api_key]))

(def ^:private gen-params
  (gen/map gen-key (gen/elements ["x" 1 true "y"]) {:max-elements 5}))

(defn run-default-stack
  "Params through the DEFAULT middleware stack over an echo handler."
  [params]
  ((bridge/compose-middleware identity (bridge/default-proxy-middleware)) params))

(deftrifecta default-stack-contract
  hive-mcp.addons.mcp-bridge-trifecta-test/run-default-stack
  {:golden-path "test/golden/addons/mcp-bridge-default-stack.edn"
   :cases {:empty     {}
           :internal  {:directory "/x" :agent_id "a" :q "find"}
           :mixed     {:q "find" :limit 3 :project_id "p"}
           :only-user {:q "find" :limit 3}}
   :gen gen-params
   :pred (fn [r] (and (map? r)
                      (every? string? (keys r))
                      (not-any? #(contains? r %) ["directory" "agent_id" "project_id"])))
   :num-tests 200
   :mutations [["strips nothing" (fn [p] (update-keys p name))]
               ["strips without stringifying" (fn [p] (apply dissoc p bridge/default-strip-params))]
               ["drops everything" (constantly {})]]
   :assert (fn []
             (is (= {"q" "find"}
                    (run-default-stack {:q "find" :directory "/x" :agent_id "a"}))))})

(defn run-extra-params
  "[extra caller] through wrap-proxy-params over an echo handler."
  [[extra caller]]
  (((bridge/wrap-proxy-params extra) identity) caller))

(deftrifecta extra-params-caller-wins
  hive-mcp.addons.mcp-bridge-trifecta-test/run-extra-params
  {:golden-path "test/golden/addons/mcp-bridge-extra-params.edn"
   :cases {:inject      [{:api_key "k"} {:q "a"}]
           :caller-wins [{:q "default"} {:q "mine"}]
           :empty-extra [{} {:q "a"}]}
   :gen (gen/tuple gen-params gen-params)
   :pred map?
   :num-tests 200
   :mutations [["extra wins over caller" (fn [[e c]] (merge c e))]
               ["ignores extra" (fn [[_ c]] c)]
               ["ignores caller" (fn [[e _]] e)]]
   :assert (fn []
             (is (= {:q "mine" :k 1} (run-extra-params [{:q "default" :k 1} {:q "mine"}]))))})

(deftest compose-order-test
  (let [tag (fn [t] (fn [h] (fn [p] (h (update p :trail (fnil conj []) t)))))
        h   (bridge/compose-middleware identity [(tag :a) (tag :b) (tag :c)])]
    (testing "the LAST middleware wraps outermost, so it runs first"
      (is (= [:c :b :a] (:trail (h {})))))
    (testing "an empty stack is the handler itself"
      (is (= {:x 1} ((bridge/compose-middleware identity []) {:x 1}))))))

(deftest retry-and-timeout-test
  (testing "retry re-invokes a throwing handler up to max-retries, then answers"
    (let [calls (atom 0)
          flaky (fn [_] (if (< (swap! calls inc) 3) (throw (ex-info "transport" {})) :ok))]
      (is (= :ok (((bridge/wrap-proxy-retry 3 1) flaky) {})))
      (is (= 3 @calls))))
  (testing "retry exhausted rethrows"
    (is (thrown? clojure.lang.ExceptionInfo
                 (((bridge/wrap-proxy-retry 2 1) (fn [_] (throw (ex-info "down" {})))) {}))))
  (testing "a handler slower than the timeout answers a text error, not an exception"
    (let [slow (fn [_] (Thread/sleep 400) :late)
          r    (((bridge/wrap-proxy-timeout 20) slow) {})]
      (is (= "text" (:type r)))
      (is (re-find #"timed out after 20ms" (:text r)))))
  (testing "a handler inside the timeout answers its own value"
    (is (= :fast (((bridge/wrap-proxy-timeout 500) (fn [_] :fast)) {})))))

(deftest logging-middleware-is-transparent-test
  (let [h ((bridge/wrap-proxy-logging "pfx" "tool") (fn [p] (assoc p :seen true)))]
    (is (= {:a 1 :seen true} (h {:a 1})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (((bridge/wrap-proxy-logging "pfx" "tool") (fn [_] (throw (ex-info "x" {})))) {}))
        "failures are logged and rethrown unchanged")))

;; =============================================================================
;; Proxy tool-defs over a reified IMcpBridge
;; =============================================================================

(defn- canned-bridge
  "An IMcpBridge answering `call` for every call-tool and `remote` for list."
  [call remote]
  (reify bridge/IMcpBridge
    (transport-type [_] :stdio)
    (start-bridge! [_ _] {:success? true :errors []})
    (stop-bridge! [_] {:success? true :errors []})
    (bridge-status [_] {:connected? true :transport :stdio :uptime-ms 1 :remote-tool-count (count remote)})
    (call-tool [_ n p] (if (fn? call) (call n p) call))
    (list-remote-tools [_] remote)))

(defn run-proxy-call
  "Canned remote answer -> the text/seq the proxy handler returns."
  [answer]
  (let [b   (canned-bridge answer [])
        def (bridge/proxy-tool-def b "pfx" {:name "t"} {:skip-defaults? true})]
    ((:handler def) {})))

(deftrifecta proxy-handler-contract
  hive-mcp.addons.mcp-bridge-trifecta-test/run-proxy-call
  {:golden-path "test/golden/addons/mcp-bridge-proxy-handler.edn"
   :cases {:text      {:content [{:type "text" :text "hello"}]}
           :error     {:isError true :content [{:type "text" :text "bad"} {:type "text" :text "worse"}]}
           :multi     {:content [{:type "text" :text "a"} {:type "text" :text "b"}]}
           :no-text   {:content []}}
   :gen (gen/fmap (fn [texts] {:content (mapv (fn [t] {:type "text" :text t}) texts)})
                  (gen/vector gen/string-alphanumeric 0 3))
   :pred (fn [r] (or (map? r) (sequential? r)))
   :num-tests 100
   :mutations [["always the first item" (fn [a] {:type "text" :text (-> a :content first :text)})]
               ["never an error" (constantly {:type "text" :text "ok"})]
               ["throws away multi-content" (fn [a] (first (:content a)))]]
   :assert (fn []
             (is (= {:type "text" :text "Remote tool error: bad\nworse"}
                    (run-proxy-call {:isError true :content [{:text "bad"} {:text "worse"}]})))
             (is (= [{:text "a"} {:text "b"}]
                    (run-proxy-call {:content [{:text "a"} {:text "b"}]}))
                 "multi-content responses are returned whole")
             (is (= {:type "text" :text "hello"}
                    (run-proxy-call {:content [{:text "hello"}]}))))})

(deftest proxy-tool-def-shape-test
  (let [b (canned-bridge {:content [{:text "r"}]} [])
        d (bridge/proxy-tool-def b "pfx" {:name "convert" :description "Convert"})]
    (testing "name is namespaced, description attributed, schema defaulted"
      (is (= "pfx:convert" (:name d)))
      (is (= "Convert [via pfx]" (:description d)))
      (is (= {:type "object" :properties {}} (:inputSchema d)))
      (is (= "pfx" (:bridge-source d))))
    (testing "name-xf / desc-xf / schema-xf override the defaults"
      (let [d2 (bridge/proxy-tool-def b "pfx" {:name "convert" :description "C" :inputSchema {:type "object"}}
                                      {:name-xf (fn [n p] (str p "." n))
                                       :desc-xf (fn [d _] (str "<" d ">"))
                                       :schema-xf #(assoc % :extra true)})]
        (is (= "pfx.convert" (:name d2)))
        (is (= "<C>" (:description d2)))
        (is (= {:type "object" :extra true} (:inputSchema d2)))))
    (testing "the contract schema accepts what was built"
      (is (m/validate bridge/ProxyToolDef d)))))

(deftest proxy-tool-defs-filters-test
  (let [remote [{:name "keep"} {:name "drop"}]
        b      (canned-bridge nil remote)]
    (is (= ["pfx:keep" "pfx:drop"] (mapv :name (bridge/proxy-tool-defs b "pfx"))))
    (is (= ["pfx:keep"]
           (mapv :name (bridge/proxy-tool-defs b "pfx" {:tool-filter #(= "keep" (:name %))}))))
    (is (= [] (bridge/proxy-tool-defs (reify bridge/IMcpBridge
                                        (list-remote-tools [_] (throw (ex-info "down" {}))))
                                      "pfx"))
        "a bridge that cannot list tools yields no tools, never an exception")))

;; =============================================================================
;; Bridge registry (noop + stdio constructors)
;; =============================================================================

(deftest bridge-registry-roundtrip-test
  (let [b (bridge/->noop-bridge :bridge/trifecta)]
    (try
      (testing "predicates"
        (is (bridge/bridge? b))
        (is (bridge/bridge-addon? b))
        (is (not (bridge/bridge? {:not "a bridge"})))
        (is (not (bridge/bridge-addon? (reify bridge/IMcpBridge)))))
      (testing "register -> listed -> connected? false for the noop -> unregister"
        (let [r (bridge/register-bridge! b {})]
          (is (:success? r))
          (is (= "trifecta" (:bridge-prefix r))))
        (is (identical? b (bridge/get-bridge :bridge/trifecta)))
        (is (some #(m/validate bridge/BridgeListing %) (bridge/list-bridges)))
        (is (false? (bridge/bridge-connected? :bridge/trifecta)))
        (is (nil? (bridge/bridge-connected? :bridge/absent))))
      (finally
        (bridge/unregister-bridge! :bridge/trifecta)))
    (is (nil? (bridge/get-bridge :bridge/trifecta)))))

(deftest stdio-constructor-test
  (let [b (stdio/->stdio-bridge :bridge/stdio-probe)]
    (is (m/validate bridge/BridgeAddon b))
    (is (= :stdio (bridge/transport-type b)))
    (is (= :bridge/stdio-probe (proto/addon-id b)))
    (is (false? (:connected? (bridge/bridge-status b))) "not started -> not connected")))

(deftest schemas-generate-and-discriminate-test
  (doseq [s [bridge/Bridge bridge/BridgeAddon bridge/TransportType bridge/RemoteTool
             bridge/ProxyOpts bridge/BridgeStatus bridge/BridgeOutcome]]
    (is (seq (mg/sample s {:size 5})) (str "generates: " (pr-str s))))
  (is (not (m/validate bridge/TransportType :carrier-pigeon)))
  (is (not (m/validate bridge/RemoteTool {:description "no name"})))
  (is (not (m/validate bridge/Bridge {:not "a bridge"}))))
