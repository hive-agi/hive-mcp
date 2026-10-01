(ns hive-mcp.server.registration-test
  "tools/list projection: the hide-rule registry (pure), and the installed
   method as MCP-HTTP actually dispatches it, whatever order the SDK loaded in."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [io.modelcontext.clojure-sdk.server :as sdk]
            [jsonrpc4clj.server :as jsonrpc-server]
            [hive-mcp.server.registration :as reg]
            [hive-mcp.transport.mcp-http :as mcp-http]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private deprecated-only {:deprecated #'reg/deprecated-tool?})

(def ^:private gen-tool
  (gen/let [n (gen/fmap #(str "t" %) (gen/choose 0 50))
            d gen/boolean]
    (cond-> {:name n} d (assoc :deprecated true))))

(defspec visible-tools-is-exactly-the-unhidden-tools 200
  (prop/for-all [tools (gen/vector gen-tool 0 15)]
    (let [entries (map (fn [t] {:tool t :handler identity}) tools)
          visible (reg/visible-tools deprecated-only entries)]
      (and (= visible (vec (remove :deprecated tools)))
           (not-any? :deprecated visible)))))

(deftest no-rules-hide-nothing-test
  (let [entries [{:tool {:name "a" :deprecated true}} {:tool {:name "b"}}]]
    (is (= ["a" "b"] (mapv :name (reg/visible-tools {} entries))))))

(deftest a-new-hide-reason-is-one-registration-test
  (reg/register-hide-rule! ::internal (fn [tool] (= "internal" (:name tool))))
  (try
    (is (contains? (reg/hide-rule-ids) ::internal))
    (is (= ["kept"]
           (mapv :name (reg/visible-tools @@#'reg/hide-rules
                                          [{:tool {:name "internal"}} {:tool {:name "kept"}}]))))
    (finally (reg/unregister-hide-rule! ::internal)))
  (is (not (contains? (reg/hide-rule-ids) ::internal))))

(defn- context-with-tools []
  (sdk/create-context!
   {:name "registration-test" :version "0"
    :tools [{:name "live" :description "live" :inputSchema {:type "object"}
             :handler (fn [_] {:type "text" :text "x"})}
            {:name "old" :description "old" :deprecated true :inputSchema {:type "object"}
             :handler (fn [_] {:type "text" :text "y"})}]}))

(defn- http-tools-list [context]
  (->> (mcp-http/answer (mcp-http/sdk-dispatch context)
                        {:jsonrpc "2.0" :id 1 :method "tools/list"})
       :body :result :tools (mapv :name)))

(deftest mcp-http-tools-list-hides-deprecated-tools-test
  (is (reg/tools-list-installed?) "loading the namespace installs the filter")
  (is (= ["live"] (http-tools-list (context-with-tools)))))

(deftest the-filter-survives-the-sdk-method-being-installed-later-test
  (testing "the SDK's unfiltered method wins when it is (re)installed after us"
    (.addMethod ^clojure.lang.MultiFn jsonrpc-server/receive-request "tools/list"
                (fn [_ context _] {:tools (mapv :tool (vals @(:tools context)))}))
    (try
      (is (not (reg/tools-list-installed?)))
      (is (= #{"live" "old"} (set (http-tools-list (context-with-tools)))))
      (finally
        (testing "and install-tools-list! (run at MCP-HTTP start) takes it back"
          (is (true? (reg/install-tools-list!)))))))
  (is (= ["live"] (http-tools-list (context-with-tools)))))

(deftest hidden-tools-stay-callable-test
  (let [ctx (context-with-tools)
        res (mcp-http/answer (mcp-http/sdk-dispatch ctx)
                             {:jsonrpc "2.0" :id 2 :method "tools/call"
                              :params {:name "old" :arguments {}}})]
    (is (= "y" (get-in res [:body :result :content 0 :text])))))
