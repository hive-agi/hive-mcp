(ns hive-mcp.transport.mcp-http.facet-test
  "Facets for hive-mcp.transport.mcp-http (cov-facets f07). No socket: the
   Ring handler is called directly with request maps and an injected
   IRpcDispatch stub that answers by method name. The pure helpers (rpc-error,
   rpc-result, parse-message, bearer-ok?, bind-host) are driven from their
   contracts; the over-the-wire behaviour stays in mcp_http_test.clj."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.transport.mcp-http :as mh]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private RpcId [:maybe [:or [:string {:min 1 :max 4}] [:int {:min 0 :max 99}]]])
(def ^:private Kind [:enum :parse-error :invalid-request :method-not-found :internal-error])

(deftrifecta-from-schema rpc-error-contract mh/rpc-error
  {:in  [:cat RpcId Kind [:maybe [:string {:max 8}]]]
   :out [:map [:jsonrpc [:= "2.0"]] [:id RpcId]
         [:error [:map [:code [:maybe :int]] [:message [:maybe :string]]]]]
   :rel (fn [[id kind msg] out]
          (and (= id (:id out))
               (= msg (get-in out [:error :message]))
               (= (get {:parse-error -32700 :invalid-request -32600
                        :method-not-found -32601 :internal-error -32603} kind)
                  (get-in out [:error :code]))))
   :golden-path "test/golden/hive-mcp/f07-mcp-http-rpc-error.edn"})

(deftrifecta-from-schema rpc-result-contract mh/rpc-result
  {:in  [:cat RpcId [:or :int [:map [:ok :boolean]]]]
   :out [:map [:jsonrpc [:= "2.0"]] [:id RpcId] [:result :any]]
   :rel (fn [[id v] out] (= {:jsonrpc "2.0" :id id :result v} out))
   :golden-path "test/golden/hive-mcp/f07-mcp-http-rpc-result.edn"})

(def ^:private Body
  "Bodies that reach every arm of parse-message: valid single messages, a
   wrong version, a batch, and text that is not JSON."
  [:or
   [:map {:gen/fmap json/write-str}
    [:jsonrpc [:= "2.0"]] [:id [:int {:min 0 :max 9}]] [:method [:enum "ping" "tools/list"]]]
   [:enum "{nope" "" "[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}]"
    "{\"jsonrpc\":\"1.0\",\"id\":3,\"method\":\"ping\"}"]])

(deftrifecta-from-schema parse-message-contract mh/parse-message
  {:in  [:cat [:fn {:gen/schema Body :gen/fmap #(if (string? %) % (json/write-str %))} string?]]
   :out [:or [:map [:ok :map]] [:map [:error [:map [:status [:= 400]] [:body :map]]]]]
   :rel (fn [[body] out]
          (let [ok? (and (str/starts-with? body "{\"")
                         (str/includes? body "\"2.0\""))]
            (if ok? (contains? out :ok) (= 400 (get-in out [:error :status])))))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/f07-mcp-http-parse-message.edn"})

(deftrifecta-from-schema bind-host-contract mh/bind-host
  {:in  [:cat [:maybe [:enum "" "  " "tok"]] [:maybe [:enum "0.0.0.0" "10.0.0.5"]]]
   :out [:string {:min 1}]
   :rel (fn [[token req] out]
          (if (str/blank? token)
            (= out "127.0.0.1")
            (= out (or req "0.0.0.0"))))
   :golden-path "test/golden/hive-mcp/f07-mcp-http-bind-host.edn"})

(defrecord TableDispatch []
  mh/IRpcDispatch
  (request! [_ method _params]
    (case method
      "ping"       {}
      "tools/list" {:tools [{:name "echo"}]}
      "boom"       (throw (ex-info "boom" {:secret "x"}))
      :method-not-found))
  (notify! [_ _ _] nil))

(defn- req [method uri body auth]
  {:request-method method :uri uri :body body
   :headers (cond-> {} auth (assoc "authorization" auth))})

(defn- view
  "Response projected to status, the two policy headers and the parsed body."
  [r]
  (cond-> {:status (:status r)}
    (get-in r [:headers "WWW-Authenticate"]) (assoc :www-auth (get-in r [:headers "WWW-Authenticate"]))
    (get-in r [:headers "Allow"])            (assoc :allow (get-in r [:headers "Allow"]))
    (:body r) (assoc :body (json/read-str (:body r) :key-fn keyword))))

(deftest-golden mcp-http-handler-golden
  "test/golden/hive-mcp/f07-mcp-http-handler.edn"
  (let [open  (mh/make-handler (->TableDispatch) nil)
        gated (mh/make-handler (->TableDispatch) "tok")
        rpc   #(json/write-str (merge {:jsonrpc "2.0"} %))]
    {:health        (view (open (req :get "/health" nil nil)))
     :not-found     (view (open (req :post "/other" (rpc {:id 1 :method "ping"}) nil)))
     :get-mcp       (view (open (req :get "/mcp" nil nil)))
     :ping          (view (open (req :post "/mcp" (rpc {:id 1 :method "ping"}) nil)))
     :list          (view (open (req :post "/mcp" (rpc {:id "a" :method "tools/list"}) nil)))
     :unknown       (view (open (req :post "/mcp" (rpc {:id 2 :method "no/such"}) nil)))
     :throws        (view (open (req :post "/mcp" (rpc {:id 3 :method "boom"}) nil)))
     :notification  (view (open (req :post "/mcp" (rpc {:method "notifications/x"}) nil)))
     :not-json      (view (open (req :post "/mcp" "{nope" nil)))
     :no-token      (view (gated (req :post "/mcp" (rpc {:id 4 :method "ping"}) nil)))
     :wrong-token   (view (gated (req :post "/mcp" (rpc {:id 4 :method "ping"}) "Bearer to")))
     :right-token   (view (gated (req :post "/mcp" (rpc {:id 4 :method "ping"}) "Bearer tok")))
     :bearer        [(mh/bearer-ok? nil nil) (mh/bearer-ok? "tok" "Bearer tok") (mh/bearer-ok? "tok" "tok")]}))
