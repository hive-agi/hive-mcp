(ns hive-mcp.transport.mcp-http
  "MCP over HTTP: the stateless profile of the Streamable HTTP transport.

   POST /mcp carries one JSON-RPC message. A request is answered with one
   application/json response, a notification with 202. No SSE stream is
   offered, so GET /mcp is 405, which the transport allows.

   Exposure policy: without a token the server binds loopback only. With a
   token every /mcp call must carry `Authorization: Bearer <token>`."
  (:require [aleph.http :as http]
            [aleph.netty :as netty]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [malli.core :as m]
            [taoensso.timbre :as log]
            [hive-mcp.dns.result :as result]
            [hive-mcp.transport.exposure :as exposure])
  (:import [java.net InetSocketAddress]))

;; ── Schemas ──────────────────────────────────────────────────────────────

(def RpcMessage
  [:map
   [:jsonrpc [:= "2.0"]]
   [:method :string]
   [:id {:optional true} [:or :string :int]]
   [:params {:optional true} [:maybe [:or [:map-of :any :any] [:sequential :any]]]]])

(def HttpReply
  [:map
   [:status :int]
   [:body {:optional true} :any]])

(def error-codes
  {:parse-error      -32700
   :invalid-request  -32600
   :method-not-found -32601
   :internal-error   -32603})

(def ErrorKind
  "A key of `error-codes`."
  [:enum :parse-error :invalid-request :method-not-found :internal-error])

(def RpcId
  "A JSON-RPC id as the message carried it, or nil when it carried none."
  [:maybe [:or :string :int]])

(def RpcErrorBody
  [:map [:jsonrpc [:= "2.0"]] [:id RpcId]
   [:error [:map [:code [:maybe :int]] [:message [:maybe :string]]]]])

(def RpcResultBody
  [:map [:jsonrpc [:= "2.0"]] [:id RpcId] [:result :any]])

(def Port [:int {:min 0 :max 65535}])

;; ── Ports ────────────────────────────────────────────────────────────────

(defprotocol IRpcDispatch
  (request! [this method params]
    "Answer a JSON-RPC request. Returns the result value, or the keyword
     :method-not-found.")
  (notify! [this method params]
    "Deliver a JSON-RPC notification. Return value is ignored."))

(defn- not-found? [v]
  (and (keyword? v) (= "method-not-found" (name v))))

(defrecord SdkDispatch [context receive-request receive-notification]
  IRpcDispatch
  (request! [_ method params]
    (let [v (receive-request method context params)]
      (if (not-found? v) :method-not-found v)))
  (notify! [_ method params]
    (receive-notification method context params)))

(def Dispatch
  "An IRpcDispatch port."
  [:fn {:gen/fmap (fn [_] (reify IRpcDispatch
                            (request! [_ _ _] {:ok true})
                            (notify! [_ _ _] nil)))
        :gen/schema :nil}
   #(satisfies? IRpcDispatch %)])

(defn sdk-dispatch
  "Dispatch into the MCP SDK's method table over CONTEXT."
  [context]
  (->SdkDispatch context
                 (requiring-resolve 'jsonrpc4clj.server/receive-request)
                 (requiring-resolve 'jsonrpc4clj.server/receive-notification)))

(m/=> sdk-dispatch
      [:=> [:cat :map]
       [:fn {:gen/fmap (fn [_] (->SdkDispatch {} (constantly nil) (constantly nil)))
             :gen/schema :nil}
        #(instance? SdkDispatch %)]])

;; ── Pure ─────────────────────────────────────────────────────────────────

(defn bind-host
  "Loopback when TOKEN is blank, whatever was requested. Otherwise REQUESTED,
   default all interfaces."
  [token requested]
  (exposure/bind-host token requested))

(m/=> bind-host [:=> [:cat exposure/Secret [:maybe :string]] exposure/Host])

(defn bearer-ok?
  "True when TOKEN is blank, or AUTH-HEADER is exactly `Bearer TOKEN`.
   Constant-time."
  [token auth-header]
  (exposure/bearer-ok? token auth-header))

(m/=> bearer-ok? [:=> [:cat exposure/Secret [:maybe :string]] :boolean])

(defn rpc-error [id kind message]
  {:jsonrpc "2.0" :id id :error {:code (error-codes kind) :message message}})

(m/=> rpc-error [:=> [:cat RpcId ErrorKind [:maybe :string]] RpcErrorBody])

(defn rpc-result [id value]
  {:jsonrpc "2.0" :id id :result value})

(m/=> rpc-result [:=> [:cat RpcId :any] RpcResultBody])

(defn parse-message
  "BODY string to {:ok msg} or {:error reply-map}."
  [body]
  (let [parsed (result/rescue ::unreadable (json/read-str body :key-fn keyword))]
    (cond
      (= ::unreadable parsed)
      {:error {:status 400 :body (rpc-error nil :parse-error "Body is not JSON")}}

      (not (m/validate RpcMessage parsed))
      {:error {:status 400
               :body (rpc-error (when (map? parsed) (:id parsed))
                                :invalid-request
                                "Expected one JSON-RPC 2.0 message")}}

      :else {:ok parsed})))

(m/=> parse-message
      [:=> [:cat :string]
       [:or [:map [:ok RpcMessage]]
        [:map [:error [:map [:status [:= 400]] [:body RpcErrorBody]]]]]])

(defn answer
  "One parsed MSG through DISPATCH to an HttpReply."
  [dispatch {:keys [id method params] :as msg}]
  (if-not (contains? msg :id)
    (do (result/rescue nil (notify! dispatch method params))
        {:status 202})
    (let [v (result/rescue ::threw (request! dispatch method params))]
      (cond
        (= ::threw v)
        {:status 200 :body (rpc-error id :internal-error "Request handler failed")}

        (= :method-not-found v)
        {:status 200 :body (rpc-error id :method-not-found (str "Unknown method: " method))}

        :else
        {:status 200 :body (rpc-result id v)}))))

;; A notification (no :id) is 202 with no body; a request is 200 with a
;; result or an error body.
(m/=> answer
      [:=> [:cat Dispatch RpcMessage]
       [:or [:map [:status [:= 202]]]
        [:map [:status [:= 200]] [:body [:or RpcResultBody RpcErrorBody]]]]])

;; ── Boundary ─────────────────────────────────────────────────────────────

(defn- read-body [req]
  (let [b (:body req)]
    (cond (nil? b) "" (string? b) b :else (slurp b))))

(defn- ring [{:keys [status body]}]
  (cond-> {:status status :headers {"Content-Type" "application/json"}}
    (some? body) (assoc :body (json/write-str body))))

(defn make-handler
  "Ring handler over DISPATCH, gated by TOKEN."
  [dispatch token]
  (fn [req]
    (let [method (:request-method req)
          uri    (:uri req)]
      (cond
        (and (= :get method) (= "/health" uri))
        (ring {:status 200 :body {:status "healthy" :service "mcp-http"}})

        (not= "/mcp" uri)
        (ring {:status 404 :body {:error "not found"}})

        (not (bearer-ok? token (get-in req [:headers "authorization"])))
        (assoc-in (ring {:status 401 :body {:error "Invalid or missing bearer token"}})
                  [:headers "WWW-Authenticate"] "Bearer")

        (not= :post method)
        (assoc-in (ring {:status 405 :body {:error "POST only"}})
                  [:headers "Allow"] "POST")

        :else
        (let [parsed (parse-message (read-body req))]
          (ring (if-let [reply (:error parsed)]
                  reply
                  (answer dispatch (:ok parsed)))))))))

(def RingHandler
  "A ring handler: request map -> ring response."
  [:=> [:cat :map] [:map [:status :int] [:headers [:map-of :string :string]]
                    [:body {:optional true} :string]]])

(m/=> make-handler
      [:=> [:cat Dispatch exposure/Secret]
       [:fn {:gen/fmap (fn [_] (fn [_] {:status 404 :headers {}}))
             :gen/schema :nil}
        ifn?]])

(defonce ^:private server-atom (atom nil))

(defn start!
  "Start the MCP HTTP server. Opts: :dispatch (IRpcDispatch, required), :port
   (default 7921), :token, :bind. Returns the actual port, nil on failure."
  [{:keys [dispatch port token bind]}]
  (if-let [running @server-atom]
    (do (log/warn "MCP HTTP already running on port" (:port running))
        (:port running))
    (result/rescue nil
      (let [token  (when-not (str/blank? token) token)
            host   (bind-host token bind)
            server (http/start-server
                    (make-handler dispatch token)
                    {:socket-address (InetSocketAddress. ^String host (int (or port 7921)))})
            actual (netty/port server)]
        (reset! server-atom {:server server :port actual :bind host :auth? (some? token)})
        (log/info "MCP HTTP started on" (str host ":" actual)
                  (if token "(bearer auth)" "(no auth, loopback only)"))
        actual))))

;; The bound port, nil when the server could not start.
(m/=> start!
      [:=> [:cat [:map [:dispatch {:optional true} [:maybe Dispatch]]
                  [:port {:optional true} [:maybe Port]]
                  [:token {:optional true} exposure/Secret]
                  [:bind {:optional true} [:maybe :string]]]]
       [:maybe Port]])

(defn stop! []
  (when-let [{:keys [server port]} @server-atom]
    (.close ^java.io.Closeable server)
    (reset! server-atom nil)
    (log/info "MCP HTTP stopped (was on port" port ")")))

;; nil either way: the last form is a log call, or no server was running.
(m/=> stop! [:=> [:cat] :nil])

(defn status []
  (let [s @server-atom]
    {:running? (boolean s) :port (:port s) :bind (:bind s) :auth? (boolean (:auth? s))}))

(m/=> status
      [:=> [:cat]
       [:map [:running? :boolean] [:port [:maybe Port]] [:bind [:maybe :string]] [:auth? :boolean]]])
