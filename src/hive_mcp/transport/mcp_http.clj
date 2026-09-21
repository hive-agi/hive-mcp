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
            [hive-mcp.dns.result :as result])
  (:import [java.net InetSocketAddress]
           [java.security MessageDigest]))

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

(defn sdk-dispatch
  "Dispatch into the MCP SDK's method table over CONTEXT."
  [context]
  (->SdkDispatch context
                 (requiring-resolve 'jsonrpc4clj.server/receive-request)
                 (requiring-resolve 'jsonrpc4clj.server/receive-notification)))

;; ── Pure ─────────────────────────────────────────────────────────────────

(defn bind-host
  "Loopback when TOKEN is blank, whatever was requested. Otherwise REQUESTED,
   default all interfaces."
  [token requested]
  (if (str/blank? token)
    "127.0.0.1"
    (if (str/blank? requested) "0.0.0.0" requested)))

(defn bearer-ok?
  "True when TOKEN is blank, or AUTH-HEADER is exactly `Bearer TOKEN`.
   Constant-time."
  [token auth-header]
  (or (str/blank? token)
      (MessageDigest/isEqual
       (.getBytes (str auth-header) "UTF-8")
       (.getBytes (str "Bearer " token) "UTF-8"))))

(defn rpc-error [id kind message]
  {:jsonrpc "2.0" :id id :error {:code (error-codes kind) :message message}})

(defn rpc-result [id value]
  {:jsonrpc "2.0" :id id :result value})

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

(defn stop! []
  (when-let [{:keys [server port]} @server-atom]
    (.close ^java.io.Closeable server)
    (reset! server-atom nil)
    (log/info "MCP HTTP stopped (was on port" port ")")))

(defn status []
  (let [s @server-atom]
    {:running? (boolean s) :port (:port s) :bind (:bind s) :auth? (boolean (:auth? s))}))
