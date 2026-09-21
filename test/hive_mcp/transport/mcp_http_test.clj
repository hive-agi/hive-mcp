(ns hive-mcp.transport.mcp-http-test
  "MCP over HTTP, driven through the real MCP SDK method table and, for the
   exposure policy, a real listening socket."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.transport.mcp-http :as mh]
            [io.modelcontext.clojure-sdk.server :as sdk])
  (:import [java.net InetSocketAddress Socket NetworkInterface Inet4Address
            URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers
            HttpRequest$BodyPublishers]))

(use-fixtures :each (fn [t] (mh/stop!) (try (t) (finally (mh/stop!)))))

(defn- echo-context []
  (sdk/create-context!
   {:name "mcp-http-test" :version "0.0.0"
    :tools [{:name "echo"
             :description "Echo the text back"
             :inputSchema {:type "object"
                           :properties {"text" {:type "string"}}
                           :required ["text"]}
             :handler (fn [{:keys [text]}] {:type "text" :text (str "echo:" text)})}]}))

(defn- dispatch [] (mh/sdk-dispatch (echo-context)))

(defn- req
  ([method uri] (req method uri nil nil))
  ([method uri body] (req method uri body nil))
  ([method uri body auth]
   {:request-method method :uri uri :body body
    :headers (cond-> {} auth (assoc "authorization" auth))}))

(defn- rpc [m] (json/write-str (merge {:jsonrpc "2.0"} m)))

(defn- body-of [resp] (some-> (:body resp) (json/read-str :key-fn keyword)))

;; ── Pure policy ──────────────────────────────────────────────────────────

(deftest bind-host-is-loopback-without-a-token
  (is (= "127.0.0.1" (mh/bind-host nil "0.0.0.0")))
  (is (= "127.0.0.1" (mh/bind-host "  " "10.0.0.5")))
  (is (= "0.0.0.0" (mh/bind-host "t" nil)))
  (is (= "10.0.0.5" (mh/bind-host "t" "10.0.0.5"))))

(deftest bearer-check
  (is (mh/bearer-ok? nil nil))
  (is (mh/bearer-ok? "" "anything"))
  (is (mh/bearer-ok? "tok" "Bearer tok"))
  (is (not (mh/bearer-ok? "tok" nil)))
  (is (not (mh/bearer-ok? "tok" "Bearer to")))
  (is (not (mh/bearer-ok? "tok" "Bearer tokk")))
  (is (not (mh/bearer-ok? "tok" "tok"))))

;; ── Through the real SDK method table ────────────────────────────────────

(deftest a-client-session-over-post
  (let [h (mh/make-handler (dispatch) nil)]
    (testing "initialize"
      (let [r (h (req :post "/mcp"
                      (rpc {:id 1 :method "initialize"
                            :params {:protocolVersion "2025-03-26"
                                     :capabilities {}
                                     :clientInfo {:name "t" :version "0"}}})))
            b (body-of r)]
        (is (= 200 (:status r)))
        (is (= 1 (:id b)))
        (is (= "mcp-http-test" (get-in b [:result :serverInfo :name])))))
    (testing "the initialized notification is accepted with no body"
      (let [r (h (req :post "/mcp" (rpc {:method "notifications/initialized"})))]
        (is (= 202 (:status r)))
        (is (nil? (:body r)))))
    (testing "tools/list names the tool"
      (let [b (body-of (h (req :post "/mcp" (rpc {:id "a" :method "tools/list"}))))]
        (is (= "a" (:id b)))
        (is (= ["echo"] (mapv :name (get-in b [:result :tools]))))))
    (testing "tools/call runs it"
      (let [b (body-of (h (req :post "/mcp"
                               (rpc {:id 2 :method "tools/call"
                                     :params {:name "echo" :arguments {:text "hi"}}}))))]
        (is (= 2 (:id b)))
        (is (nil? (:error b)))
        (is (re-find #"echo:hi" (pr-str (:result b))))))))

(deftest malformed-and-unknown
  (let [h (mh/make-handler (dispatch) nil)]
    (testing "not JSON"
      (let [r (h (req :post "/mcp" "{nope"))]
        (is (= 400 (:status r)))
        (is (= -32700 (get-in (body-of r) [:error :code])))))
    (testing "a batch is refused, this profile carries one message"
      (let [r (h (req :post "/mcp" (json/write-str [{:jsonrpc "2.0" :id 1 :method "ping"}])))]
        (is (= 400 (:status r)))
        (is (= -32600 (get-in (body-of r) [:error :code])))))
    (testing "wrong jsonrpc version"
      (is (= 400 (:status (h (req :post "/mcp" (json/write-str {:jsonrpc "1.0" :id 1 :method "ping"})))))))
    (testing "unknown method is a JSON-RPC error, not an HTTP one"
      (let [r (h (req :post "/mcp" (rpc {:id 9 :method "no/such"})))]
        (is (= 200 (:status r)))
        (is (= -32601 (get-in (body-of r) [:error :code])))
        (is (= 9 (:id (body-of r))))))
    (testing "GET offers no stream"
      (is (= 405 (:status (h (req :get "/mcp"))))))
    (testing "other paths"
      (is (= 404 (:status (h (req :post "/other" (rpc {:id 1 :method "ping"})))))))))

(defrecord ThrowingDispatch []
  mh/IRpcDispatch
  (request! [_ _ _] (throw (ex-info "boom" {:secret "do-not-leak"})))
  (notify! [_ _ _] (throw (ex-info "boom" {}))))

(deftest a-throwing-handler-is-an-internal-error-that-leaks-nothing
  (let [h (mh/make-handler (->ThrowingDispatch) nil)
        r (h (req :post "/mcp" (rpc {:id 5 :method "tools/call"})))]
    (is (= 200 (:status r)))
    (is (= -32603 (get-in (body-of r) [:error :code])))
    (is (not (re-find #"do-not-leak|boom" (:body r))))
    (is (= 202 (:status (h (req :post "/mcp" (rpc {:method "notifications/x"}))))))))

(defrecord RecordingDispatch [calls inner]
  mh/IRpcDispatch
  (request! [_ m p] (swap! calls conj [:request m]) (mh/request! inner m p))
  (notify! [_ m p] (swap! calls conj [:notify m]) (mh/notify! inner m p)))

(deftest a-token-gates-every-mcp-call-before-dispatch
  (let [calls (atom [])
        h (mh/make-handler (->RecordingDispatch calls (dispatch)) "s3cret")
        call (rpc {:id 1 :method "tools/list"})]
    (is (= 401 (:status (h (req :post "/mcp" call)))))
    (is (= 401 (:status (h (req :post "/mcp" call "Bearer wrong")))))
    (is (= 401 (:status (h (req :post "/mcp" call "Bearer s3cre")))))
    (is (= 401 (:status (h (req :get "/mcp")))))
    (is (= "Bearer" (get-in (h (req :post "/mcp" call)) [:headers "WWW-Authenticate"])))
    (is (= [] @calls) "nothing reached the dispatcher")
    (is (= 200 (:status (h (req :post "/mcp" call "Bearer s3cret")))))
    (is (= [[:request "tools/list"]] @calls))
    (is (= 200 (:status (h (req :get "/health")))) "health stays open for probes")))

;; ── Real socket ──────────────────────────────────────────────────────────

(defn- non-loopback-ipv4 []
  (->> (enumeration-seq (NetworkInterface/getNetworkInterfaces))
       (filter #(and (.isUp ^NetworkInterface %) (not (.isLoopback ^NetworkInterface %))))
       (mapcat #(enumeration-seq (.getInetAddresses ^NetworkInterface %)))
       (filter #(instance? Inet4Address %))
       first))

(defn- connects? [^java.net.InetAddress addr port]
  (try (with-open [s (Socket.)]
         (.connect s (InetSocketAddress. addr (int port)) 1000)
         true)
       (catch java.io.IOException _ false)))

(defn- post! [port body auth]
  (let [b (cond-> (-> (HttpRequest/newBuilder (URI. (str "http://127.0.0.1:" port "/mcp")))
                      (.header "Content-Type" "application/json")
                      (.POST (HttpRequest$BodyPublishers/ofString body)))
            auth (.header "Authorization" auth))
        r (.send (HttpClient/newHttpClient) (.build b) (HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode r) :body (.body r)}))

(deftest a-tokenless-server-is-unreachable-off-loopback
  (let [port (mh/start! {:dispatch (dispatch) :port 0})]
    (is (pos-int? port))
    (is (= {:running? true :port port :bind "127.0.0.1" :auth? false} (mh/status)))
    (is (= 200 (:status (post! port (rpc {:id 1 :method "ping"}) nil))))
    (when-let [addr (non-loopback-ipv4)]
      (is (not (connects? addr port)) (str "tokenless MCP answered on " addr)))))

(deftest a-token-server-answers-over-the-wire
  (let [port (mh/start! {:dispatch (dispatch) :port 0 :token "tok"})]
    (is (= "0.0.0.0" (:bind (mh/status))))
    (is (= 401 (:status (post! port (rpc {:id 1 :method "tools/list"}) nil))))
    (let [r (post! port (rpc {:id 1 :method "tools/list"}) "Bearer tok")]
      (is (= 200 (:status r)))
      (is (= ["echo"] (mapv :name (get-in (json/read-str (:body r) :key-fn keyword) [:result :tools])))))))
