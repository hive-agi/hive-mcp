(ns hive-mcp.transport.contract-trifecta-test
  "Contracts of src/hive_mcp/transport (batch m30): the coverage gates, the
   schemas generating, and trifectas over the pure fns the contracts describe.

   The server lifecycles (start!/stop!/status, the bencode socket servers) are
   boundaries; their existing suites (a2a-bind, mcp-http, websocket) are run
   under instrumentation, and the cases here drive them through their ports."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.transport.a2a :as a2a]
            [hive-mcp.transport.a2a.handlers :as handlers]
            [hive-mcp.transport.a2a.schema :as schema]
            [hive-mcp.transport.core :as tc]
            [hive-mcp.transport.exposure :as exposure]
            [hive-mcp.transport.mcp-http :as mh]
            [hive-mcp.transport.olympus]
            [hive-mcp.transport.olympus.http :as ohttp]
            [hive-mcp.transport.olympus.snapshots :as snap]
            [hive-mcp.transport.olympus.state-bridge :as bridge]
            [hive-mcp.transport.olympus.stream :as stream]
            [hive-mcp.transport.websocket :as ws]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]
            [manifold.stream :as ms]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; Gates
;;; ---------------------------------------------------------------------------

;; transport/olympus.clj is a facade of (def x #'impl/x) VAR aliases: a call
;; through the facade goes through the impl var, so instrumenting the impl
;; reaches facade callers. The facade file is in the olympus gate so a defn
;; added there is counted.
(coverage/deftest-contract-coverage transport-contracts-gate
  {:paths ["src/hive_mcp/transport"]
   :exempt {}})

(coverage/deftest-contract-coverage transport-a2a-contracts-gate
  {:paths ["src/hive_mcp/transport/a2a"]
   :exempt {}})

(coverage/deftest-contract-coverage transport-olympus-contracts-gate
  {:paths ["src/hive_mcp/transport/olympus" "src/hive_mcp/transport/olympus.clj"]
   :exempt {}})

(deftest transport-gate-is-not-vacuous
  (let [r (coverage/coverage {:paths ["src/hive_mcp/transport"]})]
    (is (= 58 (count (:universe r))))
    (is (= 58 (count (:covered r))))))

(def ^:private transport-nses
  '[hive-mcp.transport.core hive-mcp.transport.exposure hive-mcp.transport.mcp-http
    hive-mcp.transport.websocket hive-mcp.transport.a2a hive-mcp.transport.a2a.schema
    hive-mcp.transport.a2a.handlers hive-mcp.transport.olympus.snapshots
    hive-mcp.transport.olympus.state-bridge hive-mcp.transport.olympus.stream
    hive-mcp.transport.olympus.http])

(deftest every-transport-contract-generates-both-sides
  (doseq [ns-sym transport-nses
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (let [sch (m/schema schema)]
                  (if (= :function (m/type sch)) (m/children sch) [sch]))
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

;;; ---------------------------------------------------------------------------
;;; a2a.schema
;;; ---------------------------------------------------------------------------

(def ^:private State [:enum "working" "completed" "failed" "canceled"])

(hst/deftrifecta-from-schema make-message-contract
  hive-mcp.transport.a2a.schema/make-message
  {:in          [:cat [:enum "user" "agent"]
                 [:sequential {:max 3} [:map [:type [:= "text"]] [:text [:enum "a" "b"]]]]]
   :out         schema/Message
   :rel         (fn [[role parts] out] (= out {:role role :parts (vec parts)}))
   :golden-path "test/golden/transport/a2a/make-message.edn"})

(hst/deftrifecta-from-schema make-task-contract
  hive-mcp.transport.a2a.schema/make-task
  {:in          [:cat [:enum "t1" "t2"] [:enum "c1" "c2"] State [:enum "ok" "done" ""]
                 [:alt [:cat]
                  [:cat [:= :metadata] [:map [:k [:enum 1 2]]]]
                  [:cat [:= :messages] [:vector {:max 2} [:enum {} {:role "user" :parts [{:type "text" :text "q"}]}]]
                   [:= :artifacts] [:vector {:max 1} [:enum {} {:a 1}]]]]]
   :out         schema/Task
   :rel         (fn [[id ctx state msg & kvs] out]
                  (let [opts (apply hash-map kvs)]
                    (and (= id (:id out)) (= ctx (:contextId out))
                         (= state (get-in out [:status :state]))
                         (= msg (get-in out [:status :message :parts 0 :text]))
                         (= (into {} (filter val) (select-keys opts [:messages :artifacts :metadata]))
                            (select-keys out [:messages :artifacts :metadata])))))
   :golden-path "test/golden/transport/a2a/make-task.edn"})

(hst/deftrifecta-from-schema make-status-update-event-contract
  hive-mcp.transport.a2a.schema/make-status-update-event
  {:in          [:cat [:enum "t1" "t2"] State [:enum "m" "connected"]]
   :out         schema/StatusUpdateEvent
   :rel         (fn [[id state msg] out]
                  (= out {:id id :kind "status-update"
                          :status {:state state :message {:role "agent" :parts [{:type "text" :text msg}]}}}))
   :golden-path "test/golden/transport/a2a/make-status-update-event.edn"})

(hst/deftrifecta-from-schema make-artifact-update-event-contract
  hive-mcp.transport.a2a.schema/make-artifact-update-event
  {:in          [:cat [:enum "t1" "t2"] [:map [:name [:enum "a" "b"]]]]
   :out         schema/ArtifactUpdateEvent
   :rel         (fn [[id art] out] (= out {:id id :artifact art :kind "artifact-update"}))
   :golden-path "test/golden/transport/a2a/make-artifact-update-event.edn"})

(hst/deftrifecta-from-schema jsonrpc-success-contract
  hive-mcp.transport.a2a.schema/jsonrpc-success
  {:in          [:cat [:or [:enum "1" "2"] [:int {:min 0 :max 9}] :nil] [:map [:x [:enum 1 2]]]]
   :out         schema/RpcSuccess
   :rel         (fn [[id r] out] (= out {:jsonrpc "2.0" :id id :result r}))
   :golden-path "test/golden/transport/a2a/jsonrpc-success.edn"})

(hst/deftrifecta-from-schema jsonrpc-error-contract
  hive-mcp.transport.a2a.schema/jsonrpc-error
  {:in          [:cat [:or [:enum "1"] [:int {:min 0 :max 9}] :nil]
                 (into [:enum] (vals schema/error-codes)) [:enum "bad" "missing"]
                 [:alt [:cat] [:cat [:= :hint] [:enum "h1" "h2"]]]]
   :out         schema/RpcError
   :rel         (fn [[id code msg & kvs] out]
                  (and (= {:jsonrpc "2.0" :id id} (select-keys out [:jsonrpc :id]))
                       (= {:code code :message msg} (dissoc (:error out) :data))
                       (= (some->> (seq kvs) (apply hash-map)) (get-in out [:error :data]))))
   :golden-path "test/golden/transport/a2a/jsonrpc-error.edn"})

;;; ---------------------------------------------------------------------------
;;; a2a.handlers: the refusal arms are pure (no agent lookup is reached)
;;; ---------------------------------------------------------------------------

(def ^:private codes schema/error-codes)

(hst/deftrifecta-from-schema handle-get-task-contract
  hive-mcp.transport.a2a.handlers/handle-get-task
  {:in          [:map [:id {:optional true} [:enum "" "t1" "a2a-1"]]]
   :out         handlers/HandlerResult
   ;; no journal entry for these ids, so a non-blank id is a generic working task
   :rel         (fn [{:keys [id]} out]
                  (if (empty? id)
                    (= (:invalid-params codes) (get-in out [:error :code]))
                    (and (= id (get-in out [:result :id]))
                         (= "working" (get-in out [:result :status :state])))))
   :golden-path "test/golden/transport/a2a/handle-get-task.edn"})

(hst/deftrifecta-from-schema handle-cancel-task-refusals-contract
  hive-mcp.transport.a2a.handlers/handle-cancel-task
  ;; no metadata.hive-agent-id: refused before any agent lookup
  {:in          [:map [:id {:optional true} [:enum "" "t1"]]]
   :out         handlers/HandlerError
   :rel         (fn [{:keys [id]} out]
                  (= (if (empty? id)
                       "id is required"
                       "Cannot resolve agent for task. Provide metadata.hive-agent-id")
                     (get-in out [:error :message])))
   :golden-path "test/golden/transport/a2a/handle-cancel-task.edn"})

(def ^:private SendRefused
  "Params refused before the agent lookup: no agent id, or no parts."
  [:or
   [:map [:configuration [:map [:agentId [:enum ""]]]]
    [:message [:map [:parts [:vector {:min 1 :max 2} [:enum {} {:type "text" :text "x"}]]]]]]
   [:map [:configuration [:map [:agentId [:enum "a1" "a2"]]]] [:message [:map [:parts [:enum {} []]]]]]])

(hst/deftrifecta-from-schema handle-send-message-refusals-contract
  hive-mcp.transport.a2a.handlers/handle-send-message
  {:in          SendRefused
   :out         handlers/HandlerError
   :rel         (fn [in out]
                  (= (if (empty? (get-in in [:configuration :agentId]))
                       "configuration.agentId is required"
                       "message.parts is required")
                     (get-in out [:error :message])))
   :golden-path "test/golden/transport/a2a/handle-send-message.edn"})

(hst/deftrifecta-from-schema handle-send-streaming-message-refusals-contract
  hive-mcp.transport.a2a.handlers/handle-send-streaming-message
  {:in          SendRefused
   :out         handlers/StreamingResult
   :rel         (fn [in out] (= out (handlers/handle-send-message in)))
   :golden-path "test/golden/transport/a2a/handle-send-streaming-message.edn"})

(deftest handler-result-arms-discriminate
  (let [ok  (handlers/handle-get-task {:id "t1"})
        bad (handlers/handle-get-task {})]
    (is (m/validate handlers/HandlerSuccess ok))
    (is (not (m/validate handlers/HandlerError ok)))
    (is (m/validate handlers/HandlerError bad))
    (is (not (m/validate handlers/HandlerSuccess bad)))))

;;; ---------------------------------------------------------------------------
;;; mcp-http: pure stratum
;;; ---------------------------------------------------------------------------

(hst/deftrifecta-from-schema rpc-error-contract
  hive-mcp.transport.mcp-http/rpc-error
  {:in          [:cat [:or [:enum "1"] [:int {:min 0 :max 9}] :nil] mh/ErrorKind [:enum "m1" "m2"]]
   :out         mh/RpcErrorBody
   :rel         (fn [[id kind msg] out]
                  (= out {:jsonrpc "2.0" :id id :error {:code (mh/error-codes kind) :message msg}}))
   :golden-path "test/golden/transport/mcp-http/rpc-error.edn"})

(hst/deftrifecta-from-schema rpc-result-contract
  hive-mcp.transport.mcp-http/rpc-result
  {:in          [:cat [:or [:enum "1"] [:int {:min 0 :max 9}]] [:map [:v [:enum 1 2 3]]]]
   :out         mh/RpcResultBody
   :rel         (fn [[id v] out] (= out {:jsonrpc "2.0" :id id :result v}))
   :golden-path "test/golden/transport/mcp-http/rpc-result.edn"})

(def ^:private Body
  "Request bodies reaching every arm: JSON-RPC requests and notifications,
   wrong version, missing method, a JSON array, and non-JSON."
  [:enum
   "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}"
   "{\"jsonrpc\":\"2.0\",\"id\":\"a\",\"method\":\"tools/list\",\"params\":{}}"
   "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
   "{\"jsonrpc\":\"1.0\",\"id\":2,\"method\":\"ping\"}"
   "{\"jsonrpc\":\"2.0\",\"id\":3}"
   "[1,2]"
   "not json"
   ""])

(hst/deftrifecta-from-schema parse-message-contract
  hive-mcp.transport.mcp-http/parse-message
  {:in          Body
   :out         [:or [:map [:ok mh/RpcMessage]]
                 [:map [:error [:map [:status [:= 400]] [:body mh/RpcErrorBody]]]]]
   :rel         (fn [in out]
                  (if (contains? out :ok)
                    (and (str/includes? in "\"2.0\"") (str/includes? in "method"))
                    (contains? #{-32700 -32600} (get-in out [:error :body :error :code]))))
   :n-cases     8
   :golden-path "test/golden/transport/mcp-http/parse-message.edn"})

(defrecord ScriptedDispatch []
  mh/IRpcDispatch
  (request! [_ method _]
    (case method
      "ping" {:pong true}
      "boom" (throw (ex-info "boom" {}))
      :method-not-found))
  (notify! [_ _ _] nil))

(def ^:private stub (->ScriptedDispatch))

(defn run-answer
  "Unary adapter: one parsed message through the scripted dispatch port."
  [msg]
  (mh/answer stub msg))

(def ^:private Msg
  "Parsed messages reaching every answer arm: notification, answered request,
   unknown method, throwing handler."
  [:or
   [:map [:jsonrpc [:= "2.0"]] [:method [:enum "ping" "notifications/x"]]]
   [:map [:jsonrpc [:= "2.0"]] [:id [:or [:int {:min 0 :max 9}] [:enum "a"]]]
    [:method [:enum "ping" "boom" "nope"]]]])

(hst/deftrifecta-from-schema answer-contract
  hive-mcp.transport.contract-trifecta-test/run-answer
  {:in          Msg
   :out         [:or [:map [:status [:= 202]]]
                 [:map [:status [:= 200]] [:body [:or mh/RpcResultBody mh/RpcErrorBody]]]]
   :rel         (fn [msg out]
                  (if-not (contains? msg :id)
                    (= {:status 202} out)
                    (= (case (:method msg)
                         "ping" (mh/rpc-result (:id msg) {:pong true})
                         "boom" (mh/rpc-error (:id msg) :internal-error "Request handler failed")
                         (mh/rpc-error (:id msg) :method-not-found (str "Unknown method: " (:method msg))))
                       (:body out))))
   :num-tests   150
   :golden-path "test/golden/transport/mcp-http/answer.edn"})

(deftest answer-arms-test
  (is (= {:status 202} (run-answer {:jsonrpc "2.0" :method "ping"})))
  (is (= {:status 200 :body {:jsonrpc "2.0" :id 1 :result {:pong true}}}
         (run-answer {:jsonrpc "2.0" :id 1 :method "ping"})))
  (is (= -32601 (get-in (run-answer {:jsonrpc "2.0" :id 2 :method "nope"}) [:body :error :code])))
  (is (= -32603 (get-in (run-answer {:jsonrpc "2.0" :id 3 :method "boom"}) [:body :error :code]))))

(deftest make-handler-routes-test
  (let [h (mh/make-handler stub "tok")]
    (is (= 200 (:status (h {:request-method :get :uri "/health"}))))
    (is (= 404 (:status (h {:request-method :get :uri "/nope"}))))
    (is (= 401 (:status (h {:request-method :post :uri "/mcp" :headers {}}))))
    (is (= 405 (:status (h {:request-method :get :uri "/mcp" :headers {"authorization" "Bearer tok"}}))))
    (is (= 202 (:status (h {:request-method :post :uri "/mcp" :headers {"authorization" "Bearer tok"}
                            :body "{\"jsonrpc\":\"2.0\",\"method\":\"x\"}"}))))))

(deftest sdk-dispatch-wraps-the-sdk-table-test
  (is (instance? hive_mcp.transport.mcp_http.SdkDispatch (mh/sdk-dispatch {}))))

;;; ---------------------------------------------------------------------------
;;; exposure (its own trifecta lives in exposure-trifecta-test)
;;; ---------------------------------------------------------------------------

(deftest bind-host-is-never-blank-test
  (doseq [secret [nil "" "  " "s"] requested [nil "" " " "10.0.0.1"]]
    (is (m/validate exposure/Host (exposure/bind-host secret requested)))
    (is (= (exposure/bind-host secret requested)
           (a2a/bind-host secret requested)
           (mh/bind-host secret requested)))))

;;; ---------------------------------------------------------------------------
;;; transport.core: the bencode codec
;;; ---------------------------------------------------------------------------

(def ^:private BMsg
  [:map-of [:enum :type "id" :data]
   [:or [:enum "ping" "pong" :kw] [:int {:min -5 :max 5}] [:vector {:max 3} [:enum "a" 1]]]])

(defn- stringify [v]
  (cond (map? v) (into {} (map (fn [[k x]] [(name k) (stringify x)]) v))
        (sequential? v) (mapv stringify v)
        (keyword? v) (name v)
        :else v))

(defn roundtrip
  "Unary adapter: encode then decode, the codec's defining law over the
   contracted encode-msg and decode-msg."
  [msg]
  (tc/decode-msg (tc/encode-msg msg)))

(hst/deftrifecta-from-schema bencode-roundtrip-contract
  hive-mcp.transport.contract-trifecta-test/roundtrip
  {:in          BMsg
   :out         [:maybe tc/Decoded]
   :rel         (fn [in out] (= (stringify in) out))
   :num-tests   200
   :golden-path "test/golden/transport/core/bencode-roundtrip.edn"})

(deftest decode-msg-arms-test
  (is (= {"a" 1} (tc/decode-msg (tc/encode-msg {:a 1}))))
  (is (nil? (tc/decode-msg (.getBytes "garbage" "UTF-8"))) "unreadable bytes are nil")
  (is (= {"a" []} (tc/decode-msg (tc/encode-msg {:a nil}))) "nil encodes as an empty list"))

(deftest decode-msg-from-stream-arms-test
  (let [in (fn [^bytes b] (java.io.PushbackInputStream. (java.io.ByteArrayInputStream. b)))]
    (is (= {"k" "v"} (tc/decode-msg-from-stream (in (tc/encode-msg {:k "v"})))))
    (is (= :eof (tc/decode-msg-from-stream (in (byte-array 0)))))
    ;; a truncated or malformed value runs the reader off the end of a finite
    ;; stream, which is the EOF arm, not the recoverable-nil one
    (is (= :eof (tc/decode-msg-from-stream (in (.getBytes "d3:abc" "UTF-8")))))))

(deftest encode-msg-refuses-what-bencode-cannot-write
  (is (thrown? Exception (tc/encode-msg {:a true})))
  (is (not (m/validate tc/Bencodable {:a true})) "the contract excludes booleans too")
  (is (not (m/validate tc/Bencodable {1 2})) "and numeric keys"))

(deftest tcp-server-roundtrip-test
  (testing "start-server! defaults to TCP; a client message reaches on-message; broadcast reaches the client"
    (let [got    (promise)
          server (tc/start-server! {:port 0 :on-message #(deliver got %)})]
      (try
        (let [port (.getPort ^java.net.InetSocketAddress
                    (.getLocalAddress ^java.nio.channels.ServerSocketChannel (:server-channel server)))
              cl   (tc/tcp-transport "127.0.0.1" port)]
          (is (tc/connect! cl))
          (is (tc/send! cl {:type "ping"}))
          (is (= "ping" (get (deref got 3000 nil) "type")))
          (is (= 1 (tc/client-count server)))
          (is (nil? (tc/broadcast! server {:type "pong"})))
          (is (= {"type" "pong"} (tc/recv! cl)))
          (tc/disconnect! cl))
        (finally (tc/stop-server! server)))
      (is (not (tc/server-running? server))))))

(deftest unix-transport-without-a-socket-test
  (let [t (tc/unix-transport "/tmp/nonexistent-m30.sock")]
    (is (not (tc/connected? t)))
    (is (false? (tc/connect! t)))))

;;; ---------------------------------------------------------------------------
;;; olympus
;;; ---------------------------------------------------------------------------

(hst/deftrifecta-from-schema serialize-date-contract
  hive-mcp.transport.olympus.snapshots/serialize-date
  {:in          [:or [:enum nil "2026-01-01" 5]
                 [:fn {:gen/fmap #(java.util.Date. (long %)) :gen/schema [:int {:min 0 :max 4102444800000}]}
                  inst?]]
   :out         [:maybe snap/IsoString]
   :rel         (fn [in out]
                  (cond (nil? in) (nil? out)
                        (instance? java.util.Date in) (= (.toInstant ^java.util.Date in)
                                                         (java.time.Instant/parse out))
                        :else (= (str in) out)))
   ;; Dates are not EDN-stable in a golden; the cases are pinned in
   ;; serialize-date-golden-test instead.
   :num-tests   200})

(deftest serialize-date-golden-test
  (is (= ["1970-01-01T00:00:00Z" "2026-10-20T00:00:00Z" nil "5" "x"]
         (map snap/serialize-date [(java.util.Date. 0) (java.util.Date. 1792454400000) nil 5 "x"]))))

(deftest snapshots-answer-their-schemas
  ;; Without a swarm store every builder degrades to its empty shape.
  (is (m/validate [:vector snap/AgentRow] (snap/build-agents-snapshot)))
  (is (m/validate snap/KgSnapshot (snap/build-kg-snapshot)))
  (is (m/validate snap/ProjectTreeSnapshot (snap/build-project-tree-snapshot)))
  (is (m/validate snap/FullSnapshot (snap/build-full-snapshot))))

(deftest stream-emitters-with-no-clients-are-silent-nil
  (let [prev @stream/clients]
    (reset! stream/clients #{})
    (try
      (is (nil? (stream/broadcast! {:type :agents :data []})))
      (is (nil? (stream/emit! :agents {:data []})))
      (is (nil? (stream/emit! :agents nil)))
      (is (nil? (stream/emit-agent-event! :agent-spawned {:id "a"})))
      (is (nil? (stream/emit-hivemind-shout! {:agent-id "a" :event-type "progress" :message "m"})))
      (is (nil? (stream/emit-kg-event! :memory-entry-added {:entry-id "e"})))
      (finally (reset! stream/clients prev)))))

(deftest broadcast-reaches-a-connected-client
  (let [sink (ms/stream 4)
        prev @stream/clients]
    (reset! stream/clients #{sink})
    (try
      (stream/emit-agent-event! :agent-spawned {:id "a1"})
      (let [msg @(ms/try-take! sink nil 1000 nil)]
        (is (str/includes? msg "\"type\":\"agent-spawned\""))
        (is (str/includes? msg "\"agent\":{\"id\":\"a1\"}")))
      (finally (reset! stream/clients prev)))))

(deftest state-bridge-stop-is-nil-and-idempotent
  (is (nil? (bridge/stop-ds-state-bridge!)))
  (is (nil? (bridge/stop-ds-state-bridge!))))

(deftest servers-report-stopped-status
  (is (m/validate (-> (m/function-schemas)
                      (get-in ['hive-mcp.transport.olympus.http 'status :schema])
                      m/schema m/children second)
                  (ohttp/status)))
  (is (nil? (ohttp/stop!)) "stopping a stopped olympus server answers nil")
  (is (m/validate a2a/Status (a2a/status)))
  (is (nil? (a2a/stop!)))
  (is (false? (:running? (mh/status))))
  (is (nil? (mh/stop!)))
  (is (nil? (ws/stop-server!)))
  (is (boolean? (ws/connected?))))
