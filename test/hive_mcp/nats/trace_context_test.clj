(ns hive-mcp.nats.trace-context-test
  "STD-04 O1: the W3C trace context crosses a tools/call (params._meta), a
   NATS publish (headers traceparent / tracestate) and a ling spawn (env
   TRACEPARENT). The NATS side is checked on a recording jnats Connection."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.context.request :as ctx]
            [hive-mcp.nats.client :as nc]
            [hive-mcp.server.routes.middleware :as mw]
            [hive-ttracking.tracecontext :as tc]
            [jsonrpc4clj.server :as jsonrpc-server])
  (:import [io.nats.client Connection Connection$Status]
           [io.nats.client.impl Headers NatsMessage]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def caller-tp "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")
(def caller-trace-id "4bf92f3577b34da6a3ce929d0e0e4736")
(def caller-parent-id "00f067aa0ba902b7")

;; ── Recording connection ────────────────────────────────────────────────────

(defn- recording-connection
  "A jnats Connection that reports CONNECTED and records every publish as
   {:subject :headers {name value} :body str} into SENT."
  ^Connection [sent]
  (reify Connection
    (getStatus [_] Connection$Status/CONNECTED)
    (^void publish [_ ^String subject ^bytes body]
      (swap! sent conj {:subject subject :headers nil :body (String. body "UTF-8")}))
    (^void publish [_ ^String subject ^Headers h ^bytes body]
      (swap! sent conj {:subject subject
                        :headers (into {} (for [k (.keySet h)] [k (.getFirst h k)]))
                        :body (String. body "UTF-8")}))
    (close [_])))

(def ^:dynamic *sent* nil)

(use-fixtures :each
  (fn [t]
    (let [conn-atom @#'nc/connection
          before    @conn-atom
          sent      (atom [])]
      (reset! conn-atom (recording-connection sent))
      (try (binding [*sent* sent] (t))
           (finally (reset! conn-atom before))))))

(defn- handler-publishing
  "A tool handler that publishes once, as a shout or event handler would."
  [_args]
  (nc/publish! "hive.v1.event.test" {:ok true})
  [{:type "text" :text "done"}])

(defn- call-tool-with
  "Run PARAMS through the tools/call shim into the wrapped HANDLER, the way
   the SDK hands a request to a tool handler (params.arguments only)."
  [handler params]
  (let [shim (mw/carry-trace-meta
              (fn [_method _context p] (handler (:arguments p))))]
    (shim "tools/call" {} params)))

(defn- call-tool
  [params]
  (call-tool-with (mw/wrap-handler-context handler-publishing) params))

;; ── tools/call -> NATS ──────────────────────────────────────────────────────

(deftest tools-call-meta-traceparent-reaches-nats-header
  (testing "_meta.traceparent: same trace-id, new parent-id, tracestate kept"
    (call-tool {:name "t" :arguments {:x 1}
                :meta {:traceparent caller-tp :tracestate "vendor=abc"}})
    (let [{:keys [headers body]} (last @*sent*)
          ctx (tc/<-headers headers)]
      (is (some? ctx) "the message carries a valid traceparent header")
      (is (= caller-trace-id (:trace-id ctx)))
      (is (not= caller-parent-id (:parent-id ctx)))
      (is (= "01" (:flags ctx)))
      (is (= "vendor=abc" (get headers "tracestate")))
      (is (= "{\"ok\":true}" body) "the body is unchanged"))))

(deftest tools-call-without-meta-starts-a-new-root
  (testing "no _meta: a valid new root, not the caller's trace"
    (call-tool {:name "t" :arguments {:x 1}})
    (let [{:keys [headers]} (last @*sent*)
          ctx (tc/<-headers headers)]
      (is (tc/valid? ctx))
      (is (not= caller-trace-id (:trace-id ctx)))
      (is (nil? (get headers "tracestate"))))))

(deftest tools-call-with-malformed-traceparent-starts-a-new-root
  (doseq [bad ["garbage" "00-4BF92F3577B34DA6A3CE929D0E0E4736-00f067aa0ba902b7-01"
               "ff-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
               "00-00000000000000000000000000000000-00f067aa0ba902b7-01"]]
    (reset! *sent* [])
    (call-tool {:name "t" :arguments {} :meta {:traceparent bad}})
    (let [ctx (tc/<-headers (:headers (last @*sent*)))]
      (is (tc/valid? ctx) bad)
      (is (not= caller-trace-id (:trace-id ctx)) bad))))

(deftest trace-arg-keys-never-reach-the-handler
  (let [seen (atom nil)
        h    (mw/wrap-handler-context (fn [args] (reset! seen args) [{:type "text" :text "ok"}]))]
    (h {:x 1 :_traceparent caller-tp :_tracestate "a=b"})
    (is (= {:x 1} (select-keys @seen [:x :_traceparent :_tracestate])))))

(deftest trace-meta->args-reads-both-meta-spellings
  (is (= {:traceparent caller-tp} (mw/trace-meta->args {:_meta {:traceparent caller-tp}})))
  (is (= {:traceparent caller-tp :tracestate "a=b"}
         (mw/trace-meta->args {:meta {"traceparent" caller-tp "tracestate" "a=b"}})))
  (is (nil? (mw/trace-meta->args {:arguments {:a 1}})))
  (testing "total over shapes it does not understand"
    (doseq [p [nil [] "s" 5 {:meta "x"} {:meta {:traceparent 5}}]]
      (is (nil? (mw/trace-meta->args p)) (pr-str p)))))

(deftest carry-trace-meta-is-idempotent
  (let [orig (fn [_ _ p] p)
        once (mw/carry-trace-meta orig)]
    (is (identical? once (mw/carry-trace-meta once)))))

;; ── NATS without a bound context; subscriber binding ────────────────────────

(deftest publish-outside-a-trace-sends-no-header
  (nc/publish! "hive.v1.event.test" {:ok true})
  (is (nil? (:headers (last @*sent*)))))

(deftest inbound-headers-bind-the-trace-for-the-handler
  (let [h   (doto (Headers.) (.add "traceparent" (into-array String [caller-tp])))
        msg (-> (NatsMessage/builder) (.subject "s") (.headers h)
                (.data (.getBytes "{}" "UTF-8")) (.build))
        seen (atom nil)]
    (#'nc/call-with-trace (nc/inbound-trace msg) #(reset! seen (:trace (ctx/request-ctx))))
    (is (= caller-trace-id (:trace-id @seen)))
    (is (= caller-parent-id (:parent-id @seen))))
  (testing "a malformed inbound traceparent binds nothing"
    (let [h   (doto (Headers.) (.add "traceparent" (into-array String ["nope"])))
          msg (-> (NatsMessage/builder) (.subject "s") (.headers h)
                  (.data (.getBytes "{}" "UTF-8")) (.build))]
      (is (nil? (nc/inbound-trace msg))))))

;; ── ling spawn env ──────────────────────────────────────────────────────────

(deftest spawn-env-carries-traceparent
  (let [env (ctx/with-request-context {:trace (tc/parse caller-tp)}
              (headless/build-child-env "ling-x" {:cwd "/tmp"}))
        ctx (tc/parse (get env "TRACEPARENT"))]
    (is (= caller-trace-id (:trace-id ctx)))
    (is (not= caller-parent-id (:parent-id ctx))))
  (testing "no bound trace: no TRACEPARENT"
    (is (not (contains? (headless/build-child-env "ling-x" {:cwd "/tmp"}) "TRACEPARENT")))))

;; ── Review fixes (fail open, nesting, live reload) ─────────────────────────

(deftest non-map-arguments-with-meta-pass-through-untouched
  (doseq [arguments [[1 2] "str" 5 nil]]
    (let [seen (atom ::none)
          shim (mw/carry-trace-meta (fn [_ _ p] (reset! seen p) :ok))
          params {:name "t" :arguments arguments :meta {:traceparent caller-tp}}]
      (is (= :ok (shim "tools/call" {} params)) (pr-str arguments))
      (is (= params @seen) "params reach the SDK method unchanged")))
  (testing "non-map params"
    (let [shim (mw/carry-trace-meta (fn [_ _ p] p))]
      (doseq [p [nil [] "x"]] (is (= p (shim "tools/call" {} p)))))))

(defn- refusing-connection
  "Recording Connection whose headers publish throws, like jnats against a
   server without header support."
  ^Connection [sent]
  (reify Connection
    (getStatus [_] Connection$Status/CONNECTED)
    (^void publish [_ ^String subject ^bytes body]
      (swap! sent conj {:subject subject :headers nil :body (String. body "UTF-8")}))
    (^void publish [_ ^String _subject ^Headers _h ^bytes _body]
      (throw (IllegalArgumentException. "Headers are not supported by the server")))
    (close [_])))

(deftest header-refusing-publish-still-delivers
  (let [conn-atom @#'nc/connection
        sent (atom [])]
    (reset! conn-atom (refusing-connection sent))
    (ctx/with-request-context {:trace (tc/parse caller-tp)}
      (nc/publish! "hive.v1.event.test" {:ok true}))
    (is (= [{:subject "hive.v1.event.test" :headers nil :body "{\"ok\":true}"}] @sent))))

(deftest header-build-failure-still-delivers
  (with-redefs [nc/trace-headers (fn [_] (throw (RuntimeException. "boom")))]
    (ctx/with-request-context {:trace (tc/parse caller-tp)}
      (nc/publish! "hive.v1.event.test" {:ok true})))
  (is (= [{:subject "hive.v1.event.test" :headers nil :body "{\"ok\":true}"}] @*sent*)))

(deftest nested-call-is-a-child-of-the-bound-trace
  (let [inner-trace (atom nil)
        inner (mw/wrap-handler-context
               (fn [_] (reset! inner-trace (:trace (ctx/request-ctx))) [{:type "text" :text "in"}]))
        outer-trace (atom nil)
        outer (mw/wrap-handler-context
               (fn [_] (reset! outer-trace (:trace (ctx/request-ctx))) (inner {:y 2})))]
    (call-tool-with outer {:name "t" :arguments {:x 1} :meta {:traceparent caller-tp}})
    (is (= caller-trace-id (:trace-id @outer-trace)))
    (is (= caller-trace-id (:trace-id @inner-trace)) "same trace-id")
    (is (not= (:parent-id @outer-trace) (:parent-id @inner-trace)) "new span id")))

(deftest request-trace-failure-means-no-trace-and-the-call-proceeds
  (with-redefs [tc/new-root (fn [& _] (throw (RuntimeException. "rng down")))]
    (let [seen (atom ::none)
          h (mw/wrap-handler-context (fn [_] (reset! seen (:trace (ctx/request-ctx))) [{:type "text" :text "ok"}]))]
      (is (= [{:type "text" :text "ok"}] (h {:x 1})))
      (is (nil? @seen)))))

(deftest client-trace-args-cannot-set-the-trace-nor-reach-a-tool
  (let [seen (atom nil)
        h (mw/wrap-handler-context
           (fn [args] (reset! seen [args (:trace (ctx/request-ctx))]) [{:type "text" :text "ok"}]))]
    (h {:x 1 :_traceparent caller-tp :_tracestate "a=b"})
    (let [[args trace] @seen]
      (is (= {:x 1} (select-keys args [:x :_traceparent :_tracestate])))
      (is (not= caller-trace-id (:trace-id trace)) "an argument is not a carrier"))))

(deftest install-is-idempotent-across-reloads
  (let [mf jsonrpc-server/receive-request
        before (get (methods mf) "tools/call")
        base (fn [_ _ p] p)]
    (try
      (.addMethod ^clojure.lang.MultiFn mf "tools/call" base)
      (is (true? (mw/install-trace-meta!)))
      (let [once (get (methods mf) "tools/call")]
        (is (true? (mw/install-trace-meta!)))
        (require 'hive-mcp.server.routes.middleware :reload)
        (is (identical? once (get (methods mf) "tools/call")) "wrapped once"))
      (finally (.addMethod ^clojure.lang.MultiFn mf "tools/call" before)))))

(deftest strict-tool-works-right-after-install-before-any-refresh
  (testing "a tool rejecting unknown keys, built BEFORE install, behind the SDK method"
    (let [strict (fn [args]
                   (when-let [extra (seq (remove #{:x} (keys args)))]
                     (throw (ex-info "unknown params" {:extra extra})))
                   [{:type "text" :text "ok"}])
          sdk-method (fn [_ _ params] (strict (:arguments params)))
          installed (mw/carry-trace-meta sdk-method)]
      (is (= [{:type "text" :text "ok"}]
             (installed "tools/call" {} {:name "t" :arguments {:x 1}
                                         :meta {:traceparent caller-tp}}))))))
