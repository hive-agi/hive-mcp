(ns hive-mcp.nats.trace-context-test
  "STD-04 O1: the W3C trace context crosses a tools/call (params._meta), a
   NATS publish (headers traceparent / tracestate) and a ling spawn (env
   TRACEPARENT). The NATS side is checked on a recording jnats Connection."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.context.request :as ctx]
            [hive-mcp.nats.client :as nc]
            [hive-mcp.server.routes.middleware :as mw]
            [hive-ttracking.tracecontext :as tc])
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

(defn- call-tool
  "Run ARGS through the tools/call shim and wrap-handler-context, the way the
   SDK hands a request to a tool handler."
  [params]
  (let [shim (mw/carry-trace-meta
              (fn [_method _context p] ((mw/wrap-handler-context handler-publishing)
                                        (:arguments p))))]
    (shim "tools/call" {} params)))

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
  (is (= caller-tp (:_traceparent (mw/trace-meta->args {:_meta {:traceparent caller-tp}} {}))))
  (is (= caller-tp (:_traceparent (mw/trace-meta->args {:meta {:traceparent caller-tp}} {}))))
  (is (= {:a 1} (mw/trace-meta->args {} {:a 1}))))

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
