(ns hive-mcp.nats.inbox-stream-test
  "hive-mcp.nats.inbox-stream through its source port: an in-test broker
   with explicit ack/nak (no with-redefs, no NATS). Acceptance of card
   20261116182711-093a70b4: missing/wrong token refused, non-loopback
   refused, 3 messages -> 3 lines in order acked only after the write,
   disconnect -> reconnect delivers the unacked with no dup, status shows
   the reader."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.nats.inbox-stream :as is]
            [hive-mcp.tools.consolidated.inbox :as tool]
            [malli.core :as m]
            [manifold.stream :as s]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; In-test broker: unacked messages are redelivered after a nak
;; =============================================================================

(defn- broker []
  (let [q* (atom (sorted-map)) inflight* (atom #{}) acked* (atom [])]
    {:q* q* :acked* acked*
     :send! (fn [seq body] (swap! q* assoc seq {:seq seq :id (str "m" seq) :body body}))
     :source
     (fn [_id {:keys [n wait-ms]}]
       (let [ready (->> @q* vals (remove #(@inflight* (:seq %))) (take n) vec)]
         (if (empty? ready)
           (do (Thread/sleep (long (min 20 (or wait-ms 20)))) [])
           (do (swap! inflight* into (map :seq ready))
               (mapv (fn [msg]
                       {:message msg
                        :ack!    (fn [] (swap! q* dissoc (:seq msg))
                                   (swap! inflight* disj (:seq msg))
                                   (swap! acked* conj (:seq msg)) true)
                        :nak!    (fn [] (swap! inflight* disj (:seq msg)) true)})
                     ready)))))}))

(defn- req
  ([id token] (req id token "127.0.0.1"))
  ([id token addr]
   {:request-method :get :uri (str "/inbox/" id ".sse")
    :query-string (when token (str "token=" token)) :remote-addr addr :headers {}}))

(defn- frames
  "Take from `out` until `n` data frames arrived (or `ms` passed)."
  [out n ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop [acc []]
      (if (or (>= (count acc) n) (> (System/currentTimeMillis) deadline))
        acc
        (let [x @(s/try-take! out ::none 100 ::none)]
          (cond
            (= ::none x) (if (s/drained? out) acc (recur acc))
            (str/starts-with? x "id: ") (recur (conj acc x))
            :else (recur acc)))))))

(defn- data-of [frame]
  (json/read-str (second (re-find #"(?m)^data: (.*)$" frame)) :key-fn keyword))

(defn- wait-until [pred ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (while (and (not (pred)) (< (System/currentTimeMillis) deadline)) (Thread/sleep 10))
    (pred)))

(defn- clean [f]
  (is/forget-tokens!)
  (is/configure! nil)
  (try (f) (finally (is/forget-tokens!) (is/configure! nil))))

(use-fixtures :each clean)

;; =============================================================================
;; Pure
;; =============================================================================

(deftest routes-and-queries-parse
  (is (= "pedro" (is/parse-route "/inbox/pedro.sse")))
  (is (= "coordinator-12" (is/parse-route "/inbox/coordinator-12.sse")))
  (is (nil? (is/parse-route "/inbox/pedro")))
  (is (nil? (is/parse-route "/inbox/a/b.sse")))
  (is (= {"token" "a b" "x" ""} (is/parse-query "token=a%20b&x=")))
  (is (= {} (is/parse-query nil))))

(deftest loopback-is-the-only-address-allowed
  (is (is/loopback? "127.0.0.1"))
  (is (is/loopback? "::1"))
  (is (is/loopback? "0:0:0:0:0:0:0:1"))
  (is (not (is/loopback? "192.168.1.5")))
  (is (not (is/loopback? "localhost")) "a name is never resolved")
  (is (not (is/loopback? nil))))

(deftest tokens-compare-in-constant-time-and-nil-never-matches
  (is (is/token= "abc" "abc"))
  (is (not (is/token= "abc" "abd")))
  (is (not (is/token= nil nil)))
  (is (not (is/token= "abc" nil))))

(deftest a-frame-is-one-data-line-under-the-cap
  (let [f (is/frame {:seq 7 :id "m7" :body {:text "two\nlines"}})]
    (is (str/starts-with? f "id: 7\nevent: inbox\ndata: "))
    (is (str/ends-with? f "\n\n"))
    (is (= 1 (count (re-seq #"(?m)^data: " f))) "a newline in the body stays escaped")
    (is (= "two\nlines" (get-in (data-of f) [:body :text]))))
  (let [big (apply str (repeat 5000 "x"))
        f   (is/frame {:seq 1 :body big} 2048)]
    (is (<= (count (.getBytes ^String f "UTF-8")) (+ 2048 64)))
    (is (true? (:truncated (data-of f))))))

(deftest the-monitor-spec-names-the-url-with-its-token
  (let [spec (is/monitor-spec {:port 9999 :id "pedro" :token "t0k"})]
    (is (m/validate is/Monitor spec))
    (is (= "http://127.0.0.1:9999/inbox/pedro.sse?token=t0k" (:url spec)))
    (is (str/includes? (:command spec) "curl -sN"))
    (is (str/includes? (:plugin-command spec) "while"))))

(deftest the-reminder-fires-only-after-60s-without-a-reader
  (is (nil? (is/reminder "p" {:issued-at 0 :readers 0} 60000)))
  (is (string? (is/reminder "p" {:issued-at 0 :readers 0} 60001)))
  (is (nil? (is/reminder "p" {:issued-at 0 :readers 1} 999999)) "a reader is connected")
  (is (nil? (is/reminder "p" {:readers 0} 999999)) "no Monitor was ever issued")
  (is (nil? (is/reminder "p" {:issued-at 0 :readers 0 :last-left-at 50000} 100000))
      "counted from when the last reader left"))

;; =============================================================================
;; Authorization
;; =============================================================================

(deftest the-stream-refuses-without-the-right-token-or-from-afar
  (let [tok (is/issue-token! "pedro")]
    (is (= tok (is/issue-token! "pedro")) "one token per inbox")
    (is (not= tok (is/issue-token! "other")) "per inbox, not global")
    (is (= 401 (:status (is/handler (req "pedro" nil)))) "missing")
    (is (= 401 (:status (is/handler (req "pedro" "nope")))) "wrong")
    (is (= 401 (:status (is/handler (req "other" tok)))) "another inbox's token")
    (is (= 401 (:status (is/handler (req "never-armed" tok)))) "no token issued")
    (is (= 403 (:status (is/handler (req "pedro" tok "10.0.0.2")))) "not loopback")
    (is (= 403 (:status (is/handler (assoc-in (req "pedro" tok) [:headers "origin"]
                                              "https://evil.example")))))
    (is (= 405 (:status (is/handler (assoc (req "pedro" tok) :request-method :post)))))
    (is (= 404 (:status (is/handler (assoc (req "pedro" tok) :uri "/inbox/a.b.sse")))))))

;; =============================================================================
;; The stream
;; =============================================================================

(deftest three-messages-are-three-lines-in-order-acked-after-the-write
  (let [{:keys [send! source acked*]} (broker)]
    (is/configure! {:source source :wait-ms 20 :write-timeout-ms 2000})
    (doseq [i [1 2 3]] (send! i {:n i}))
    (let [tok  (is/issue-token! "pedro")
          resp (is/handler (req "pedro" tok))
          out  (:body resp)]
      (is (= 200 (:status resp)))
      (is (str/starts-with? (get-in resp [:headers "Content-Type"]) "text/event-stream"))
      (is (empty? @acked*) "nothing is acked before the reader takes it")
      (let [fs (frames out 3 3000)]
        (is (= [1 2 3] (mapv (comp :n :body data-of) fs)))
        (is (wait-until #(= [1 2 3] @acked*) 2000)))
      (is (= 1 (:readers (is/stream-status "pedro"))))
      (is (= 3 (:delivered (is/stream-status "pedro"))))
      (s/close! out)
      (is (wait-until #(zero? (:readers (is/stream-status "pedro"))) 3000)))))

(deftest a-disconnect-leaves-the-rest-unacked-and-a-reconnect-gets-them-once
  (let [{:keys [send! source acked*]} (broker)]
    (is/configure! {:source source :wait-ms 20 :write-timeout-ms 300})
    (doseq [i [1 2 3]] (send! i {:n i}))
    (let [tok (is/issue-token! "pedro")
          out (:body (is/handler (req "pedro" tok)))]
      (is (= [1] (mapv (comp :n :body data-of) (frames out 1 3000))))
      (s/close! out)
      (is (wait-until #(zero? (:readers (is/stream-status "pedro"))) 3000))
      (is (= [1] @acked*) "only what was written is acked")
      (testing "reconnect: the unacked rest, in order, nothing twice"
        (let [out2 (:body (is/handler (req "pedro" tok)))]
          (is (= [2 3] (mapv (comp :n :body data-of) (frames out2 2 3000))))
          (is (wait-until #(= [1 2 3] @acked*) 2000))
          (s/close! out2))))))

(deftest an-idle-stream-heartbeats-and-a-failing-source-retries
  (let [calls (atom 0)]
    (is/configure! {:source (fn [_ _] (swap! calls inc) (throw (ex-info "no bus" {})))
                    :retry-ms 10 :write-timeout-ms 1000})
    (let [out (:body (is/handler (req "pedro" (is/issue-token! "pedro"))))
          xs  (doall (repeatedly 4 #(deref (s/try-take! out ::none 1000 ::none))))]
      (is (some #(and (string? %) (str/starts-with? % ": hb ")) xs))
      (is (>= @calls 2) "the source is retried, the stream stays open")
      (s/close! out))))

(deftest status-shows-the-reader-and-reminds-when-it-is-gone
  (is/issue-token! "pedro")
  (let [now (System/currentTimeMillis)
        st  (is/stream-status "pedro" (+ now 61000))]
    (is (m/validate is/StreamStatus st))
    (is (true? (:monitor-issued? st)))
    (is (zero? (:readers st)))
    (is (string? (:reminder st))))
  (is (= {:inbox "unknown" :readers 0 :monitor-issued? false} (is/stream-status "unknown"))))

;; =============================================================================
;; Tool layer
;; =============================================================================

(deftest arm-hands-out-a-monitor-or-says-why-not
  (is (= false (:available? (tool/monitor-for "pedro" nil))))
  (let [mon (tool/monitor-for "pedro" 9999)]
    (is (:available? mon))
    (is (str/includes? (:command mon) (str "token=" (is/issue-token! "pedro"))))))
