(ns hive-mcp.nats.inbox-consumer
  "In-process consumer port on HIVE_INBOX: `consume!` hands every message of
   one inbox to a handler in this JVM, acking only what the handler took
   (card 20261116182711-40333bab). It is the ONE port the in-process readers
   share, so they are not built twice: a live ling run (hive-agent soft-
   resolves `consume!` and offers each message to its loop inbox), a
   hive-scripts monitor (3c7c6043), and the :inbox await source (RRS-S3
   24d04aba).

   The reader is a long poll on the inbox's durable
   (hive-mcp.nats.inbox/fetch! with a wait), so a message sent to an idle
   inbox is handed over as soon as the broker has it, not on a 30 s tick.

   The handler answers, per message:
     :ack   taken: acked, never redelivered
     :nak   not taken now: nak'd, the broker redelivers it (after `nak-ms`
            of local backoff, so a refusing handler cannot spin)
     :stop  not taken, and stop consuming: nak'd, and so is the rest of the
            batch, so the message waits in the durable for the next reader.
            A ling whose run has ended answers :stop.
   A handler that throws counts as :nak. Anything else counts as :ack, so a
   plain side-effecting fn just works.

   Strata (CPPB):
     Value objects  Verdict, ConsumerStatus
     Pure           verdict
     Ports          source (fn [inbox {:keys [n wait-ms]}]) -> [Fetched];
                    arm! (fn [inbox]); both default to the hive NATS client
     Boundary       consume!, consumers, stop-all!"
  (:require [hive-mcp.nats.client :as client]
            [hive-mcp.nats.inbox :as inbox]
            [hive-mcp.resilience.swallow :as swallow]
            [malli.core :as m]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Value objects and pure
;; =============================================================================

(def Verdict [:enum :ack :nak :stop])

(def ConsumerStatus
  [:map
   [:inbox :string]
   [:running? :boolean]
   [:acked :int] [:naked :int] [:failed :int]
   [:stopped-by {:optional true} [:maybe :keyword]]
   [:last-error {:optional true} [:maybe :string]]])

(defn verdict
  "The Verdict a handler's answer means: :nak and :stop are themselves,
   everything else (true, nil, a map) is :ack."
  [answer]
  (if (contains? #{:nak :stop} answer) answer :ack))

(defn offer-handler
  "A handler over an `offer!`-style fn `(offer msg) -> boolean`, such as
   hive-agent.loop.inbox/offer! over a live run's inbox: an accepted offer
   is :ack, a refused one (the run has ended) is :stop, so the message
   stays unacked in the durable for whoever reads the inbox next. `->msg`
   (default identity) maps the delivery to what the run is offered."
  ([offer] (offer-handler offer identity))
  ([offer ->msg]
   (fn [delivered]
     (if (offer (->msg delivered)) :ack :stop))))

(m/=> offer-handler [:function
                     [:=> [:cat ifn?] ifn?]
                     [:=> [:cat ifn? ifn?] ifn?]])

(m/=> verdict [:=> [:cat :any] Verdict])

;; =============================================================================
;; Default ports: the hive NATS client
;; =============================================================================

(defn nats-source
  "`inbox/fetch!` over the live connection; throws :inbox/no-bus without one."
  [id opts]
  (if-let [conn (client/current-connection)]
    (inbox/fetch! conn id opts)
    (throw (ex-info "no connected event bus" {:error :inbox/no-bus}))))

(m/=> nats-source [:=> [:cat :string :map] [:vector inbox/Fetched]])

(defn nats-arm!
  "`inbox/arm!` over the live connection; throws :inbox/no-bus without one."
  [id]
  (if-let [conn (client/current-connection)]
    (inbox/arm! conn id)
    (throw (ex-info "no connected event bus" {:error :inbox/no-bus}))))

(m/=> nats-arm! [:=> [:cat :string] :map])

;; =============================================================================
;; Boundary
;; =============================================================================

(defonce ^:private consumers* (atom {}))

(defn- nak-all! [fetched]
  (doseq [{:keys [nak!]} fetched] (swallow/attempt (nak!))))

(defn- run-batch!
  "Hand `fetched` to `handler` in order. -> :continue | :stop | :backoff"
  [handler fetched stats*]
  (loop [[f & more :as todo] fetched]
    (if (nil? f)
      :continue
      (let [v (try (verdict (handler (:message f)))
                   (catch Throwable t
                     (swap! stats* assoc :last-error (or (.getMessage t) (str (class t))))
                     (swap! stats* update :failed inc)
                     :nak))]
        (case v
          :ack  (do (if (swallow/attempt ((:ack! f)))
                      (swap! stats* update :acked inc)
                      (swap! stats* update :failed inc))
                    (recur more))
          :nak  (do (nak-all! todo)
                    (swap! stats* update :naked + (count todo))
                    :backoff)
          :stop (do (nak-all! todo)
                    (swap! stats* update :naked + (count todo))
                    :stop))))))

(defn consume!
  "Hand every message of inbox `id` to `(handler delivered)` on a daemon
   thread, until the returned stop fn is called or the handler answers
   :stop. The inbox is armed first (idempotent; arming keeps the broker's
   cursor). Options:
     :source  :arm!   the ports (default the hive NATS client)
     :batch   messages per fetch (default 16)
     :wait-ms long-poll per fetch (default 10000)
     :retry-ms wait after a failed fetch, e.g. no bus (default 2000)
     :nak-ms  backoff after a :nak or a throw (default 1000)
     :on-stop (fn [reason]) called once when it stops (:stopped | :handler)
   A second `consume!` of the same inbox stops the first. Returns a stop fn
   (0-arity, idempotent, answers nil)."
  ([id handler] (consume! id handler nil))
  ([raw-id handler {:keys [source arm! batch wait-ms retry-ms nak-ms on-stop]}]
   (let [id       (inbox/encode-inbox-id raw-id)
         source   (or source nats-source)
         arm!     (or arm! nats-arm!)
         batch    (or batch 16)
         wait-ms  (or wait-ms 10000)
         retry-ms (or retry-ms 2000)
         nak-ms   (or nak-ms 1000)
         run?     (atom true)
         stats*   (atom {:acked 0 :naked 0 :failed 0 :last-error nil :stopped-by nil})
         token    (Object.)
         finish!  (fn [reason]
                    (when (compare-and-set! run? true false)
                      (swap! stats* assoc :stopped-by reason)
                      (swap! consumers* (fn [m] (if (= token (get-in m [id :token])) (dissoc m id) m)))
                      (when on-stop (swallow/attempt (on-stop reason)))))
         pump     (fn []
                    (loop [armed? false]
                      (when @run?
                        (let [armed? (or armed?
                                         (try (arm! id) true
                                              (catch Throwable t
                                                (swap! stats* assoc :last-error (.getMessage t))
                                                (Thread/sleep (long retry-ms))
                                                false)))]
                          (if-not armed?
                            (recur false)
                            (let [fetched (try (source id {:n batch :wait-ms wait-ms})
                                               (catch Throwable t
                                                 (swap! stats* assoc :last-error (.getMessage t))
                                                 (Thread/sleep (long retry-ms))
                                                 ::failed))]
                              (cond
                                (= ::failed fetched) (recur false)
                                (not @run?)          (nak-all! fetched)
                                :else
                                (case (run-batch! handler fetched stats*)
                                  :continue (recur true)
                                  :backoff  (do (Thread/sleep (long nak-ms)) (recur true))
                                  :stop     (finish! :handler)))))))))
         stop!    (fn [] (finish! :stopped) nil)]
     (when-let [old (get @consumers* id)] ((:stop! old)))
     (swap! consumers* assoc id {:token token :stop! stop! :stats* stats* :run? run?})
     (doto (Thread. ^Runnable (fn []
                                (try (pump)
                                     (catch Throwable t
                                       (log/warn "[inbox-consumer]" id "ended:" (.getMessage t)))
                                     (finally (finish! :stopped))))
                    (str "hive-inbox-consumer-" id))
       (.setDaemon true)
       (.start))
     stop!)))

(m/=> consume! [:function
                [:=> [:cat :string ifn?] ifn?]
                [:=> [:cat :string ifn? [:maybe :map]] ifn?]])

(defn consumers
  "Every running consumer's status."
  []
  (mapv (fn [[id {:keys [stats* run?]}]]
          (assoc (select-keys @stats* [:acked :naked :failed :stopped-by :last-error])
                 :inbox id :running? (boolean @run?)))
        @consumers*))

(m/=> consumers [:=> [:cat] [:vector ConsumerStatus]])

(defn stop-all!
  "Stop every consumer. Returns nil."
  []
  (doseq [[_ {:keys [stop!]}] @consumers*] (stop!))
  nil)

(m/=> stop-all! [:=> [:cat] :nil])
