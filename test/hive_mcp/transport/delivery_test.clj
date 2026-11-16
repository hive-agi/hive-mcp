(ns hive-mcp.transport.delivery-test
  "The :delivery chain: target resolution first, then
   [:nats-inbox :tmux :emacs :hivemind-shout] with the chain annotation.
   Every transport is a scripted port; nothing real is touched."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-dsl.result :as r]
            [hive-mcp.nats.inbox :as inbox]
            [hive-mcp.tools.agent.dispatch :as dispatch]
            [hive-mcp.tools.consolidated.hivemind :as hivemind]
            [hive-mcp.transport.chain :as chain]
            [hive-mcp.transport.delivery :as delivery]
            [hive-mcp.transport.registry :as reg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private fast
  {:retry {:attempts 1 :backoff-ms [0]} :probe-interval-ms 0})

(def ^:private transports
  {:defaults fast
   :delivery {:chain [:nats-inbox :tmux :emacs :hivemind-shout]}})

(def ^:private sent (atom []))

(defn- world
  "Run f with the ports scripted: :nats? whether a connection exists, :armed?
   whether the coordinator's inbox is armed, :tmux? whether the pane takes it."
  [{:keys [nats? armed? tmux? lings]} f]
  (reg/clear!)
  (reset! sent [])
  (reg/rebind! transports)
  (with-redefs [inbox/status (fn [_ id] {:inbox id :armed? (boolean armed?)})
                inbox/send!  (fn [_ msg] (swap! sent conj [:nats msg])
                               {:id "m1" :to (:to msg) :subject "s" :seq 1 :duplicate? false})]
    (binding [delivery/*connection*    (fn [] (when nats? :conn))
              delivery/*find-ling*     (fn [id] (get lings id))
              delivery/*tmux-reply*    (fn [to msg]
                                         (swap! sent conj [:tmux to msg])
                                         (if tmux? {:ok true :routed :tmux :to to :delivered? true}
                                             {:ok false :error "no pane"}))
              delivery/*ling-dispatch* (fn [p] (swap! sent conj [:emacs (:agent_id p)])
                                         {:task-id "t1"})
              delivery/*shout*         (fn [to msg] (swap! sent conj [:shout to msg])
                                         {:delivered true :shout-id "s1" :routing :direct :to to})]
      (try (f) (finally (reg/clear!))))))

(use-fixtures :each (fn [t] (reg/clear!) (t) (reg/clear!)))

(deftest target-kinds
  (is (= :coordinator (delivery/target-kind "coordinator:134462")))
  (binding [delivery/*find-ling* {"ling-1" {:slave/id "ling-1"}}]
    (is (nil? (delivery/target-kind "coordinator:abc")) "not digits, not a registered ling")
    (is (nil? (delivery/target-kind "")))
    (is (= :ling (delivery/target-kind "ling-1")))
    (is (nil? (delivery/target-kind "nobody")))))

(deftest coordinator-with-armed-inbox-lands-via-nats-inbox
  (world {:nats? true :armed? true :tmux? true}
         (fn []
           (let [res (delivery/deliver! {:to "coordinator:134462" :message "wake"})
                 v   (:ok res)]
             (is (r/ok? res))
             (is (= :nats-inbox (:via v)))
             (is (= :primary (:resolved-by v)))
             (is (= [:nats] (mapv first @sent)))
             (is (= "coordinator-134462" (get-in @sent [0 1 :to])))))))

(deftest nats-down-lands-via-tmux-and-says-fallback
  (world {:nats? false :tmux? true}
         (fn []
           (let [res (delivery/deliver! {:to "coordinator:134462" :message "wake"})
                 v   (:ok res)]
             (is (r/ok? res))
             (is (= :tmux (:via v)))
             (is (= :fallback (:resolved-by v)))
             (is (= [:nats-inbox] (mapv :adapter (:skipped v))))
             (is (= [:tmux] (mapv first @sent)))))))

(deftest unarmed-inbox-is-skipped-not-degraded
  (world {:nats? true :armed? false :tmux? true}
         (fn []
           (let [res (delivery/deliver! {:to "coordinator:134462" :message "wake"})]
             (is (= :tmux (get-in res [:ok :via])))
             (is (= :inapplicable (get-in res [:ok :skipped 0 :reason])))
             (is (not= :degraded
                       (get-in (reg/status) [:ports :delivery :adapters :nats-inbox :status])))))))

(deftest every-coordinator-route-down-ends-in-the-shout
  (world {:nats? false :tmux? false}
         (fn []
           (let [res (delivery/deliver! {:to "coordinator:134462" :message "wake"})]
             (is (= :hivemind-shout (get-in res [:ok :via])))
             (is (= :fallback (get-in res [:ok :resolved-by])))))))

(deftest ling-goes-through-the-terminal-path
  (world {:nats? true :armed? true :tmux? true :lings {"ling-1" {:slave/id "ling-1"}}}
         (fn []
           (let [res (delivery/deliver! {:to "ling-1" :message "m"
                                         :params {:agent_id "ling-1" :prompt "m"}})]
             (is (= :emacs (get-in res [:ok :via])))
             (is (= [[:emacs "ling-1"]] @sent))))))

(deftest unknown-target-is-an-addressing-error-with-no-fallback
  (world {:nats? true :armed? true :tmux? true}
         (fn []
           (let [res (delivery/deliver! {:to "nobody-here" :message "m"})]
             (is (r/err? res))
             (is (chain/addressing? res))
             (is (empty? @sent) "no adapter ran, the shout included")))))

(deftest handle-dispatch-and-nudge-return-the-chain-annotation
  (world {:nats? false :tmux? true}
         (fn []
           (let [out  (dispatch/handle-dispatch {:agent_id "coordinator:134462" :prompt "wake"})
                 body (json/read-str (:text out) :key-fn keyword)]
             (is (not (:isError out)))
             (is (= "tmux" (:via body)))
             (is (= "fallback" (:resolved-by body)))
             (is (= [{:adapter "nats-inbox" :reason "failed"}]
                    (mapv #(select-keys % [:adapter :reason]) (:skipped body)))))
           (let [out (hivemind/handle-nudge {:agent_id "coordinator:134462"})]
             (is (not (:isError out)))
             (is (= "tmux" (:via (json/read-str (:text out) :key-fn keyword)))))
           (testing "an unknown id is an addressing error"
             (let [out (dispatch/handle-dispatch {:agent_id "ghost" :prompt "x"})]
               (is (true? (:isError out)))
               (is (= "Agent not found: ghost" (:text out))))))))
