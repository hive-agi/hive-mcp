(ns hive-mcp.transport.delivery-test
  "The :delivery chain: target resolution first, then
   [:nats-inbox :tmux :headless-ling :emacs :hivemind-shout] with the chain
   annotation, coordinators and lings each in their own lane.
   Every transport is a scripted port; nothing real is touched."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.extensions.soft :as soft]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-dsl.result :as r]
            [hive-mcp.extensions.soft :as soft]
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
   :delivery {:chain [:nats-inbox :tmux :headless-ling :emacs :hivemind-shout]}})

(def ^:private sent (atom []))

(defn- world
  "Run f with the ports scripted: :nats? whether a connection exists, :armed?
   whether the coordinator's inbox is armed, :tmux? whether the pane takes it."
  [{:keys [nats? armed? tmux? lings backends]} f]
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
              delivery/*ling-dispatch* (fn [p]
                                         (when (str/starts-with? (str (:agent_id p)) "down")
                                           (throw (ex-info "no Emacs daemon answers" {})))
                                         (swap! sent conj [:ling (:agent_id p)])
                                         {:task-id "t1"})
              delivery/*headless-backends* (fn [] (or backends #{:hive-agent}))
              delivery/*io-model*      {:claude :buffer :vterm :buffer :tmux :buffer
                                        :headless :stdin-stdout}
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

(deftest terminal-ling-goes-through-the-emacs-path
  (world {:nats? true :armed? true :tmux? true
          :lings {"ling-1" {:slave/id "ling-1" :ling/spawn-mode :vterm}}}
         (fn []
           (let [res (delivery/deliver! {:to "ling-1" :message "m"
                                         :params {:agent_id "ling-1" :prompt "m"}})]
             (is (= :emacs (get-in res [:ok :via])))
             (is (= [{:adapter :headless-ling :reason :inapplicable}]
                    (mapv #(select-keys % [:adapter :reason]) (get-in res [:ok :skipped])))
                 "the ling lane never walks the coordinator adapters")
             (is (= [[:ling "ling-1"]] @sent))))))

(deftest a-ling-with-no-spawn-mode-is-a-terminal-claude-ling
  (world {:lings {"ling-0" {:slave/id "ling-0"}}}
         (fn []
           (is (= :emacs (get-in (delivery/deliver! {:to "ling-0" :message "m"
                                                    :params {:agent_id "ling-0"}})
                                 [:ok :via]))))))

(deftest headless-ling-goes-via-headless-ling-with-emacs-down
  (world {:nats? true :armed? true :tmux? true
          :lings {"hl-1" {:slave/id "hl-1" :ling/spawn-mode :hive-agent}
                  "hl-2" {:slave/id "hl-2" :ling/spawn-mode "headless"}}}
         (fn []
           (binding [soft/*resolve* (fn [sym]
                                      (when (= sym 'hive-mcp.emacs.client/emacs-running?)
                                        (constantly false)))]
             (testing "the probes: Emacs is down, headless-ling checks the backend registry"
               (is (r/err? ((:probe (reg/open-adapter :delivery :emacs)))))
               (is (r/ok? ((:probe (reg/open-adapter :delivery :headless-ling)))))
               (binding [delivery/*headless-backends* (constantly #{})]
                 (is (r/err? ((:probe (reg/open-adapter :delivery :headless-ling)))))))
             (doseq [id ["hl-1" "hl-2"]]
               (let [res (delivery/deliver! {:to id :message "m" :params {:agent_id id}})]
                 (is (= :headless-ling (get-in res [:ok :via])) id)
                 (is (= :primary (get-in res [:ok :resolved-by]))
                     "headless-ling heads the ling lane")
                 (is (empty? (get-in res [:ok :skipped])))))
             (testing "a probe round with Emacs down leaves the ling lane on headless-ling"
               (reg/probe-port! :delivery {:force? true})
               (is (= :headless-ling
                      (get-in (delivery/deliver! {:to "hl-1" :message "m" :params {:agent_id "hl-1"}})
                              [:ok :via]))))
             (is (= [[:ling "hl-1"] [:ling "hl-2"] [:ling "hl-1"]] @sent))
             (is (not= :degraded (get-in (reg/status) [:ports :delivery :adapters :headless-ling :status])))))))

(deftest an-unregistered-backend-mode-is-treated-as-a-terminal
  (world {:backends #{} :lings {"hl-3" {:slave/id "hl-3" :ling/spawn-mode :hive-agent}}}
         (fn []
           ;; :hive-agent is no backend and has no io-model: a terminal mode by
           ;; elimination, so it reaches :emacs, which here dispatches.
           (is (= :emacs (get-in (delivery/deliver! {:to "hl-3" :message "m"
                                                    :params {:agent_id "hl-3"}})
                                 [:ok :via]))))))

(deftest coordinator-and-ling-lanes-keep-their-own-active
  (world {:nats? true :armed? true :tmux? true
          :lings {"hl-1" {:slave/id "hl-1" :ling/spawn-mode :hive-agent}
                  "vt-1" {:slave/id "vt-1" :ling/spawn-mode :vterm}}}
         (fn []
           (let [switches (atom [])
                 prev     (reg/set-switch-sink! #(swap! switches conj %))]
             (try
               (dotimes [_ 3]
                 (is (= :nats-inbox (get-in (delivery/deliver! {:to "coordinator:134462" :message "w"})
                                            [:ok :via])))
                 (is (= :headless-ling (get-in (delivery/deliver! {:to "hl-1" :message "m"
                                                                  :params {:agent_id "hl-1"}})
                                               [:ok :via]))))
               (is (= #{[:coordinator nil :nats-inbox] [:ling nil :headless-ling]}
                      (set (map (juxt :lane :from :to) @switches)))
                   "one election per lane, then no failover/recovered churn")
               (let [lanes (get-in (reg/status) [:ports :delivery :lanes])]
                 (is (= :nats-inbox (get-in lanes [:coordinator :active])))
                 (is (= :headless-ling (get-in lanes [:ling :active])))
                 (is (= [:headless-ling :emacs :hivemind-shout] (get-in lanes [:ling :chain]))))
               (testing "a vterm ling in the same lane still lands via :emacs"
                 (is (= :emacs (get-in (delivery/deliver! {:to "vt-1" :message "m"
                                                          :params {:agent_id "vt-1"}})
                                       [:ok :via]))))
               (finally (reg/set-switch-sink! prev)))))))

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
