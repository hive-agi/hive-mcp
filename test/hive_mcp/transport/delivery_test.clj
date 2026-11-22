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
  [{:keys [nats? armed? tmux? lings backends ling-inboxes consumers]} f]
  (reg/clear!)
  (reset! sent [])
  (reg/rebind! transports)
  (binding [delivery/*inbox-status* (fn [_ id] {:inbox id :armed? (if (set? armed?)
                                                                      (contains? armed? id)
                                                                      (boolean armed?))})
            delivery/*inbox-send*   (fn [_ msg] (swap! sent conj [:nats msg])
                                      {:id "m1" :to (:to msg) :subject "s" :seq 1 :duplicate? false})]
    (binding [delivery/*connection*    (fn [] (when nats? :conn))
              ;; Lings: :ling-inboxes armed, :consumers attached (by id).
              delivery/*ling-route*    (fn [id]
                                         (let [up? (boolean nats?)
                                               a?  (contains? (set ling-inboxes) id)
                                               c?  (contains? (set consumers) id)]
                                           {:ling-id id :nats-up? up? :armed? a?
                                            :consumer-attached? c? :reachable? (and up? a? c?)}))
              delivery/*find-ling*     (fn [id] (get lings id))
              delivery/*tmux-reply*    (fn [to msg]
                                         (swap! sent conj [:tmux to msg])
                                         (if tmux? {:ok true :routed :tmux :to to :delivered? true}
                                             {:ok false :error "no pane"}))
              delivery/*ling-dispatch* (fn [p]
                                         (when (str/starts-with? (str (:agent_id p)) "down")
                                           (throw (ex-info "no Emacs daemon answers" {})))
                                         (if-let [deliver (:deliver-fn p)]
                                           ;; Ling.dispatch! records the task,
                                           ;; then hands it to the route.
                                           (deliver {:task-id "t1" :task (:prompt p)})
                                           (do (swap! sent conj [:ling (:agent_id p)])
                                               {:task-id "t1"})))
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

(deftest both-coordinator-id-spellings-are-coordinators
  (binding [delivery/*find-ling* (constantly nil)]
    (testing "a pid and the first 8 hex of a session id (bb-mcp #45)"
      (doseq [id ["coordinator:12345" "coordinator:72216f39" "coordinator:1"]]
        (is (true? (delivery/coordinator-id? id)) id)
        (is (= :coordinator (delivery/target-kind id)) id)))
    (testing "ling ids and garbage are not coordinators"
      (doseq [id ["ling-72216f39" "72216f39" "swarm-worker-1" "coordinator" "coordinator-hive"
                  "coordinator:" "coordinator:abc" "coordinator:zzzzzzzz" "coordinator:72216F39"
                  "coordinator:72216f3" "coordinator:72216f390" "coordinator:72216f39-hive"
                  "coordinator:4fc0ff75-f9c0-4185-8fe4-371c0d1c8ba7" "coordinator:12 34"
                  " coordinator:12345" "" nil 12345 :coordinator]]
        (is (false? (delivery/coordinator-id? id)) (pr-str id))
        (is (nil? (delivery/target-kind id)) (pr-str id))))))

(deftest an-unknown-hex-coordinator-reaches-the-inbox-route
  (world {:nats? true :armed? #{"coordinator-72216f39"} :tmux? true}
         (fn []
           (let [res (delivery/deliver! {:to "coordinator:72216f39" :message "wake"
                                         :from "coordinator:593a2651"})
                 v   (:ok res)]
             (is (r/ok? res) "no registry row, and still no \"Agent not found\"")
             (is (not (chain/addressing? res)))
             (is (= :nats-inbox (:via v)))
             (is (= [:nats] (mapv first @sent)))
             (is (= "coordinator-72216f39" (get-in @sent [0 1 :to])))
             (is (= "coordinator:593a2651" (get-in @sent [0 1 :from])))))))

(deftest an-unarmed-hex-coordinator-falls-through-to-tmux-then-the-shout
  (world {:nats? true :armed? false :tmux? true}
         (fn []
           (let [res (delivery/deliver! {:to "coordinator:72216f39" :message "wake"})]
             (is (= :tmux (get-in res [:ok :via])))
             (is (= [[:tmux "coordinator:72216f39" "wake"]] @sent)))))
  (world {:nats? false :tmux? false}
         (fn []
           (let [res (delivery/deliver! {:to "coordinator:72216f39" :message "wake"})]
             (is (= :hivemind-shout (get-in res [:ok :via])))
             (is (= [:shout "coordinator:72216f39" "wake"] (last @sent)))))))

(deftest dispatch-to-a-hex-coordinator-is-not-agent-not-found
  (world {:nats? true :armed? #{"coordinator-72216f39"}}
         (fn []
           (let [out  (dispatch/handle-dispatch {:agent_id "coordinator:72216f39" :prompt "wake"})
                 body (json/read-str (:text out) :key-fn keyword)]
             (is (not (:isError out)) (:text out))
             (is (= "nats-inbox" (:via body)))))))

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
             (is (= [{:adapter :nats-inbox :reason :inapplicable}
                     {:adapter :headless-ling :reason :inapplicable}]
                    (mapv #(select-keys % [:adapter :reason]) (get-in res [:ok :skipped])))
                 "the ling lane never walks :tmux, a coordinator-only adapter")
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
                 (is (= [{:adapter :nats-inbox :reason :inapplicable}]
                        (mapv #(select-keys % [:adapter :reason]) (get-in res [:ok :skipped])))
                     "no consumer attached: NATS is skipped, never degraded")))
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
               (is (= #{[:coordinator nil :nats-inbox]}
                      (set (map (juxt :lane :from :to) @switches)))
                   (str "one election in the coordinator lane, then no churn; the ling"
                        " lane elects nothing past an inapplicable :nats-inbox"))
               (let [lanes (get-in (reg/status) [:ports :delivery :lanes])]
                 (is (= :nats-inbox (get-in lanes [:coordinator :active])))
                 (is (nil? (get-in lanes [:ling :active]))
                     "headless-ling served only lings NATS does not reach: no evidence against NATS")
                 (is (= [:nats-inbox :headless-ling :emacs :hivemind-shout]
                        (get-in lanes [:ling :chain]))))
               (testing "a vterm ling in the same lane still lands via :emacs"
                 (is (= :emacs (get-in (delivery/deliver! {:to "vt-1" :message "m"
                                                          :params {:agent_id "vt-1"}})
                                       [:ok :via]))))
               (finally (reg/set-switch-sink! prev)))))))

(deftest a-ling-with-an-attached-consumer-goes-via-nats-inbox
  (world {:nats? true :lings {"hl-n" {:slave/id "hl-n" :ling/spawn-mode :hive-agent}}
          :ling-inboxes #{"hl-n"} :consumers #{"hl-n"}}
         (fn []
           (let [res (delivery/deliver! {:to "hl-n" :message "do it" :from "coordinator:7"
                                         :params {:agent_id "hl-n" :prompt "do it"}})
                 v   (:ok res)]
             (is (r/ok? res))
             (is (= :nats-inbox (:via v)))
             (is (= :primary (:resolved-by v)) ":nats-inbox heads the ling lane")
             (is (= "t1" (get-in v [:value :task-id]))
                 "task bookkeeping stays in hive-mcp: the dispatch's task id is answered")
             (is (true? (get-in v [:value :route :consumer-attached?])))
             (is (= [:nats] (mapv first @sent)) "no in-process hand-off")
             (let [msg (get-in @sent [0 1])]
               (is (= "hl-n" (:to msg)))
               (is (= "t1" (:id msg)) "the task id is the message id the consumer dedups by")
               (is (= "do it" (:body msg)))
               (is (= "coordinator:7" (:from msg)))))
           (is (= :nats-inbox (get-in (reg/status) [:ports :delivery :lanes :ling :active]))))))

(deftest the-nats-route-carries-the-ling-sequence-in-meta
  (world {:nats? true :lings {"hl-s" {:slave/id "hl-s" :ling/spawn-mode :hive-agent}}
          :ling-inboxes #{"hl-s"} :consumers #{"hl-s"}}
         (fn []
           (let [stamp {:epoch "e1" :seq 3 :after 2}]
             ;; Ling.dispatch! stamps the task, then hands it to the route.
             (binding [delivery/*ling-dispatch* (fn [p] ((:deliver-fn p) {:task-id "t3" :task "go"
                                                                          :ling-seq stamp}))]
               (is (r/ok? (delivery/deliver! {:to "hl-s" :message "go"
                                              :params {:agent_id "hl-s" :prompt "go"}}))))
             (is (= {:task-id "t3" :ling-seq stamp} (get-in @sent [0 1 :meta]))
                 "the consumer reads the stamp from the envelope meta"))
           (reset! sent [])
           (binding [delivery/*ling-dispatch* (fn [p] ((:deliver-fn p) {:task-id "t4" :task "go"}))]
             (delivery/deliver! {:to "hl-s" :message "go" :params {:agent_id "hl-s" :prompt "go"}}))
           (is (= {:task-id "t4"} (get-in @sent [0 1 :meta])) "an unstamped task carries no stamp"))))

(deftest a-ling-without-a-consumer-falls-back-to-headless-ling
  (testing "inbox armed, consumer missing"
    (world {:nats? true :lings {"hl-x" {:slave/id "hl-x" :ling/spawn-mode :hive-agent}}
            :ling-inboxes #{"hl-x"}}
           (fn []
             (let [res (delivery/deliver! {:to "hl-x" :message "m" :params {:agent_id "hl-x"}})]
               (is (= :headless-ling (get-in res [:ok :via])))
               (is (= :fallback (get-in res [:ok :resolved-by])))
               (is (re-find #"no consumer of hl-x is attached"
                            (get-in res [:ok :skipped 0 :error :message])))
               (is (= [[:ling "hl-x"]] @sent))
               (is (not= :degraded (get-in (reg/status) [:ports :delivery :adapters :nats-inbox :status]))
                   "a missing consumer is the ling's fact, not the bus's")))))
  (testing "NATS down"
    (world {:nats? false :lings {"hl-y" {:slave/id "hl-y" :ling/spawn-mode :hive-agent}}
            :ling-inboxes #{"hl-y"} :consumers #{"hl-y"}}
           (fn []
             (let [res (delivery/deliver! {:to "hl-y" :message "m" :params {:agent_id "hl-y"}})]
               (is (= :headless-ling (get-in res [:ok :via])))
               (is (= "nats-inbox does not serve ling targets: NATS is down"
                      (get-in res [:ok :skipped 0 :error :message])))))))
  (testing "a consumer-less ling between two reachable ones keeps the lane on NATS"
    (world {:nats? true :lings {"a" {:slave/id "a" :ling/spawn-mode :hive-agent}
                                "b" {:slave/id "b" :ling/spawn-mode :hive-agent}}
            :ling-inboxes #{"a" "b"} :consumers #{"a"}}
           (fn []
             (is (= :nats-inbox (get-in (delivery/deliver! {:to "a" :message "1" :params {:agent_id "a"}}) [:ok :via])))
             (is (= :headless-ling (get-in (delivery/deliver! {:to "b" :message "2" :params {:agent_id "b"}}) [:ok :via])))
             (is (= :nats-inbox (get-in (reg/status) [:ports :delivery :lanes :ling :active])))
             (is (= :nats-inbox (get-in (delivery/deliver! {:to "a" :message "3" :params {:agent_id "a"}}) [:ok :via])))))))

(deftest the-ling-lane-probe-tells-nats-up-from-consumer-attached
  (world {:nats? true :ling-inboxes #{"hl-p"}}
         (fn []
           (is (r/ok? ((:probe (reg/open-adapter :delivery :nats-inbox)))) "the bus is up")
           (is (= {:nats-up? true :armed? true :consumer-attached? false :reachable? false}
                  (select-keys (delivery/ling-route-health "hl-p")
                               [:nats-up? :armed? :consumer-attached? :reachable?]))
                  "but this ling's consumer is not attached"))))

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
