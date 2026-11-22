(ns hive-mcp.nats.sense-replay-test
  "hive-mcp.nats.sense-replay through its ports: an in-test consume-once
   sense log as the drain, in-memory publishers and stores (no with-redefs).
   Acceptance of card 20261116182711-549cf262: bus down -> N senses -> bus
   up -> exactly N in order, no dup; unbound -> bind -> delivered; queue
   overflow recovered by the drain; a restart resumes from persisted state."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.nats.sense-adapters :as adapters]
            [hive-mcp.nats.sense-replay :as replay]
            [hive-mcp.nats.sense-republish :as sr]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; In-test adapters
;; =============================================================================

(defn- mem-log
  "A consume-once sense log: {:append! :port}, one cursor per consumer,
   honouring :limit and a {:classes [..]} receptor, like hive-agent's."
  []
  (let [entries* (atom []) cursors* (atom {})]
    {:entries* entries*
     :append!  (fn [s] (swap! entries* conj s))
     :port     {:drain!
                (fn [{:keys [consumer limit receptor]}]
                  (let [cur   (get @cursors* consumer 0)
                        cls   (set (map #(keyword "sense" %) (:classes receptor)))
                        after (drop cur @entries*)
                        idx   (map-indexed (fn [i s] [(+ cur i 1) s]) after)
                        adm   (filter #(or (empty? cls) (contains? cls (:sense/class (second %)))) idx)
                        taken (if limit (take limit adm) adm)
                        cut?  (and limit (> (count adm) (count taken)))
                        c'    (if (and cut? (seq taken)) (first (last taken)) (count @entries*))]
                    (swap! cursors* assoc consumer c')
                    {:senses (mapv second taken) :cursor c' :epoch "e1"}))}}))

(defrecord MemFeed [listeners*]
  sr/ISenseFeed
  (feed-id [_] :mem)
  (subscribe! [_ k f] (swap! listeners* assoc k f) true)
  (unsubscribe! [_ k] (swap! listeners* dissoc k) nil))

(defrecord MemPublisher [up?* sent*]
  sr/IInboxPublisher
  (publisher-available? [_] @up?*)
  (publish-message! [_ msg]
    (let [dup? (some #(= (:id msg) (:id %)) @sent*)]
      (when-not dup? (swap! sent* conj msg))
      {:duplicate? (boolean dup?)})))

(defn- mem-publisher [] (->MemPublisher (atom true) (atom [])))

(defn- sense
  ([id] (sense id :sense/completed "coordinator:abc"))
  ([id cls parent]
   {:sense/id id :sense/class cls :sense/agent "ling-a" :sense/project "hive"
    :sense/parent parent :sense/text "done" :sense/at 42}))

(def ^:private bound {"coordinator:abc" "pedro"})

(defn- rig
  "A republisher + replayer over a fresh log. `bindings*` holds the table."
  ([] (rig {}))
  ([{:keys [store pub bindings* now* log] :or {}}]
   (let [log       (or log (mem-log))
         pub       (or pub (mem-publisher))
         bindings* (or bindings* (atom bound))
         now*      (or now* (atom 1000))
         r         (sr/republisher (->MemFeed (atom {})) pub
                                   {:resolve-inbox #(sr/inbox-for @bindings* %)})
         rp        (replay/replayer r {:drain  #(adapters/drain-senses (:port log) %)
                                       :store  (or store (replay/mem-store))
                                       :now-fn #(deref now*)})]
     {:log log :pub pub :bindings* bindings* :now* now* :r r :rp rp})))

(defn- sent-ids [pub] (mapv :id @(:sent* pub)))

(defn- clean [f]
  (replay/uninstall!) (sr/uninstall!)
  (doseq [k (keys (sr/bindings))] (sr/unbind-inbox! k))
  (try (f) (finally
             (replay/uninstall!) (sr/uninstall!)
             (doseq [k (keys (sr/bindings))] (sr/unbind-inbox! k)))))

(use-fixtures :each clean)

;; =============================================================================
;; Pure
;; =============================================================================

(deftest hold-keeps-order-skips-dups-and-non-republishable
  (let [p (replay/hold [] [(sense "a") (sense "b" :sense/progress "coordinator:abc") (sense "c")] 5)
        p (replay/hold p [(sense "a") (sense "d")] 6)]
    (is (= ["a" "c" "d"] (mapv (comp :sense/id :sense) p)))
    (is (= [5 5 6] (mapv :held-at p)))
    (is (m/validate [:vector replay/Held] p))))

(deftest trim-expires-then-caps-oldest-first
  (let [p [{:sense (sense "a") :held-at 0} {:sense (sense "b") :held-at 50}
           {:sense (sense "c") :held-at 60} {:sense (sense "d") :held-at 70}]
        {:keys [pending expired evicted]} (replay/trim p 100 {:ttl-ms 60 :cap 2})]
    (is (= 1 expired))
    (is (= 1 evicted))
    (is (= ["c" "d"] (mapv (comp :sense/id :sense) pending)))))

(deftest what-stays-pending
  (is (replay/keep-outcome? {:outcome :degraded}))
  (is (replay/keep-outcome? {:outcome :failed}))
  (is (replay/keep-outcome? {:outcome :skipped :reason :no-inbox}))
  (is (not (replay/keep-outcome? {:outcome :skipped :reason :class})))
  (is (not (replay/keep-outcome? {:outcome :sent})))
  (is (not (replay/keep-outcome? {:outcome :duplicate}))))

(deftest edn-safe-prints-unreadable-values
  (is (= {:a "x" :b 1} (update (replay/edn-safe {:a (Object.) :b 1}) :a (constantly "x"))))
  (is (string? (:a (replay/edn-safe {:a (Object.)})))))

;; =============================================================================
;; The loop
;; =============================================================================

(deftest the-first-step-baselines-and-replays-no-history
  (let [{:keys [log pub rp]} (rig)]
    ((:append! log) (sense "old-1"))
    ((:append! log) (sense "old-2"))
    (let [res (replay/step! rp)]
      (is (m/validate replay/StepResult res))
      (is (true? (:baseline? res)))
      (is (= 2 (:drained res))))
    (is (empty? @(:sent* pub)) "history is not flooded into inboxes")
    ((:append! log) (sense "new-1"))
    (is (= 1 (:sent (replay/step! rp))))
    (is (= ["sense-new-1"] (sent-ids pub)))))

(deftest bus-down-n-senses-bus-up-exactly-n-in-order-no-dup
  (let [{:keys [log pub rp]} (rig)]
    (replay/step! rp)
    (reset! (:up?* pub) false)
    (doseq [i (range 5)]
      ((:append! log) (sense (str "s" i)))
      (let [res (replay/step! rp)]
        (is (:bus-down? res))
        (is (= (inc i) (:pending res)))))
    (is (empty? @(:sent* pub)))
    (reset! (:up?* pub) true)
    (let [res (replay/step! rp)]
      (is (= 5 (:sent res)))
      (is (zero? (:pending res))))
    (is (= ["sense-s0" "sense-s1" "sense-s2" "sense-s3" "sense-s4"] (sent-ids pub)))
    (testing "further wakes deliver nothing twice"
      (replay/step! rp) (replay/step! rp)
      (is (= 5 (count @(:sent* pub)))))))

(deftest an-unbound-parent-is-held-until-it-binds
  (let [{:keys [log pub rp bindings*]} (rig {:bindings* (atom {})})]
    (replay/step! rp)
    ((:append! log) (sense "u1" :sense/ask "coordinator:abc"))
    ((:append! log) (sense "u2" :sense/blocked "coordinator:abc"))
    (is (= 2 (:pending (replay/step! rp))))
    (is (empty? @(:sent* pub)))
    (reset! bindings* bound)
    (is (= 2 (:sent (replay/step! rp))))
    (is (= ["sense-u1" "sense-u2"] (sent-ids pub)))))

(deftest unbound-senses-expire-after-the-ttl
  (let [{:keys [log pub rp now*]} (rig {:bindings* (atom {})})]
    (replay/step! rp)
    ((:append! log) (sense "x1"))
    (is (= 1 (:pending (replay/step! rp))))
    (swap! now* + replay/default-ttl-ms 1)
    (let [res (replay/step! rp)]
      (is (= 1 (:expired res)))
      (is (zero? (:pending res))))
    (is (empty? @(:sent* pub)))))

(deftest a-sense-the-push-queue-dropped-is-recovered-by-the-drain
  (let [{:keys [log pub rp]} (rig)
        dropped (sr/listener (assoc (:r (rig)) :execute (fn [_] false)))]
    (replay/step! rp)
    ((:append! log) (sense "q1"))
    (dropped (sense "q1"))
    (is (empty? @(:sent* pub)) "the push hand-off was refused")
    (replay/step! rp)
    (is (= ["sense-q1"] (sent-ids pub)))))

(deftest progress-senses-are-not-replayed
  (let [{:keys [log pub rp]} (rig)]
    (replay/step! rp)
    ((:append! log) (sense "p1" :sense/progress "coordinator:abc"))
    ((:append! log) (sense "c1"))
    (replay/step! rp)
    (is (= ["sense-c1"] (sent-ids pub)))))

(deftest a-failing-drain-keeps-pending-and-reports
  (let [pub (mem-publisher)
        r   (sr/republisher (->MemFeed (atom {})) pub {:resolve-inbox #(sr/inbox-for bound %)})
        rp  (replay/replayer r {:drain (fn [_] {:ok? false :consumer "c" :senses []
                                                :reason :drain-failed :error "boom"})})]
    (let [res (replay/step! rp)]
      (is (= "boom" (:error res)))
      (is (true? (:baseline? res)))
      (is (true? (:baseline? (replay/step! rp))) "still baselining: the log was never read")
      (is (false? (:baselined? @(:state* rp)))))
    (is (= 2 (get-in @(:stats* rp) [:drain-failed])))))

;; =============================================================================
;; Persistence: a restart resumes
;; =============================================================================

(defn- tmp-path []
  (let [d (.toFile (java.nio.file.Files/createTempDirectory
                    "sense-replay" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (str (io/file d "state.edn"))))

(deftest file-store-round-trips-and-survives-corruption
  (let [p  (tmp-path)
        st {:baselined? true :pending [{:sense (sense "f1") :held-at 7}]}]
    (is (nil? (replay/read-state p)))
    (replay/write-state! p st)
    (is (= st (replay/read-state p)))
    (spit p "{:baselined? tr")
    (is (nil? (replay/read-state p)) "unreadable -> nil, never throws")
    (is (not (.exists (io/file p))) "moved aside for a human")))

(deftest a-restart-resumes-pending-from-the-persisted-outbox
  (let [p   (tmp-path)
        log (mem-log)
        pub (mem-publisher)
        a   (rig {:store (replay/file-store p) :log log :pub pub})]
    (replay/step! (:rp a))
    (reset! (:up?* pub) false)
    ((:append! log) (sense "r1"))
    ((:append! log) (sense "r2"))
    (is (= 2 (:pending (replay/step! (:rp a)))))
    (testing "a fresh replayer over the same file (JVM restart) delivers them once"
      (reset! (:up?* pub) true)
      (let [b   (rig {:store (replay/file-store p) :log log :pub pub})
            res (replay/step! (:rp b))]
        (is (false? (:baseline? res)) "the persisted state says it already baselined")
        (is (= 2 (:sent res))))
      (is (= ["sense-r1" "sense-r2"] (sent-ids pub)))
      (is (= {:baselined? true :pending []} (replay/read-state p))))))

;; =============================================================================
;; Wakes and the process replayer
;; =============================================================================

(deftest wakes-coalesce-and-a-refused-hand-off-is-counted
  (let [queued (atom [])
        {:keys [r]} (rig)
        rp (replay/replayer r {:drain (constantly {:ok? true :consumer "c" :senses []})
                               :execute (fn [t] (swap! queued conj t) true)})]
    (is (true? (replay/wake! rp)))
    (is (true? (replay/wake! rp)))
    (is (= 1 (count @queued)) "the second wake rode the queued step")
    ((first @queued))
    (is (true? (replay/wake! rp)))
    (is (= 2 (count @queued)) "after the step ran, a wake queues again"))
  (let [{:keys [r]} (rig)
        rp (replay/replayer r {:drain (constantly {:ok? true :consumer "c" :senses []})
                               :execute (fn [_] false)})]
    (is (false? (replay/wake! rp)))
    (is (= 1 (:wake-dropped @(:stats* rp))))
    (is (false? (replay/wake! rp)) "not stuck scheduled after a refusal")))

(deftest install-wakes-on-push-and-on-bind
  (let [log  (mem-log)
        pub  (mem-publisher)
        feed (->MemFeed (atom {}))
        r    (sr/republisher feed pub)
        rp   (replay/replayer r {:drain #(adapters/drain-senses (:port log) %)})]
    (is (:installed? (sr/install! r (replay/wake-listener rp))))
    (is (= {:replay? true} (replay/install! rp)))
    (is (:baselined? (replay/status)) "install! woke it once: baselined")
    (testing "a pushed sense for an unbound parent is held"
      ((:append! log) (sense "w1"))
      (doseq [[_ f] @(:listeners* feed)] (f (sense "w1")))
      (is (empty? @(:sent* pub)))
      (is (= 1 (:pending (replay/status)))))
    (testing "binding the parent wakes the replayer, which delivers it"
      (sr/bind-inbox! "coordinator:abc" "pedro")
      (is (= ["sense-w1"] (sent-ids pub)))
      (is (zero? (:pending (replay/status)))))
    (testing "a reconnect wake is harmless"
      (is (true? (replay/wake-current!)))
      (is (= 1 (count @(:sent* pub)))))
    (replay/uninstall!)
    (is (false? (replay/wake-current!)))
    (is (= {:installed? false} (replay/status)))))
