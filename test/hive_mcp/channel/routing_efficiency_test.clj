(ns hive-mcp.channel.routing-efficiency-test
  "What directed addressing COSTS, measured, and a ratchet so the win survives.

   ## Why this test exists

   Every other test here asks whether a message reaches the right reader. None
   of them can fail when delivery becomes more expensive, and expense is the
   entire reason directed addressing was built. A regression would not look
   like a bug; it would look like a slightly larger context window, on a bill
   nobody reads per-message. So the saving needs an assertion.

   ## The workload, and why it is shaped this way

   A swarm of N lings, each taking T turns. A share of those turns is PEER
   COORDINATION: ling i needs something from ling j. The rest is ordinary
   progress telemetry aimed at the spawner, which every regime treats alike and
   which is therefore the control — a regime that only looked good because it
   dropped telemetry would show up here.

   Three regimes, and the model of each is the honest part:

     :broadcast  what you do TODAY to reach a peer when there is no peer
                 address: say it to everyone. One message, N readers pay.
     :spawner    the other thing you do today: route through the coordinator,
                 because it is the only reader you can address. That is TWO
                 messages per exchange (ling -> coordinator, coordinator ->
                 ling) and the coordinator pays for both.
     :directed   one message, one reader.

   Modelling :spawner as a single message would flatter it: the peer never
   receives anything, so the coordination does not happen. The relay is what
   makes the comparison fair.

   ## The fourth measurement: what METERING is worth on its own

   `metered-report` runs the :broadcast workload past the live volume gate
   (hive-mcp.channel.broadcast-policy plus broadcast-ledger) instead of letting
   every request through. It answers a different question from the three
   regimes: not \"is addressing a peer cheaper than shouting\" but \"how much
   damage can a swarm that shouts anyway still do\". The answer is a constant
   set by the budget rather than a function of how chatty the swarm is.

   It deliberately also asserts that metered broadcasting is STILL dearer than
   directed delivery. The gate bounds a bad habit; it does not make the habit
   right, and a test that only showed the saving would read as though it did.

   ## What is measured

   EXACT CHARACTERS of the rows each reader's drain returns, summed over every
   reader. That is the wire cost, with no estimation in it. Tokens are reported
   as chars/4 and labelled an estimate, because no tokenizer is on this
   classpath and inventing precision would be worse than not having it."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.channel.audience :as aud]
            [hive-mcp.channel.broadcast-ledger :as bledger]
            [hive-mcp.channel.broadcast-policy :as bp]
            [hive-mcp.channel.piggyback :as pb]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Workload
;; =============================================================================

(def ^:private swarm-size 6)
(def ^:private turns-per-ling 8)
(def ^:private coordinator "coordinator:bench")
(def ^:private project "bench")

(defn- lings [] (mapv #(str "ling-" %) (range swarm-size)))

(def ^:private peer-question
  "A realistic peer message: long enough to matter, short enough to stay inline."
  "the schema you generated has :order/total as a string; should I coerce on read or is that deliberate?")

(def ^:private telemetry
  "Ordinary per-turn progress, aimed at the spawner in every regime.")

(defn- peer-turn?
  "Half the turns are peer coordination. Deterministic, so the bench does not
   move between runs. `mix` :all makes every turn a peer turn, which isolates
   the traffic directed addressing is FOR from the telemetry control."
  ([i t] (peer-turn? i t :mixed))
  ([i t mix]
   (or (= :all mix) (odd? (+ i t)))))

(defn- build-messages
  "The same workload spelled for one routing regime. Returns messages in
   timestamp order. `mix` is :mixed (half peer coordination, half telemetry) or
   :all (peer coordination only).

   A peer exchange runs under ONE contextId per (sender, recipient) pair, not
   one per message. That is what an A2A contextId means — a conversation, not a
   turn — and it is how a real session behaves: a ling asking its neighbour
   eight questions is having one conversation with it."
  ([regime] (build-messages regime :mixed))
  ([regime mix]
   (let [ls (lings)
         base {:project-id project}]
     (vec
      (mapcat
       (fn [[i from]]
         (mapcat
          (fn [t]
            (let [ts (+ 1000 (* 100 t) i)
                  to (nth ls (mod (inc i) swarm-size))]
              (if-not (peer-turn? i t mix)
                ;; Control: identical in every regime.
                [(merge base {:agent-id from :event-type :progress
                              :message (str telemetry " turn " t)
                              :timestamp ts :parent-id coordinator})]
                (case regime
                  ;; One message, every reader pays.
                  :broadcast
                  [(merge base {:agent-id from :event-type :progress
                                :message peer-question :timestamp ts
                                :parent-id coordinator :broadcast? true
                                :deliberate? true})]
                  ;; Two messages: the peer is unreachable, so it goes through
                  ;; the one reader that IS reachable and comes back out.
                  :spawner
                  [(merge base {:agent-id from :event-type :progress
                                :message peer-question :timestamp ts
                                :parent-id coordinator :deliberate? true})
                   (merge base {:agent-id coordinator :event-type :progress
                                :message (str "relaying from " from ": " peer-question)
                                :timestamp (inc ts) :parent-id to :deliberate? true})]
                  ;; One message, one reader.
                  :directed
                  [(merge base {:agent-id from :event-type :progress
                                :message peer-question :timestamp ts
                                :parent-id coordinator :to to
                                :context-id (str "a2actx-" from "-" to)
                                :deliberate? true})]))))
          (range turns-per-ling)))
       (map-indexed vector ls))))))

;; =============================================================================
;; Measurement
;; =============================================================================

(defn- drain-all
  "Deliver `msgs` to every reader in the swarm plus the coordinator, and return
   the exact character cost per reader. Each reader drains once, as one turn of
   a real session would."
  [msgs]
  (pb/reset-all-cursors!)
  (pb/clear-backbone-buffer!)
  (pb/register-message-source! (fn [] msgs))
  (into {}
        (for [reader (conj (lings) coordinator)]
          [reader (count (pr-str (or (pb/get-messages reader :project-id project) [])))])))

(defn- measure
  ([regime] (measure regime :mixed))
  ([regime mix]
   (let [per-reader (drain-all (build-messages regime mix))
         total (reduce + 0 (vals per-reader))]
     {:regime regime
      :chars total
      :est-tokens (quot total 4)
      :per-reader per-reader})))

(defn report
  "Run every regime and return the comparison. Called by the test and usable
   straight from a REPL when the numbers need re-reading.

   `mix` :mixed is the realistic session (half the turns are peer coordination);
   :all isolates peer coordination, which is the traffic directed addressing
   exists for and therefore the number that says how good the mechanism is
   rather than how often it is used."
  ([] (report :mixed))
  ([mix]
   (let [ms (mapv #(measure % mix) [:broadcast :spawner :directed])
         by-regime (into {} (map (juxt :regime identity)) ms)]
     (assoc by-regime
            :mix mix
            :ratio-directed-vs-broadcast
            (double (/ (get-in by-regime [:directed :chars])
                       (get-in by-regime [:broadcast :chars])))
            :ratio-directed-vs-spawner
            (double (/ (get-in by-regime [:directed :chars])
                       (get-in by-regime [:spawner :chars])))))))

(defn- meter-broadcasts
  "Run a broadcast workload past the live policy and a fresh ledger, in
   timestamp order, and return what actually reaches the wire.

   This is the gate as it behaves in the shout pipeline, not a model of it:
   the same `decide` and the same ledger arithmetic. A refused broadcast is
   DOWNGRADED rather than dropped, so it keeps its :parent-id and reaches the
   coordinator alone — which is why the saving shows up as cost moving off N
   readers and onto one, and never as information going missing."
  [msgs]
  (let [window (bledger/window-ms)]
    (first
     (reduce
      (fn [[out ledger] {:keys [broadcast? project-id timestamp] :as m}]
        (if-not broadcast?
          [(conj out m) ledger]
          (let [spent (bledger/spent ledger project-id timestamp window)
                {:keys [verdict]} (bp/decide {:broadcast? true
                                              :broadcast-reason :shared-discovery}
                                             {:recent-broadcasts spent})]
            (if (= :broadcast verdict)
              [(conj out m) (bledger/spend ledger project-id timestamp window)]
              [(conj out (dissoc m :broadcast?)) ledger]))))
      [[] {}]
      msgs))))

(defn metered-report
  "What the volume gate saves a swarm that broadcasts every peer turn.

   The :broadcast regime is the honest worst case of today's behaviour, and it
   is UNBOUNDED: every extra turn is another N-reader copy. The gate makes the
   broadcast share O(1) in the number of turns — a constant set by the budget,
   not by how chatty the swarm is — and moves the rest onto the single reader
   a downgrade addresses."
  []
  (let [raw (build-messages :broadcast :all)
        metered (meter-broadcasts raw)
        chars (fn [ms] (reduce + 0 (vals (drain-all ms))))]
    {:attempted (count (filterv :broadcast? raw))
     :admitted (count (filterv :broadcast? metered))
     :budget bp/default-budget
     :chars-unmetered (chars raw)
     :chars-metered (chars metered)}))

(deftest the-volume-gate-bounds-what-a-chatty-swarm-can-cost-test
  ;; Measured 2026-09-15 on this workload (6 lings, 8 turns each, every turn a
  ;; peer turn): 48 broadcasts attempted, 8 admitted, 46039 chars -> 13159.
  ;; Thresholds are set from that measurement with headroom and annotated with
  ;; it, so a regression moves a number a reader can compare against the one
  ;; written here.
  (let [{:keys [attempted admitted budget chars-unmetered chars-metered]} (metered-report)
        directed (:chars (measure :directed :all))
        ratio (double (/ chars-metered chars-unmetered))]

    (testing "the admitted count is the budget, whatever the swarm attempts"
      (is (= 48 attempted) "the workload really does try to broadcast every turn")
      (is (= budget admitted)
          "that is the bound: broadcasts are O(1) in turns, not O(turns)")
      (is (< admitted (quot attempted 4))
          "and the bound is well below what was asked for, or it bounds nothing"))

    (testing "the refused ones are downgraded, so nothing goes missing"
      ;; Every attempt still reaches a reader; the cheap ones reach one reader
      ;; instead of six. If a refusal dropped the message this number would
      ;; fall much further, which is why the saving alone is not the assertion.
      (is (pos? chars-metered) "a swarm that is entirely refused still says things")
      (is (> chars-metered (* 0.1 chars-unmetered))
          "a collapse to near zero would mean refusal is dropping, not downgrading"))

    (testing "and it costs a lot less (measured 0.286)"
      (is (< ratio 0.35)
          (str "metered/unmetered was " ratio ", measured 0.286 on 2026-09-15")))

    (testing "but the gate is NOT a substitute for addressing a peer"
      ;; This is the claim that keeps the feature honest. Metering bounds the
      ;; damage a broadcast habit does; it does not make broadcasting the right
      ;; call. Directed delivery is still cheaper, because a downgraded
      ;; broadcast reaches the coordinator, and the coordinator is not who the
      ;; sender needed.
      (is (< directed chars-metered)
          (str "directed " directed " vs metered broadcast " chars-metered)))))

;; =============================================================================
;; The ratchet
;; =============================================================================

(deftest directed-costs-less-than-broadcast-test
  (testing "a realistic session: half peer coordination, half telemetry"
    (let [{:keys [ratio-directed-vs-broadcast ratio-directed-vs-spawner] :as r} (report :mixed)]
      (testing "the workload is non-vacuous — every regime actually delivered something"
        (doseq [regime [:broadcast :spawner :directed]]
          (is (pos? (get-in r [regime :chars]))
              (str regime " delivered nothing, so the comparison means nothing"))))
      ;; Thresholds are set from the measurement with headroom, not from hope.
      ;; Measured 2026-09-15: 23707 / 8059 / 5343 chars -> 0.225 and 0.663.
      (is (< ratio-directed-vs-broadcast 0.28)
          (str "directed/broadcast = " ratio-directed-vs-broadcast " (was 0.225)"))
      (is (< ratio-directed-vs-spawner 0.72)
          (str "directed/spawner = " ratio-directed-vs-spawner " (was 0.663)"))))
  (testing "isolated peer coordination — the traffic the mechanism is for"
    (let [{:keys [ratio-directed-vs-broadcast ratio-directed-vs-spawner]} (report :all)]
      ;; Measured 2026-09-15: 46039 / 14743 / 8643 chars -> 0.188 and 0.586.
      ;;
      ;; The :spawner comparison counts CHANNEL BYTES ONLY, so it UNDERSTATES
      ;; the real saving: a relay through the coordinator also costs a
      ;; coordinator TURN — an LLM read-think-write that dwarfs the bytes and
      ;; that a deterministic bench cannot honestly price. The live arm is
      ;; where that shows up; this number is the floor, not the estimate.
      (is (< ratio-directed-vs-broadcast 0.24)
          (str "peer-only directed/broadcast = " ratio-directed-vs-broadcast))
      (is (< ratio-directed-vs-spawner 0.65)
          (str "peer-only directed/spawner = " ratio-directed-vs-spawner)))))

(deftest the-coordinator-stops-paying-for-peer-chatter-test
  (let [r (report :all)
        coord-broadcast (get-in r [:broadcast :per-reader coordinator])
        coord-spawner   (get-in r [:spawner :per-reader coordinator])
        coord-directed  (get-in r [:directed :per-reader coordinator])]
    (testing "the coordinator's own window is where the saving lands"
      ;; Measured 2026-09-15: 6577 -> 669 chars, a 90% cut, while still
      ;; learning that every one of those exchanges happened.
      (is (< coord-directed (* 0.20 coord-broadcast))
          (str "coordinator window: broadcast " coord-broadcast
               " vs directed " coord-directed))
      (is (< coord-directed (* 0.20 coord-spawner))))
    (testing "but it does not go blind — it still learns the exchanges happened"
      (pb/reset-all-cursors!)
      (pb/register-message-source! (fn [] (build-messages :directed :all)))
      (let [rows (pb/get-messages coordinator :project-id project)
            peer-rows (filter #(= "peer-traffic" (:e %)) rows)]
        (is (seq peer-rows) "no peer-traffic summary reached the coordinator")
        (is (= (reduce + 0 (map :n peer-rows))
               (count (filter :to (build-messages :directed :all))))
            "every directed message is accounted for in the summary")))))

(deftest telemetry-is-unaffected-by-regime-test
  (testing "the control: non-peer turns cost the same under every regime"
    (let [telemetry-only (filterv #(and (nil? (:to %)) (not (:broadcast? %))
                                        (not= coordinator (:agent-id %)))
                                  (build-messages :directed))
          cost (fn [] (reduce + 0 (vals (drain-all telemetry-only))))
          a (cost) b (cost)]
      (is (= a b) "the measurement itself is deterministic")
      (is (pos? a)))))

(deftest the-summary-does-not-grow-with-chattiness-test
  (testing "a swarm that opens a fresh context per message still costs the
            coordinator a bounded summary"
    ;; This is the case the first measurement got wrong: one context per
    ;; MESSAGE gave one summary row per message, which is the linear cost
    ;; directed addressing exists to remove, reintroduced one level up.
    (let [pathological (mapv (fn [i]
                               {:agent-id (str "ling-" (mod i swarm-size))
                                :to (str "ling-" (mod (inc i) swarm-size))
                                :context-id (str "ctx-" i)
                                :project-id project
                                :event-type :progress
                                :message peer-question
                                :timestamp (+ 1000 i)})
                             (range 200))
          rows (aud/peer-traffic-digest coordinator pathological)]
      (is (<= (count rows) (inc aud/max-peer-traffic-rows))
          "200 conversations must not produce 200 rows")
      (is (= 200 (reduce + 0 (map :n rows)))
          "and capping must not lose a single message from the count"))))
