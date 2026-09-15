(ns hive-mcp.channel.a2a-routing-test
  "Golden + property pinning for directed ling-to-ling delivery.

   Four subjects, all pure:

     addressed-to?        with a :to, the EXCLUSIVITY law: exactly one reader
                          in the whole swarm is addressed, and it is the named
                          one. A regression here does not look like a bug, it
                          looks like slightly higher token usage, which is
                          precisely why it needs a property rather than a case.
     peer-traffic-digest  CONSERVATION: every directed message a coordinator
                          is not addressed by is counted in some digest row.
                          A digest that silently dropped one would read exactly
                          like a working one on a golden case.
     broadcast-policy     TOTALITY: every input yields a verdict, and
                          :broadcast is reachable ONLY through an admissible
                          argument.
     payload-ref/elide!   LOSSLESSNESS: what a reader sees is bounded, and the
                          part it does not see is recoverable by id. The old
                          truncation was bounded and lossy; a test that only
                          checked the bound would pass for both."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as tc-prop]
            [hive-mcp.channel.a2a :as a2a]
            [hive-mcp.channel.audience :as aud]
            [hive-mcp.channel.broadcast-policy :as bp]
            [hive-mcp.channel.payload-ref :as pref]
            [hive-mcp.hivemind.tools :as hmt]
            [hive-mcp.tools.consolidated.hivemind :as chm]
            [hive-mcp.channel.piggyback :as pb]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Generators
;; =============================================================================

(def ^:private gen-ling-id
  (gen/fmap #(str "ling-" %) (gen/not-empty gen/string-alphanumeric)))

(def ^:private gen-swarm
  "A swarm of distinct ling ids, at least two so a directed message has both a
   recipient and a bystander to prove nothing reaches."
  (gen/fmap vec (gen/set gen-ling-id {:min-elements 2 :max-elements 6})))

;; =============================================================================
;; Exclusivity — the law directed addressing exists for
;; =============================================================================

(defspec directed-message-reaches-exactly-one-reader 200
  (tc-prop/for-all [swarm gen-swarm
                    sender-i gen/nat
                    recipient-i gen/nat
                    parent gen-ling-id]
    (let [sender (nth swarm (mod sender-i (count swarm)))
          recipient (nth swarm (mod recipient-i (count swarm)))
          msg {:agent-id sender :parent-id parent :to recipient}
          ;; Every reader that could possibly be asked: the whole swarm, the
          ;; sender's spawner, and a coordinator lane.
          readers (distinct (concat swarm [parent "coordinator:7" "coordinator"]))
          reached (filter #(aud/addressed-to? % msg) readers)]
      (and (= 1 (count (distinct (filter #(aud/same-agent? % recipient) reached))))
           (every? #(aud/same-agent? % recipient) reached)))))

(defspec directed-beats-broadcast 100
  (tc-prop/for-all [sender gen-ling-id
                    recipient gen-ling-id
                    bystander gen-ling-id]
    (let [msg {:agent-id sender :to recipient :broadcast? true}]
      ;; A message carrying both is self-contradictory. The specific address is
      ;; the one the sender actually thought about, so a bystander must not pay.
      (or (aud/same-agent? bystander recipient)
          (not (aud/addressed-to? bystander msg))))))

(deftest directed-excludes-the-coordinator-test
  (testing "a directed message does not enter the coordinator's context"
    (is (false? (aud/addressed-to? "coordinator:7"
                                   {:agent-id "ling-a" :to "ling-b"
                                    :parent-id "coordinator:7"}))))
  (testing "while the same message without :to still reaches it"
    (is (true? (aud/addressed-to? "coordinator:7"
                                  {:agent-id "ling-a" :parent-id "coordinator:7"})))))

(deftest undirected-routing-is-unchanged-test
  (testing "spawner routing survives the new rule"
    (is (true? (aud/addressed-to? "ling-parent" {:agent-id "ling-a" :parent-id "ling-parent"})))
    (is (false? (aud/addressed-to? "ling-other" {:agent-id "ling-a" :parent-id "ling-parent"}))))
  (testing "a root-level shout still reaches coordinators only"
    (is (true? (aud/addressed-to? "coordinator" {:agent-id "ling-a"})))
    (is (false? (aud/addressed-to? "ling-other" {:agent-id "ling-a"}))))
  (testing "an admitted broadcast still reaches everyone"
    (is (true? (aud/addressed-to? "ling-other" {:agent-id "ling-a" :broadcast? true})))))

;; =============================================================================
;; Conservation — the coordinator loses the bodies, never the fact
;; =============================================================================

(defspec peer-traffic-digest-conserves-every-message 100
  (tc-prop/for-all [swarm gen-swarm
                    n (gen/choose 1 12)
                    ctxs (gen/not-empty (gen/vector (gen/elements ["c1" "c2" "c3"]) 1 3))]
    (let [msgs (mapv (fn [i]
                       {:agent-id (nth swarm (mod i (count swarm)))
                        :to (nth swarm (mod (inc i) (count swarm)))
                        :context-id (nth ctxs (mod i (count ctxs)))})
                     (range n))
          rows (aud/peer-traffic-digest "coordinator:7" msgs)]
      (= n (reduce + 0 (map :n rows))))))

(deftest peer-traffic-digest-is-coordinator-only-test
  (let [msgs [{:agent-id "ling-a" :to "ling-b" :context-id "c1"}]]
    (is (= [] (aud/peer-traffic-digest "ling-c" msgs))
        "a peer has no business knowing two other peers spoke")
    (is (seq (aud/peer-traffic-digest "coordinator:7" msgs)))))

(deftest peer-traffic-digest-omits-what-the-reader-already-got-test
  (testing "a directed message the coordinator IS addressed by is not also summarised"
    (let [msgs [{:agent-id "ling-a" :to "coordinator:7" :context-id "c1"}]]
      (is (= [] (aud/peer-traffic-digest "coordinator:7" msgs))))))

(deftest peer-traffic-digest-names-the-context-test
  (let [msgs [{:agent-id "ling-a" :to "ling-b" :context-id "c1"}
              {:agent-id "ling-b" :to "ling-a" :context-id "c1"}
              {:agent-id "ling-a" :to "ling-c" :context-id "c2"}]
        rows (aud/peer-traffic-digest "coordinator:7" msgs)]
    (is (= 2 (count rows)) "one row per conversation")
    (is (= #{"c1" "c2"} (set (map :ctx rows)))
        "each row carries the id the coordinator would fetch by")
    (is (= [2 1] (map :n rows)))))

;; =============================================================================
;; Broadcast policy — totality, and the one door to :broadcast
;; =============================================================================

(defspec policy-is-total-and-broadcast-needs-an-argument 200
  (tc-prop/for-all [bcast gen/boolean
                    reason (gen/one-of [(gen/return nil)
                                        (gen/elements (vec bp/admissible-reasons))
                                        (gen/fmap keyword (gen/not-empty gen/string-alphanumeric))])
                    to (gen/one-of [(gen/return nil) gen-ling-id])]
    (let [{:keys [verdict]} (bp/decide {:broadcast? bcast :broadcast-reason reason :to to})]
      (and (contains? #{:direct :spawner :broadcast} verdict)
           ;; The only door to :broadcast: asked for it, named no peer, and
           ;; gave an argument the policy admits.
           (or (not= :broadcast verdict)
               (and bcast (nil? to) (contains? bp/admissible-reasons reason)))))))

(deftest policy-downgrades-rather-than-drops-test
  (testing "a refused broadcast still has a route"
    (let [out (bp/apply-policy {:broadcast? true :message "hi"})]
      (is (nil? (:broadcast? out)) "the flag is gone")
      (is (= :no-reason-given (:broadcast-refused out)) "and the sender is told why")
      (is (= "hi" (:message out)) "but the message survives")))
  (testing "a refused broadcast that named a peer becomes a directed message"
    (is (= :direct (:verdict (bp/decide {:broadcast? true :to "ling-b"}))))))

(deftest policy-admits-a-real-argument-test
  (doseq [r bp/admissible-reasons]
    (is (= :broadcast (:verdict (bp/decide {:broadcast? true :broadcast-reason r})))
        (str r " is an admissible argument")))
  (testing "the MCP transport's string spelling is accepted"
    (is (= :broadcast (:verdict (bp/decide {:broadcast? true :broadcast-reason "halt"}))))
    (is (= :broadcast (:verdict (bp/decide {:broadcast? true :broadcast-reason ":halt"}))))))

(deftest policy-budget-gate-test
  (testing "volume alone refuses, once a caller tracks it"
    (is (= :broadcast (:verdict (bp/decide {:broadcast? true :broadcast-reason :halt}
                                           {:recent-broadcasts 2 :budget 8}))))
    (is (= :budget-exhausted (:refused (bp/decide {:broadcast? true :broadcast-reason :halt}
                                                  {:recent-broadcasts 8 :budget 8})))))
  (testing "a caller that does not track volume is never bitten by it"
    (is (= :broadcast (:verdict (bp/decide {:broadcast? true :broadcast-reason :halt} nil))))))

;; =============================================================================
;; Payload elision — bounded AND lossless
;; =============================================================================

(defspec elision-is-bounded-and-lossless 100
  (tc-prop/for-all [s (gen/not-empty gen/string-alphanumeric)
                    pad (gen/choose 0 2000)]
    (let [text (apply str s (repeat pad "x"))
          cap 200
          store (atom {})
          put! (fn [full]
                 (let [id (str "ctx-" (count @store))]
                   (swap! store assoc id full)
                   id))
          {:keys [text out-text ref] :as _res} (let [r (pref/elide! text cap put!)]
                                                 {:text text :out-text (:text r) :ref (:ref r)})]
      (and ;; bounded: what travels never blows past the cap by more than the marker
           (<= (count out-text) (+ cap pref/ref-marker-cost))
           ;; lossless: whatever was cut is recoverable by the id in the text
           (if ref
             (= text (get @store (pref/parse-ref out-text)))
             (= text out-text))))))

(deftest elision-leaves-short-payloads-alone-test
  (let [calls (atom 0)
        put! (fn [_] (swap! calls inc) "ctx-never")]
    (is (= {:text "short"} (pref/elide! "short" 400 put!)))
    (is (zero? @calls) "a swarm of short messages never touches the store")))

(deftest elision-degrades-when-the-store-refuses-test
  (testing "a message that arrives cut beats a message that does not arrive"
    (let [long-text (apply str (repeat 900 "z"))
          {:keys [text ref]} (pref/elide! long-text 200 (fn [_] (throw (ex-info "down" {}))))]
      (is (nil? ref))
      (is (str/ends-with? text "…"))
      (is (< (count text) 300)))))

;; =============================================================================
;; A2A projection — the envelope a foreign client would read
;; =============================================================================

(deftest a2a-round-trip-preserves-routing-test
  (let [shout {:agent-id "ling-a" :event-type :progress :message "hello peer"
               :to "ling-b" :context-id "a2actx-9" :shout-id "msg-1"
               :task "build" :deliberate? true :ref "ctx-77"
               :timestamp 1234 :project-id "hive"}
        back (a2a/a2a->hive (a2a/hive->a2a shout))]
    (is (= "ling-a" (:agent-id back)))
    (is (= "ling-b" (:to back)))
    (is (= "a2actx-9" (:context-id back)))
    (is (= "msg-1" (:shout-id back)))
    (is (= "hello peer" (:message back)))
    (is (= "build" (:task back)))
    (is (true? (:deliberate? back)))
    (is (= "ctx-77" (:ref back)))
    (is (= "progress" (:event-type back)))
    (is (= 1234 (:timestamp back)))
    (is (= "hive" (:project-id back)))))

(deftest a2a-envelope-is-spec-shaped-test
  (let [m (a2a/hive->a2a {:agent-id "ling-a" :event-type :progress :message "hi"
                          :to "ling-b" :context-id "a2actx-1"})]
    (testing "the fields A2A names are spelled the way A2A spells them"
      (is (string? (:messageId m)))
      (is (contains? a2a/roles (:role m)))
      (is (vector? (:parts m)))
      (is (= "text" (:kind (first (:parts m)))))
      (is (= "a2actx-1" (:contextId m))))
    (testing "and everything hive-specific rides in metadata under a declared extension"
      (is (= [a2a/extension-uri] (:extensions m)))
      (is (= "ling-b" (get (:metadata m) a2a/meta-to)))
      (is (= "ling-a" (get (:metadata m) a2a/meta-from))))))

(deftest a2a-accepts-a-foreign-message-test
  (testing "a Message from an agent that never heard of hive still projects"
    (let [foreign {:messageId "m-1" :role "ROLE_AGENT"
                   :parts [{:kind "text" :text "line one"}
                           {:kind "data" :data {:x 1}}
                           {:kind "text" :text "line two"}]
                   :contextId "c-1"}
          hive (a2a/a2a->hive foreign)]
      (is (= "line one\nline two" (:message hive)) "text parts concatenate, data parts contribute nothing")
      (is (= "c-1" (:context-id hive)))
      (is (= "progress" (:event-type hive)) "no event metadata means it is just activity")
      (is (nil? (:to hive)) "and with no routing extension it is not directed"))))

(deftest event-to-task-state-is-total-test
  (doseq [e ["started" "progress" "completed" "error" "aborted" "ask" "something-new" nil]]
    (is (contains? a2a/task-states (a2a/event->task-state e))
        (str e " maps into the A2A TaskState enum")))
  (is (a2a/terminal-state? (a2a/event->task-state "completed")))
  (is (a2a/interrupted-state? (a2a/event->task-state "ask")))
  (is (not (a2a/terminal-state? (a2a/event->task-state "progress")))))

;; =============================================================================
;; The tool surface: what the MCP layer carries, and what the model is told
;; =============================================================================

(defn- shout-tool []
  (first (filter #(= "hivemind_shout" (:name %)) hmt/tools)))

(deftest shout-schema-declares-the-routing-params-test
  (testing "a param the schema does not declare is DROPPED at the MCP boundary"
    ;; Measured 2026-09-07 on `swarm ling-wave dispatch`: routing was
    ;; contributed, the schema was not, and `providers` silently vanished.
    ;; Directed addressing dies the same quiet death if `to` is not declared
    ;; in BOTH the leaf tool and the consolidated root.
    (let [leaf (get-in (shout-tool) [:inputSchema :properties])
          root (get-in chm/tool-def [:inputSchema :properties])]
      (doseq [p ["to" "context_id" "broadcast" "broadcast_reason"]]
        (is (contains? leaf p) (str p " missing from hivemind_shout's schema"))
        (is (contains? root p) (str p " missing from the consolidated hivemind schema"))))))

(deftest shout-description-teaches-directed-addressing-test
  ;; A controlled run on 2026-09-15 measured that this description is what
  ;; decides the behaviour: given the same ten scenarios, the OLD description
  ;; (no `to`) sent every peer message to the coordinator, and this one
  ;; addressed all five peer scenarios directly, kept the three self-status
  ;; ones on the spawner route, and broadcast the two genuinely global ones
  ;; WITH an admissible reason. The description is load-bearing, so it is
  ;; pinned rather than left to drift.
  (let [d (:description (shout-tool))]
    (testing "it names the directed address and says who does NOT pay"
      (is (re-find #"(?i)\bto\b" d))
      (is (re-find #"(?i)nobody\s+else" d))
      (is (re-find #"(?i)not the coordinator" d)))
    (testing "it frames broadcast as an exception that must be argued for"
      (is (re-find #"(?i)exception" d))
      (is (re-find #"broadcast_reason" d))
      (doseq [r ["halt" "membership" "shared-discovery" "coordinator-directive"]]
        (is (re-find (re-pattern r) d) (str "admissible reason " r " not offered to the model"))))
    (testing "and the reasons it offers are exactly the ones the policy admits"
      (is (= (set (map name bp/admissible-reasons))
             (set (get-in (shout-tool) [:inputSchema :properties "broadcast_reason" :enum])))
          "the schema's enum and the policy's closed set must not drift apart"))))

;; =============================================================================
;; Redeeming a conversation id
;; =============================================================================

(deftest fetch-conversation-redeems-the-id-the-summary-names-test
  ;; The coordinator is shown a peer-traffic row naming a contextId instead of
  ;; the messages. That row is only worth its characters if the id can be
  ;; cashed in; an id nothing can redeem is decoration.
  (let [msgs [{:agent-id "ling-a" :to "ling-b" :context-id "c-1" :event-type :progress
               :message "q1" :timestamp 10 :project-id "p"}
              {:agent-id "ling-b" :to "ling-a" :context-id "c-1" :event-type :progress
               :message "a1" :timestamp 11 :project-id "p"}
              {:agent-id "ling-a" :to "ling-c" :context-id "c-2" :event-type :progress
               :message "other" :timestamp 12 :project-id "p"}]]
    (pb/reset-all-cursors!)
    (pb/clear-backbone-buffer!)
    (pb/register-message-source! (fn [] msgs))
    (try
      (testing "it returns exactly that conversation, oldest first"
        (let [rows (pb/fetch-conversation "c-1")]
          (is (= 2 (count rows)))
          (is (= ["q1" "a1"] (mapv :m rows)))
          (is (= ["ling-b" "ling-a"] (mapv :to rows)))))
      (testing "reading is not receiving: no cursor moved, so the drain is unaffected"
        (let [before @pb/agent-read-cursors
              _ (pb/fetch-conversation "c-1")
              after @pb/agent-read-cursors]
          (is (= before after)))
        ;; and the recipient still receives its message afterwards
        (is (seq (pb/get-messages "ling-b" :project-id "p"))))
      (testing "an unknown id answers empty rather than throwing"
        (is (= [] (pb/fetch-conversation "no-such-context"))))
      (testing "a blank id is refused rather than matching everything"
        (is (nil? (pb/fetch-conversation "")))
        (is (nil? (pb/fetch-conversation nil))))
      (finally
        (pb/register-message-source! (fn [] []))
        (pb/reset-all-cursors!)))))
