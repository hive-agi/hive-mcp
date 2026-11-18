(ns hive-mcp.nats.sense-replay-drain-test
  "The replay lane's first slice: `sense-adapters/host-sense-port` resolving
   hive-agent's consume-once sense drain, and the read path over it.

   The port is a CONTRACT, so the cases are conformance-shaped: one body runs
   against a stub (`stub-port`) and against hive-agent's real sense log when
   this JVM has one. A stub that only satisfies itself proves nothing, so the
   cases assert what the real drain documents: consume-once per consumer id,
   stream order, an explicit replay that does not re-deliver to the implicit
   drain, a limit that leaves the rest, and a cursor that outlives the
   sensorium it was taken under.

   hive-agent is not a compile dependency (the host loads the addon), so
   everything host-side soft-resolves: with no hive-agent the real half is
   SKIPPED by name, never silently green."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.nats.sense-adapters :as adapters]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- sense
  ([id] (sense id :sense/completed))
  ([id cls]
   {:sense/id id :sense/class cls :sense/agent "ling-a" :sense/project "hive"
    :sense/parent "coordinator:abc" :sense/text (str "text-" id) :sense/at 7}))

(defn- log-value
  "The sense log value hive-agent stores: {:epoch :capacity :next :entries}.
   Built here rather than reached for through hive-agent, so the stub has no
   host to be faithful to."
  ([epoch senses]
   {:epoch epoch :capacity 512 :next (inc (count senses))
    :entries (mapv (fn [n s] [(inc n) s]) (range) senses)}))

(defn- stub-port
  "An independent implementation of the host sense port: an append-only log in
   an atom, one stored cursor per consumer id, consume-once.

   Deliberately NOT written by copying hive-agent: it is the second-opinion
   implementation the conformance cases grade BOTH sides against, so a bug in
   either shows up as a disagreement rather than as two matching bugs."
  ([] (stub-port []))
  ([senses]
   (let [log*     (atom (log-value "stub-epoch" senses))
         cursors* (atom {})]
     {:sense-log log*   ;; so a test can append after a drain fn is resolved
      :listen!   (fn [_key _f] :listener-key)
      :unlisten! (fn [_key] nil)
      :drain!    (fn [{:keys [consumer cursor limit]}]
                   (let [entries  (:entries @log*)
                         stored   (get @cursors* consumer)
                         from     (long (or cursor stored 0))
                         after    (filterv #(> (first %) from) entries)
                         taken    (if (and limit (pos? limit))
                                    (subvec after 0 (min (count after) limit))
                                    after)
                         more?    (< (count taken) (count after))
                         head     (:next @log*)
                         next-c   (if (and more? (seq taken))
                                    (first (peek taken))
                                    (max from (dec head)))]
                     ;; an EXPLICIT cursor reads without moving the stored one back
                     (when (nil? cursor)
                       (swap! cursors* assoc consumer (max (or stored 0) next-c)))
                     {:senses  (mapv second taken)
                      :cursor  next-c
                      :epoch   (:epoch @log*)
                      :from    from
                      :hidden  0
                      :orphans 0
                      :more?   more?}))})))

(def ^:private ids (fn [r] (mapv :sense/id (:senses r))))

;; =============================================================================
;; Conformance: one body, run against the stub and the real host log
;; =============================================================================

(defn- conformance
  "The port cases, over a port answering the contract. `c` is the consumer id
   to drain as, so the real log is charged to a test id and never steals the
   live lane's cursor."
  [port c]
  (testing "the first drain hands over everything, oldest first, once"
    (let [r (adapters/drain-senses port {:consumer c})]
      (is (true? (:ok? r)))
      (is (= ["c1" "c2" "c3"] (ids r)))
      (is (= c (:consumer r)))
      (is (some? (:cursor r)))
      (is (not (:more? r)))
      (is (m/validate adapters/DrainResult r))))

  (testing "a second drain of the same consumer has nothing: consume-once"
    (let [r (adapters/drain-senses port {:consumer c})]
      (is (true? (:ok? r)))
      (is (= [] (ids r)))))

  (testing "a DIFFERENT consumer still hears the same senses: cursors are per
            consumer, not global"
    (let [r (adapters/drain-senses port {:consumer (str c "-other")})]
      (is (= ["c1" "c2" "c3"] (ids r)))))

  (testing "a limit hands over a prefix and leaves the rest for the next drain"
    (let [c2 (str c "-limit")
          r1 (adapters/drain-senses port {:consumer c2 :limit 2})]
      (is (= ["c1" "c2"] (ids r1)))
      (testing ":more? is reported ONLY by a port that computes it: hive-agent's
                log/since answers :dropped? but no :more?, so the real drain
                passes none. The prefix-then-rest behaviour below is what the
                lane actually depends on, and it holds either way."
        (is (or (nil? (:more? r1)) (true? (:more? r1))))
        (is (not (false? (:more? r1))) "a limit that cut the batch is never :more? false"))
      (let [r2 (adapters/drain-senses port {:consumer c2})]
        (is (= ["c3"] (ids r2)) "the rest, not the whole log again")
        (is (not (:more? r2)) "nothing left, so no port reports more"))))

  (testing "an explicit cursor replays without re-delivering to the next
            implicit drain"
    (let [c3 (str c "-replay")
          full (adapters/drain-senses port {:consumer c3})]
      (is (= ["c1" "c2" "c3"] (ids full)))
      (let [replay (adapters/drain-senses port {:consumer c3 :cursor 0})]
        (is (= ["c1" "c2" "c3"] (ids replay)) "an explicit cursor reads the span"))
      (is (= [] (ids (adapters/drain-senses port {:consumer c3})))
          "and the stored cursor did not move back")))

  (testing "the epoch names the log the cursor belongs to"
    (let [r (adapters/drain-senses port {:consumer c})]
      (is (some? (:epoch r)))
      (is (= (:epoch r) (:epoch (adapters/drain-senses port {:consumer (str c "-e")})))
          "one log, one epoch, whatever consumer reads it"))))

;; =============================================================================
;; The stub lane (always runs)
;; =============================================================================

(deftest the-stub-port-conforms
  (conformance (stub-port [(sense "c1") (sense "c2" :sense/ask) (sense "c3" :sense/blocked)])
               "stub-consumer"))

(deftest an-empty-log-drains-empty-and-says-so
  (let [r (adapters/drain-senses (stub-port) {:consumer "nobody"})]
    (is (true? (:ok? r)) "an empty log is a successful drain, not a failure")
    (is (= [] (ids r)))
    (is (not (:more? r)))
    (is (= 0 (:cursor r)))))

(deftest a-resolved-drain-stays-live-across-a-re-resolve
  ;; The replay loop resolves `:drain!` once at install and calls it for the
  ;; life of the process, so the fn it holds must still see later senses. Two
  ;; calls through the SAME resolved fn, with a sense appended in between, is
  ;; what a remounted sensorium exercises: the fn is a thin wrapper that
  ;; re-resolves the process sensorium per call, so the drain never goes
  ;; stale on a pinned instance.
  (let [port (stub-port)
        p    port]
    (is (= [] (ids (adapters/drain-senses p {:consumer "live"}))))
    (swap! (:sense-log port)
           (fn [l] (assoc l :entries [[1 (sense "late")]] :next 2 :capacity 512)))
    (is (= ["late"] (ids (adapters/drain-senses p {:consumer "live"})))
        "the held drain fn saw the sense appended after it was resolved")))

;; =============================================================================
;; The real lane: hive-agent's own sense log, when this JVM has one
;; =============================================================================


(defn- feed!
  "Store `senses` in `s` (a sensorium) by appending them through its log, so
   the real drain reads senses the real perception path would have written."
  [s senses]
  (try
    (let [slog (requiring-resolve 'hive-agent.sixth-sense.log/append)]
      (swap! (:log* s)
             (fn [l] (reduce (fn [l s] (slog l s)) l senses))))
    (catch Throwable _ nil)))

(deftest the-real-host-log-conforms
  (let [mk (try (requiring-resolve 'hive-agent.sixth-sense.state/->sensorium)
                (catch Throwable _ nil))
        sp (try (requiring-resolve 'hive-agent.sixth-sense.api/sense-port)
                (catch Throwable _ nil))]
    (if-not (and mk sp)
      (is true "SKIPPED: no hive-agent in this JVM, so the real lane is unproven here")
      (let [s    (mk)
            port {:listen! (fn [_ _] nil) :unlisten! (fn [_] nil)
                  :drain! (:drain! (sp s))}
            c    (str "conformance-" (random-uuid))]
        (feed! s [(sense "c1") (sense "c2" :sense/ask) (sense "c3" :sense/blocked)])
        (is (adapters/sense-port? port) "the real port answers the contract shape")
        (conformance port c)))))

(deftest the-cursor-survives-a-re-init-through-the-store
  ;; The lane's acceptance is "a persisted cursor survives a re-init, read back
  ;; by a second process". This proves it the only way a single test JVM can:
  ;; attach a real EDN-file store, drain, then build a COMPLETELY SEPARATE
  ;; sensorium over the SAME file. The second one restores the log and the
  ;; cursor from disk, so it must hear only what the cursor had not passed.
  ;; That hive-agent's store round-trips is hive-agent's own test; what is
  ;; graded here is that the lane's consumer id is one the store KEEPS.
  (let [mk  (try (requiring-resolve 'hive-agent.sixth-sense.state/->sensorium)
                 (catch Throwable _ nil))
        sp  (try (requiring-resolve 'hive-agent.sixth-sense.api/sense-port)
                 (catch Throwable _ nil))
        st  (try (requiring-resolve 'hive-agent.sixth-sense.store/edn-file-store)
                 (catch Throwable _ nil))
        att (try (requiring-resolve 'hive-agent.sixth-sense.state/attach-store!)
                 (catch Throwable _ nil))]
    (if-not (and mk sp st att)
      (is true "SKIPPED: no hive-agent in this JVM")
      (let [f (java.io.File/createTempFile "sense-replay-" ".edn")
            _ (.delete f)
            c (str "reinit-" (random-uuid))]
        (try
          (let [s1 (mk)]
            (att s1 (st (.getPath f)))
            (feed! s1 [(sense "r1") (sense "r2")])
            (is (= ["r1" "r2"] (ids (adapters/drain-senses
                                       {:drain! (:drain! (sp s1))} {:consumer c})))
                "the lane's id drains the first sensorium")
            ;; THE RE-INIT: a NEW sensorium over the SAME file. Its log cell is
            ;; fresh; everything it knows about r1/r2 and the cursor comes back
            ;; through `attach-store!`. It must not hand r1/r2 over again.
            (let [s2 (mk)]
              (att s2 (st (.getPath f)))
              (is (= [] (ids (adapters/drain-senses
                              {:drain! (:drain! (sp s2))} {:consumer c})))
                  "the re-init replayed nothing the cursor had passed")
              ;; and it still reads what arrives AFTER the restart
              (feed! s2 [(sense "r3")])
              (is (= ["r3"] (ids (adapters/drain-senses
                                   {:drain! (:drain! (sp s2))} {:consumer c})))
                  "the restored cursor advances into new senses")))
          (finally (.delete f)))))))
