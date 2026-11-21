(ns hive-mcp.await.domain-test
  "Property tests for the pure awaiter step: a reached deadline wakes with
   :reason :timeout, dedup values never wake twice, a cancelled awaiter never
   wakes, over any sequence of events an awaiter wakes at most once, and a
   :pending awaiter never wakes but records exactly the wake that arming
   later delivers (the two-phase park contract)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.await.domain :as domain]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def gen-subscription
  (gen/hash-map :type (gen/elements [:manual :ling :inbox])
                :filter (gen/one-of [(gen/return nil)
                                     (gen/map (gen/elements [:k :ci :who]) (gen/elements [1 2 "a"])
                                              {:max-elements 2})])
                :dedup-key (gen/elements [nil :k :id])))

(def gen-pending-awaiter
  (gen/let [sub      gen-subscription
            id       gen/uuid
            deadline (gen/one-of [(gen/return nil) (gen/choose 0 1000)])
            prompt   (gen/one-of [(gen/return nil) gen/string-alphanumeric])]
    (domain/new-awaiter {:id id :owner {:kind :stub} :subscription sub
                         :deadline deadline :prompt prompt}
                        0)))

(def gen-awaiter
  "An ARMED awaiter: a created one, armed at once (no recorded wake)."
  (gen/fmap (fn [a] (:awaiter (domain/arm a nil))) gen-pending-awaiter))

(def gen-event
  (gen/one-of
   [(gen/return nil)
    (gen/hash-map :type (gen/elements [:manual :ling :inbox])
                  :data (gen/map (gen/elements [:k :ci :who :id]) (gen/elements [1 2 "a"])
                                 {:max-elements 4}))]))

(defn- run-steps
  "Fold `events` (pairs [event now]) through step; answers [awaiter wakes]."
  [awaiter events]
  (reduce (fn [[a wakes] [e t]]
            (let [r (domain/step a e t)]
              [(:awaiter r) (cond-> wakes (:wake? r) (conj (:payload r)))]))
          [awaiter []]
          events))

(defspec wakes-at-most-once 300
  (prop/for-all [a      gen-awaiter
                 events (gen/vector (gen/tuple gen-event (gen/choose 0 2000)) 0 20)]
    (let [[a' wakes] (run-steps a events)]
      (and (<= (count wakes) 1)
           (= (= 1 (count wakes)) (= :woken (:status a')))
           (m/validate domain/Awaiter a')))))

(defspec deadline-wakes-with-timeout 300
  (prop/for-all [a     gen-awaiter
                 e     gen-event
                 extra (gen/choose 0 1000)]
    (if-let [d (:deadline a)]
      (let [r (domain/step a e (+ d extra))]
        (and (:wake? r)
             (= :timeout (get-in r [:payload :reason]))
             (= d (get-in r [:payload :deadline]))
             (= :woken (get-in r [:awaiter :status]))))
      (not= :timeout (get-in (domain/step a e Long/MAX_VALUE) [:payload :reason])))))

(defspec dedup-value-never-wakes-twice 300
  (prop/for-all [v    (gen/elements [1 2 "a"])
                 data (gen/map (gen/elements [:ci :who]) (gen/elements [1 2 "a"]) {:max-elements 2})]
    (let [a     (-> (domain/new-awaiter {:id "d" :owner {:kind :stub}
                                         :subscription {:type :manual :dedup-key :k}} 0)
                    (as-> aw (:awaiter (domain/arm aw nil))))
          e     {:type :manual :data (assoc data :k v)}
          r1    (domain/step a e 1)
          ;; the same value offered to an armed awaiter that already saw it
          rearm (assoc (:awaiter r1) :status :armed :wake nil)
          r2    (domain/step rearm e 2)]
      (and (:wake? r1)
           (contains? (get-in r1 [:awaiter :seen]) v)
           (not (:wake? r2))
           (= rearm (:awaiter r2))))))

(defspec cancelled-never-wakes 300
  (prop/for-all [a      gen-awaiter
                 events (gen/vector (gen/tuple gen-event (gen/choose 0 2000)) 0 20)]
    (let [c          (domain/cancel a)
          [c' wakes] (run-steps c events)]
      (and (= :cancelled (:status c))
           (empty? wakes)
           (= c c')
           (= c (domain/cancel c))))))

(defspec pending-never-wakes-and-records-the-first-wake 300
  (prop/for-all [a      gen-pending-awaiter
                 events (gen/vector (gen/tuple gen-event (gen/choose 0 2000)) 0 20)]
    (let [[a' wakes] (run-steps a events)
          recorded   (:pending-wake a')]
      (and (empty? wakes)
           (= :pending (:status a'))
           ;; at most one wake is recorded, and it is the payload a plain wake
           ;; would have carried, so arming cannot deliver anything else
           (or (nil? recorded)
               (contains? #{:event :timeout} (:reason recorded)))
           ;; arming delivers EXACTLY what was recorded, exactly once
           (let [{a'' :awaiter w :pending-wake} (domain/arm a' "run-1")]
             (and (= :armed (:status a''))
                  (= recorded w)
                  (= "run-1" (get-in a'' [:owner :run-id]))))))))

(deftest step-examples
  (let [armed (fn [spec] (:awaiter (domain/arm (domain/new-awaiter spec 0) nil)))
        a (armed {:id "x" :owner {:kind :stub} :deadline 100 :prompt "go on"
                  :subscription {:type :manual :filter {:ci :green}}})]
    (testing "a non-matching event does not wake"
      (is (not (:wake? (domain/step a {:type :manual :data {:ci :red}} 10))))
      (is (not (:wake? (domain/step a {:type :ling :data {:ci :green}} 10)))))
    (testing "a matching event wakes with :reason :event and the event"
      (let [e {:type :manual :data {:ci :green :sha "abc"}}
            r (domain/step a e 10)]
        (is (:wake? r))
        (is (= {:reason :event :event e :awaiter-id "x" :at 10} (:payload r)))))
    (testing "a late matching event after the deadline wakes as :timeout"
      (is (= :timeout (get-in (domain/step a {:type :manual :data {:ci :green}} 100)
                              [:payload :reason]))))
    (testing "a bare tick before the deadline does not wake"
      (is (not (:wake? (domain/step a nil 99)))))
    (testing "resume-prompt carries the continuation and the payload"
      (let [p (domain/resume-prompt a {:reason :timeout})]
        (is (.startsWith ^String p "go on"))
        (is (.contains ^String p ":timeout"))))))

(deftest two-phase-examples
  (let [pending (domain/new-awaiter {:id "p" :owner {:kind :stub} :deadline 50
                                     :subscription {:type :manual :filter {:ci :green}}}
                                    0)]
    (testing "the first matching event while pending is recorded, not woken"
      (let [r (domain/step pending {:type :manual :data {:ci :green :sha "a"}} 10)]
        (is (not (:wake? r)))
        (is (= :pending (get-in r [:awaiter :status])))
        (is (= :event (get-in r [:awaiter :pending-wake :reason])))
        (is (= "a" (get-in r [:awaiter :pending-wake :event :data :sha])))))
    (testing "a second event does not overwrite the first recorded wake"
      (let [once (:awaiter (domain/step pending {:type :manual :data {:ci :green :sha "a"}} 10))
            r    (domain/step once {:type :manual :data {:ci :green :sha "b"}} 20)]
        (is (= "a" (get-in r [:awaiter :pending-wake :event :data :sha])))))
    (testing "a passed deadline while pending is recorded as :timeout"
      (let [r (domain/step pending nil 50)]
        (is (not (:wake? r)))
        (is (= :timeout (get-in r [:awaiter :pending-wake :reason])))))
    (testing "arm binds the run id and hands the recorded wake back once"
      (let [once (:awaiter (domain/step pending {:type :manual :data {:ci :green}} 10))
            r1   (domain/arm once "run-9")
            r2   (domain/arm (:awaiter r1) "run-9")]
        (is (= :armed (get-in r1 [:awaiter :status])))
        (is (= "run-9" (get-in r1 [:awaiter :owner :run-id])))
        (is (some? (:pending-wake r1)))
        (is (nil? (:pending-wake r2)) "arming twice delivers nothing twice")))
    (testing "an armed awaiter with no recorded wake arms with nothing to fire"
      (let [r (domain/arm pending "run-1")]
        (is (= :armed (get-in r [:awaiter :status])))
        (is (nil? (:pending-wake r)))))))

(deftest deferred-answer-is-recognised
  (is (true? (domain/deferred? {:success false :deferred true})))
  (is (false? (domain/deferred? {:spawned "x"})))
  (is (false? (domain/deferred? nil)))
  (is (false? (domain/deferred? {:success false}))))

