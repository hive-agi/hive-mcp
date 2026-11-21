(ns hive-mcp.await.resumer-conformance
  "Conformance cases every open-resumer implementation must pass, the stub
   in the registry tests and the real :hive-agent-run resumer alike.

   A case set is driven by an owner (the :owner map of an awaiter) and a
   `calls` fn answering what the implementation under test did, so the
   same cases run against a double and the real thing."
  (:require [clojure.test :refer [is testing]]
            [hive-mcp.await.domain :as domain]
            [hive-mcp.await.ports :as ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn wake-request
  "The map the registry hands a resumer for `owner` woken with `payload`."
  [owner payload]
  (let [a (-> (domain/new-awaiter {:id "conf-1" :owner owner :prompt "continue"
                                   :subscription {:type :manual}} 0)
              (assoc :status :woken :wake payload))]
    {:awaiter a :payload payload :prompt (domain/resume-prompt a payload)}))

(defn run-cases!
  "Run the resumer port cases for `owner`. `calls` answers the vector of
   resume requests the implementation recorded since the last case (it may
   reset). Cases: the method is registered; open-resumer answers a fn; one
   wake call resumes exactly once; the resumed prompt carries the payload."
  [owner calls]
  (testing "registered for the owner kind"
    (is (contains? (set (ports/known-resumers)) (:kind owner))))
  (testing "open-resumer answers a fn"
    (is (fn? (ports/open-resumer owner))))
  (testing "one wake resumes exactly once with the payload in the prompt"
    (calls)
    (let [payload {:reason :event :event {:type :manual :data {:k 1}}}
          f       (ports/open-resumer owner)
          _       (f (wake-request owner payload))
          seen    (calls)]
      (is (= 1 (count seen)))
      (is (.contains ^String (str (:prompt (first seen))) ":reason :event")))))
