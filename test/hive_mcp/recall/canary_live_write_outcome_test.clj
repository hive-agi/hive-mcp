(ns hive-mcp.recall.canary-live-write-outcome-test
  "The canary's fixture writer reads add-entry!'s answer through the hive-spi
   write contract: an id is a fixture id; a refusal or a queued ack is not, so
   the fixture id is nil and the probes skip instead of passing on an absent
   fixture. The store is the contract stub with its add answer injected."
  (:require [hive-mcp.recall.canary.live :as live]
            [hive-mcp.test.stub.memory-store :as mem-stub]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private AddAnswer
  "What a store may answer to add-entry!: an id, a hive-spi failure map
   (hive-qdrant or hive-milvus shape), or a queued ack."
  [:or
   [:string {:min 1}]
   [:map [:success? [:= false]] [:error :string]]
   [:map [:error [:enum :timeout :unavailable]]]
   [:map [:queued? [:= true]]]])

(defn fixture-ids-for-answer
  "ensure-fixtures! against an empty stub store whose add-entry! answers ANSWER."
  [answer]
  (live/ensure-fixtures! (mem-stub/->stub nil {:add-entry-fn (constantly answer)})))

(deftrifecta-from-schema fixture-id-only-for-a-landed-write
  fixture-ids-for-answer
  {:in  [:cat AddAnswer]
   :out live/FixtureIds
   :rel (fn [[answer] ids]
          (let [expected (when (string? answer) answer)]
            (and (= [expected expected expected]
                    ((juxt :anchor :superseded :current) ids))
                 (= #{:anchor :superseded :current} (set (:created ids))))))
   :classify (fn [[answer] _] (if (string? answer) :landed :not-landed))
   :classify-domain #{:landed :not-landed}
   :num-tests 60})
