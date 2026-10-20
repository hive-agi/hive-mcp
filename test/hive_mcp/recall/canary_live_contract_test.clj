(ns hive-mcp.recall.canary-live-contract-test
  "Contract facets for the canary's runtime arm: the fixture writer against a
   stub store, the probes' skip arms, and run!'s verdict shape. run! keeps the
   previous verdict in a private atom; each test that runs it puts the prior
   value back."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.recall.canary :as c]
            [hive-mcp.recall.canary.live :as live]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.memory.stub :as stub]
            [hive-test.golden :as golden]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private SkipIds
  "Fixture ids that never reach the search seam: an id is missing, or the
   supersedes edge was not written."
  [:or
   [:map [:superseded [:maybe [:= "s1"]]] [:current :nil] [:linked? :boolean]]
   [:map [:superseded :nil] [:current [:maybe [:= "c1"]]] [:linked? :boolean]]
   [:map [:superseded [:= "s1"]] [:current [:= "c1"]] [:linked? [:= false]]]
   :nil])

(deftrifecta-from-schema probe-supersession-skips-when-it-cannot-prove-anything
  #'live/probe-supersession
  {:in [:cat SkipIds]
   :out c/Outcome
   :rel (fn [[ids] out]
          (and (= :supersession (:label out))
               (= :skipped (:status out))
               (= (:reason out) (if (and (:superseded ids) (:current ids))
                                  "supersedes edge could not be written"
                                  "supersession fixtures absent"))))
   :classify (fn [_ out] (:reason out))
   :classify-domain #{"supersedes edge could not be written" "supersession fixtures absent"}
   :golden-path "test/golden/hive-mcp/recall/canary-live-probe-supersession.edn"
   :num-tests 100})

(deftrifecta-from-schema probe-anchor-skips-without-an-anchor
  #'live/probe-anchor
  {:in [:cat :any [:maybe [:map [:anchor :nil] [:current {:optional true} [:maybe :string]]]]]
   :out c/Outcome
   :rel (fn [_ out] (= {:label :lexical-anchor :status :skipped
                        :reason "anchor fixture absent and could not be written"} out))
   :num-tests 30})

(golden/deftest-golden ensure-fixtures-writes-once-golden
  "test/golden/hive-mcp/recall/canary-live-ensure-fixtures.edn"
  (let [st (stub/create-store)
        first-pass (live/ensure-fixtures! st)
        second-pass (live/ensure-fixtures! st)
        roles [:anchor :superseded :current]]
    {:first-created (sort (:created first-pass))
     :first-ids-present (mapv #(string? (get first-pass %)) roles)
     :first-linked-boolean (boolean? (:linked? first-pass))
     :second-created (:created second-pass)
     :second-linked (:linked? second-pass)
     :ids-stable (= (select-keys first-pass roles) (select-keys second-pass roles))
     :tags (mapv #(:tags (hive-mcp.protocols.memory/get-entry st (get first-pass %))) roles)}))

(deftest ensure-fixtures-on-a-store-that-refuses-reports-nil-ids
  (let [ids (live/ensure-fixtures! :not-a-store)]
    (is (m/validate live/FixtureIds ids))
    (is (= [nil nil nil] ((juxt :anchor :superseded :current) ids)))
    (is (= #{:anchor :superseded :current} (set (:created ids))))))

(deftest probes-answer-outcomes
  (testing "carto probes skip when hive-carto is absent, and answer an outcome either way"
    (is (m/validate c/Outcome (live/probe-carto-tag "hive-mcp")))
    (is (m/validate c/Outcome (live/probe-carto-semantic "hive-mcp")))
    (is (= :carto-tag (:label (live/probe-carto-tag "hive-mcp")))))
  (testing "the dimension probe always answers its own label"
    (let [o (live/probe-dimension)]
      (is (m/validate c/Outcome o))
      (is (= :dimension-invariant (:label o))))))

(deftest run-answers-a-verdict-and-never-throws
  (let [last-verdict @#'live/last-verdict
        prior @last-verdict
        prior-store (:default (hive-mcp.protocols.memory/registered-stores))]
    ;; No :default store for the pass, so no fixture is written anywhere.
    (hive-mcp.protocols.memory/unregister-store! :default)
    (try
      (with-out-str
        (let [v (live/run! {:carto? false})
              v0 (live/run!)]
          (is (m/validate live/RunVerdict v))
          (is (m/validate live/RunVerdict v0))
          (is (= 3 (:ran v)) "dimension, anchor and supersession, carto off")
          (is (= 5 (:ran v0)) "the two carto probes join by default")
          (is (= (:ok? v0) (empty? (:faults v0))))))
      (finally (reset! last-verdict prior)
               (when prior-store
                 (hive-mcp.protocols.memory/register-store! :default prior-store))))))
