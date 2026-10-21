(ns hive-mcp.tools.memory.decay-test
  "Facets for staleness decay.

   build-decay-plan is pure over its entry (no :last-accessed means 30 idle
   days, so the delta is deterministic): a trifecta from its contract. The
   store-reading and store-writing fns get a golden with the memory port
   satisfied by the stub store (no with-redefs)."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.crystal.core :as crystal]
            [hive-mcp.protocols.memory :as mp]
            [hive-mcp.test.stub.memory-store :as stub]
            [hive-mcp.tools.memory.decay :as decay]
            [hive-mcp.tools.memory.schema :as ms]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private DecayEntry
  "ms/Entry without a PRESENT-but-nil :staleness-beta: that input throws (see
   staleness-beta-nil-throws below), so the property draws around it."
  [:and ms/Entry [:fn {:gen/fmap #(cond-> % (and (contains? % :staleness-beta)
                                                (nil? (:staleness-beta %)))
                                    (dissoc :staleness-beta))
                       :gen/schema ms/Entry}
                  #(some? (get % :staleness-beta 1))]])

(deftrifecta-from-schema build-decay-plan-adds-the-delta
  hive-mcp.tools.memory.decay/build-decay-plan
  {:in        [:cat DecayEntry ms/DecayOpts]
   :out       [:maybe ms/DecayPlan]
   :rel       (fn [[entry opts] out]
                (let [delta (crystal/calculate-decay-delta entry opts)]
                  (if (pos? delta)
                    (and (= delta (:delta out))
                         (= (get entry :staleness-beta 1) (:old-beta out))
                         (== (+ (:old-beta out) delta) (:new-beta out)))
                    (nil? out))))
   :num-tests 300})

;; BUG (not fixed in this wave, reported): src/hive_mcp/tools/memory/decay.clj
;; build-decay-plan reads (get entry :staleness-beta 1), which is nil when the
;; key is PRESENT with nil, then (+ nil delta) throws. The contract (ms/Entry)
;; admits a nil beta and the docstring promises a plan or nil.
(deftest staleness-beta-nil-throws
  (is (thrown? NullPointerException
               (decay/build-decay-plan {:staleness-beta nil} {}))))

(def ^:private entries
  "d1/d2 are scoped to the project the test directory resolves to
   (no-such-project-dir), d3 to another project."
  [{:id "d1" :type "note" :duration "short" :access-count 0 :staleness-beta 1.0
    :tags ["scope:project:no-such-project-dir"] :content "a"}
   {:id "d2" :type "axiom" :duration "permanent" :access-count 0
    :tags ["scope:project:no-such-project-dir"] :content "b"}
   {:id "d3" :type "note" :duration "medium" :access-count 9
    :tags ["scope:project:other"] :content "c"}])

(def ^:private opts {:access-threshold 3 :recency-days 7})

(defn- rounded [m] (into {} (map (fn [[k v]] [k (if (double? v) (/ (Math/round (* v 1e6)) 1e6) v)])) m))

(deftest-golden decay-over-a-stub-store
  "test/golden/tools/memory/decay-stub-store.edn"
  (stub/with-stub-store
    (fn []
      (stub/seed! (mp/get-store) entries)
      {:plans      (mapv #(some-> (decay/build-decay-plan % opts) rounded) entries)
       :fresh      (decay/build-decay-plan (assoc (first entries) :last-accessed (str (java.time.ZonedDateTime/now))) opts)
       :scoped-a   (mapv :id (decay/query-scoped-entries "/tmp/no-such-project-dir" 10))
       :apply-d1   (some-> (decay/apply-decay! (first entries) opts) rounded)
       :apply-d2   (decay/apply-decay! (second entries) opts)
       :cycle      (decay/run-decay-cycle! {:directory "/tmp/no-such-project-dir" :limit 10})})))

(deftest-golden decay-without-a-store
  "test/golden/tools/memory/decay-no-store.edn"
  (stub/with-no-store
    (fn [] {:cycle (decay/run-decay-cycle! {:directory "/tmp/x"})
            :handler (decay/handle-decay {:directory "/tmp/x"})})))

(deftest permanent-axioms-are-not-candidates
  (is (nil? (decay/apply-decay! (second entries) opts))))
