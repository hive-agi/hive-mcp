(ns hive-mcp.multi.core-seed-reload-test
  "Contract: every load of hive-mcp.multi.core-seed converges the :multi/core seed.

   R1 a load over an image that already loaded core-seed installs every seed the
      registry is missing: tools, verbs, param aliases and batchables.
   R2 a load replaces :multi/core entries left by an earlier load with the ones
      the loaded code builds.
   R3 a plain require of the already-loaded lib does no work, so the registry
      bootstrap must call install! rather than trust require to seed.

   Every wipe targets the :multi/core owner only, so the fixture restores the
   image by calling install!, which re-seeds that owner and nothing else."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.multi.core-seed :as core-seed]
            [hive-mcp.multi.registry :as registry]
            [hive-mcp.multi.registry.tools :as r-tools]
            [hive-mcp.multi.registry.verbs :as r-verbs]
            [hive-mcp.multi.registry.aliases :as r-aliases]
            [hive-mcp.multi.registry.batchables :as r-batchables]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private core-owner :multi/core)

(defn- with-core-seed-restored
  "Run f, then re-seed :multi/core. The multi registry has no restore!, and
   install! touches only the :multi/core owner, so addon entries survive."
  [f]
  (try (f) (finally (core-seed/install!))))

(use-fixtures :each with-core-seed-restored)

(defn- load-core-seed!
  "Load hive-mcp.multi.core-seed again into an image that has already loaded it."
  []
  (require 'hive-mcp.multi.core-seed :reload))

(defn- owned-keys
  "Keys the :multi/core owner holds in one child registry snapshot."
  [snapshot-fn]
  (get-in (snapshot-fn) [:data :by-owner core-owner] #{}))

(defn- core-seed-view
  "The :multi/core seed as a comparable value: the keys it owns per child registry."
  []
  {:tools      (owned-keys r-tools/snapshot)
   :verbs      (owned-keys r-verbs/snapshot)
   :aliases    (owned-keys r-aliases/snapshot)
   :batchables (owned-keys r-batchables/snapshot)})

(defn- assert-seeded!
  "Every kind is seeded before the wipe, so a later equality is not vacuous."
  [view]
  (doseq [k [:tools :verbs :aliases :batchables]]
    (is (seq (get view k)) (str "precondition: :multi/core owns some " k))))

(defn- wipe-core-owner!
  []
  (registry/deregister-by-owner! core-owner))

(defn- stale-handler [_params] {:stale true})

(defrecord ^:private StaleBatchable [])

(defn- stale-entries
  "One :multi/core entry per seeded kind, over a key the seed owns, standing in
   for a seed an earlier load left."
  [{:keys [tools verbs aliases batchables]}]
  {:multi/tool        [{:tool-name (first (sort tools)) :handler stale-handler}]
   :multi/verb        [{:code (first (sort verbs)) :tool "stale" :command "stale"}]
   :multi/param-alias [{:short (first (sort aliases)) :full :stale}]
   :multi/batchable   [{:tool-name (first (sort batchables)) :record (->StaleBatchable)}]})

(deftest r1-load-installs-every-missing-seed
  (testing "a load of core-seed over an emptied :multi/core owner puts every seed back"
    (let [pristine (core-seed-view)]
      (assert-seeded! pristine)
      (wipe-core-owner!)
      (is (every? empty? (vals (core-seed-view))) "precondition: the wipe landed")
      (load-core-seed!)
      (is (= pristine (core-seed-view))))))

(deftest r2-load-replaces-seeds-left-by-an-earlier-load
  (testing "a load rebuilds :multi/core entries that are already present"
    (let [pristine (core-seed-view)
          stale    (stale-entries pristine)
          tool     (-> stale :multi/tool first :tool-name)
          code     (-> stale :multi/verb first :code)
          short    (-> stale :multi/param-alias first :short)
          bname    (-> stale :multi/batchable first :tool-name)]
      (assert-seeded! pristine)
      (doseq [[k entries] stale]
        (is (every? #{:replaced} (registry/register-by-key! core-owner k entries))
            (str "precondition: the stale " k " entry replaced the seed")))
      (is (identical? stale-handler (:handler (r-tools/lookup tool))))
      (load-core-seed!)
      (is (= pristine (core-seed-view)))
      (is (not (identical? stale-handler (:handler (r-tools/lookup tool)))))
      (is (not= "stale" (:tool (r-verbs/lookup code))))
      (is (not= :stale (:full (r-aliases/lookup short))))
      (is (not (instance? StaleBatchable (:record (r-batchables/lookup bname))))))))

(deftest r3-a-plain-require-cannot-reseed-so-the-bootstrap-must-install
  (testing "after a namespace refresh the registry bootstrap runs again while
            core-seed stays in *loaded-libs*: its plain require is a no-op, so
            seeding as a byproduct of loading cannot survive. install! is what
            converges the seed, on a cold load and a warm one alike."
    (let [pristine (core-seed-view)]
      (assert-seeded! pristine)
      (wipe-core-owner!)
      (require 'hive-mcp.multi.core-seed)
      (is (every? empty? (vals (core-seed-view)))
          "a plain require of an already-loaded lib does no work, so the wipe stands")
      (core-seed/install!)
      (is (= pristine (core-seed-view))))))
