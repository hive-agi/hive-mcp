(ns hive-mcp.multi.core-seed-test
  "Name-matched facets for `hive-mcp.multi.core-seed/install!`.

     golden   what install! seeds: the per-registry counts it returns and the
              keys :multi/core then owns in each child registry
     defspec  install! converges from any partial wipe: whatever subset of
              :multi/core entries is deregistered first, install! restores
              exactly the pristine seed and answers the same counts

   install! touches only the :multi/core owner, so addon entries survive and
   the fixture restores the image by calling it once more."
  (:require [clojure.test :refer [use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.multi.core-seed :as core-seed]
            [hive-mcp.multi.registry.aliases :as r-aliases]
            [hive-mcp.multi.registry.batchables :as r-batchables]
            [hive-mcp.multi.registry.tools :as r-tools]
            [hive-mcp.multi.registry.verbs :as r-verbs]
            [hive-test.golden :refer [deftest-golden]]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f] (try (log/with-min-level :warn (f))
               (finally (log/with-min-level :warn (core-seed/install!))))))

(def ^:private registries
  {:tools      {:snapshot r-tools/snapshot      :dereg r-tools/deregister-by-owner!}
   :verbs      {:snapshot r-verbs/snapshot      :dereg r-verbs/deregister-by-owner!}
   :aliases    {:snapshot r-aliases/snapshot    :dereg r-aliases/deregister-by-owner!}
   :batchables {:snapshot r-batchables/snapshot :dereg r-batchables/deregister-by-owner!}})

(defn- core-view
  "The keys :multi/core owns per child registry, sorted."
  []
  (into (sorted-map)
        (for [[k {:keys [snapshot]}] registries]
          [k (vec (sort (get-in (snapshot) [:data :by-owner :multi/core])))])))

(deftest-golden install-seeds-the-core-owner-golden
  "test/golden/hive-mcp/multi/core-seed-install.edn"
  (let [counts (core-seed/install!)]
    {:counts counts
     :owned  (core-view)}))

(defspec install-converges-from-any-partial-wipe 25
  (prop/for-all [wiped (gen/vector-distinct (gen/elements (keys registries)) {:max-elements 4})]
    (let [pristine (do (core-seed/install!) (core-view))
          counts   (core-seed/install!)]
      (doseq [k wiped] ((get-in registries [k :dereg]) :multi/core))
      (and (= counts (core-seed/install!))
           (= pristine (core-view))))))
