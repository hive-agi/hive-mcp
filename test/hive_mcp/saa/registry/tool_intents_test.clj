(ns hive-mcp.saa.registry.tool-intents-test
  "Facets for the accumulative tool-intent registry: register! replaces only
   the owner's slice and lookup answers the sorted union across owners
   (property), deregister-by-owner! removes only that owner's slices and
   drops an intent left with none (property), and a golden over the
   lifecycle. support/with-fresh-registry snapshots every SAA child registry
   and restores it afterwards."
  (:require [clojure.test :refer [use-fixtures]]
            [hive-mcp.saa.registry.tool-intents :as reg]
            [hive-mcp.saa.support :as support]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each support/with-fresh-registry)

(def ^:private Owner [:enum :owner/a :owner/b :saa/core])
(def ^:private Intent [:enum :read :search :custom])
(def ^:private Tools [:or [:vector {:max 3} [:enum "a" "b" "c"]] [:set [:enum "a" "d"]] :nil])

(defn- slices [intent] (get-in (reg/snapshot) [:data :by-intent intent]))

(deftrifecta-from-schema register-replaces-the-owners-slice
  #'reg/register!
  {:in [:cat Owner Intent [:map [:tools {:optional true} Tools]]]
   :out [:= :ok]
   :rel (fn [[owner intent entry] _]
          (let [s (slices intent)
                union (vec (sort (reduce into #{} (vals s))))]
            (and (= (set (:tools entry)) (reg/lookup-owner-slice owner intent))
                 (= union (:tools (reg/lookup intent)))
                 (= (if (contains? s :saa/core) :saa/core (first (keys s)))
                    (:owner (reg/lookup intent)))
                 (contains? (get-in (reg/snapshot) [:data :by-owner owner]) intent))))
   :num-tests 150})

(deftrifecta-from-schema deregister-removes-only-the-owners-slices
  #'reg/deregister-by-owner!
  {:in [:cat Owner]
   :out [:set :any]
   :rel (fn [[owner] removed]
          (let [data (:data (reg/snapshot))]
            (and (nil? (get-in data [:by-owner owner]))
                 (every? (fn [[_ s]] (and (seq s) (not (contains? s owner)))) (:by-intent data))
                 (every? keyword? removed))))})

(golden/deftest-golden tool-intents-accumulation-golden
  "test/golden/hive-mcp/saa/registry/tool-intents-accumulation.edn"
  (let [seeded (reg/all-intents)
        _ (reg/reset-for-test!)
        r1 (reg/register! :owner/a :read {:tools ["grep" "read"]})
        r2 (reg/register! :owner/b :read {:tools #{"view"}})
        r3 (reg/register! :saa/core :read {:tools ["cat"]})
        _ (reg/register! :owner/b :custom {})
        union (reg/lookup :read)
        replaced (do (reg/register! :owner/a :read {:tools ["only"]}) (reg/lookup :read))
        removed (vec (sort (reg/deregister-by-owner! :owner/b)))
        after (reg/lookup :read)
        custom (reg/lookup :custom)
        missing (reg/lookup-owner-slice :owner/zz :read)]
    {:seeded seeded :outcomes [r1 r2 r3] :union union :replaced replaced
     :removed removed :after after :custom-after custom :missing-slice missing
     :intents (reg/all-intents)}))
