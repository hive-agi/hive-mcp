(ns hive-mcp.agent.ling.spawn-store-test
  "Golden over the spawn-store seam: the installed-store round trip and
   `ensure-coordinator-session!` driven with an injected recording
   ISpawnStore stub.

   Only the arms that short-circuit before the existing-row lookup are
   driven: that lookup goes to `hive-mcp.swarm.datascript.queries/get-slave`
   directly (a concretion, not the injected store), which needs
   hive-datascript on the classpath. A bare `coordinator`, a ling id, nil
   and a non-string never touch the store."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.agent.ling.spawn-store :as ss]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (try (f) (finally (ss/reset-store!)))))

(defrecord RecordingStore [calls]
  ss/ISpawnStore
  (add-slave! [_ id attrs] (swap! calls conj [:add id attrs]) {:added id})
  (remove-slave! [_ id] (swap! calls conj [:remove id]) nil)
  (update-slave! [_ id u] (swap! calls conj [:update id u]) nil)
  (claims-for-slave [_ id] (swap! calls conj [:claims id]) []))

(deftest-golden-fn ensure-coordinator-session-golden
  "test/golden/agent/spawn-store-coordinator-session.edn"
  (fn []
    (let [store (->RecordingStore (atom []))
          ids   ["coordinator" "coordinator:" "swarm-ling-f02-x" nil 42]]
      {:results (mapv (fn [id] [id (ss/ensure-coordinator-session! store id)]) ids)
       :calls   @(:calls store)
       :store   (do (ss/set-store! store)
                    (ss/add-slave! (ss/get-store) "s1" {:depth 1})
                    (ss/claims-for-slave (ss/get-store) "s1")
                    @(:calls store))})))

(deftest installed-store-round-trip-test
  (let [store (->RecordingStore (atom []))]
    (is (identical? store (ss/set-store! store)))
    (is (identical? store (ss/get-store)))
    (is (instance? hive_mcp.agent.ling.spawn_store.DataScriptSpawnStore (ss/reset-store!)))
    (is (satisfies? ss/ISpawnStore (ss/get-store)))))
