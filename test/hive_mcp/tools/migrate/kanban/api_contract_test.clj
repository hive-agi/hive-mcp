(ns hive-mcp.tools.migrate.kanban.api-contract-test
  "Contract facets for the migrator facade: make-deps keeps every override
   it is given, and the facade verbs delegate to the use cases over them."
  (:require [clojure.test :refer [deftest is]]
            [hive-dsl.result :as r]
            [hive-mcp.tools.migrate.kanban.api :as api]
            [hive-mcp.tools.migrate.kanban.ports :as ports]
            [hive-mcp.tools.migrate.kanban.state :as st]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private slots
  [:source-lister :source-reader :target-reader :target-probe
   :target-durability :writer :state])

(defn overridden-deps
  "Subject: make-deps with every slot overridden by a marker value."
  [marker]
  (apply api/make-deps (mapcat (fn [k] [k [marker k]]) slots)))

(deftrifecta-from-schema make-deps-keeps-every-override
  #'overridden-deps
  {:in [:enum :m1 :m2 :m3]
   :out api/Deps
   :rel (fn [marker out] (= out (into {} (map (fn [k] [k [marker k]])) slots)))
   :num-tests 20})

(defn- deps []
  {:state (st/make (str (java.io.File/createTempFile "kanban-mig-api" ".edn")))
   :source-lister (reify ports/IIdLister (list-ids [_] (r/ok {:ids ["a"] :per-collection {}})))})

(golden/deftest-golden api-facade-golden
  "test/golden/hive-mcp/tools/migrate/kanban-api.edn"
  (let [d (deps)]
    {:init   (api/init-ids! d)
     :status (update (api/status d) :ok dissoc :phase-a-at)
     :reset  (api/reset! d)}))

(deftest reset-answers-the-initial-state
  (is (= 0 (get-in (api/reset! (deps)) [:ok :cursor]))))
