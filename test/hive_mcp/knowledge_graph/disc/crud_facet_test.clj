(ns hive-mcp.knowledge-graph.disc.crud-facet-test
  "Golden over the disc CRUD lifecycle against a fresh thread-local
   DataScript store (the :carto slot is overridden to it), so no ambient store
   is read or written."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.knowledge-graph.disc.crud :as crud]
            [hive-mcp.knowledge-graph.disc.schema :as ds]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private stable-keys
  [:disc/path :disc/content-hash :disc/project-id :disc/git-commit
   :disc/volatility-class :disc/certainty-alpha :disc/certainty-beta :disc/read-count])

(defn- lifecycle []
  (fixtures/datascript-fixture
   (fn []
     (let [p "/m14/crud/deps.edn"
           q "/m14/crud/server.log"
           before (crud/disc-exists? p)
           eid (crud/add-disc! {:path p :content-hash "h1" :project-id "proj"})
           _ (crud/add-disc! {:path q})
           got (crud/get-disc p)
           updated (crud/update-disc! p {:disc/read-count 2})
           batch (crud/batch-update-discs! [[p {:disc/git-commit "c1"}] [nil {}] ["" {}] [q :not-a-map]])
           after-batch (crud/get-disc p)
           by-proj (count (crud/get-all-discs :project-id "proj"))
           by-map (count (crud/get-all-discs {:project-id "global"}))
           none (count (crud/get-all-discs :project-id "nobody"))
           stats (crud/disc-stats)
           touched (crud/touch-disc! "/m14/crud/new.clj")
           touched-again (crud/touch-disc! "/m14/crud/new.clj")
           removed (crud/remove-disc! p)
           removed-again (crud/remove-disc! p)
           update-missing (crud/update-disc! p {:disc/read-count 9})]
       {:exists-before before
        :eid-int? (int? eid)
        :got (select-keys got stable-keys)
        :got-valid? (m/validate ds/Disc got)
        :updated (select-keys updated stable-keys)
        :batch batch
        :after-batch (select-keys after-batch stable-keys)
        :by-proj by-proj :by-map by-map :none none
        :stats stats
        :touched (select-keys touched stable-keys)
        :touched-again (:disc/read-count touched-again)
        :removed removed :removed-again removed-again
        :update-missing update-missing
        :exists-after (crud/disc-exists? p)}))))

(golden/deftest-golden crud-lifecycle-golden
  "test/golden/hive-mcp/m14-disc-crud.edn"
  (lifecycle))

(deftest add-disc-keeps-its-precondition
  (is (thrown? AssertionError (crud/add-disc! {:path ""})))
  (is (thrown? AssertionError (crud/add-disc! {:path nil}))))
