(ns hive-mcp.chroma.maintenance-contract-test
  "Expiry cleanup and the expiring-soon window under instrumentation over the
   in-memory transport: entries are seeded with known durations and expiry
   instants, and the outcome is pinned as a golden."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.chroma.client :as client]
            [hive-mcp.chroma.connection :as conn]
            [hive-mcp.chroma.crud :as crud]
            [hive-mcp.chroma.maintenance :as maint]
            [hive-mcp.config.test-support :as cfg]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.test-fixtures :as fixtures]
            [hive-mcp.test.stub.chroma-transport :as transport]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  #{#'maint/cleanup-expired! #'maint/entries-expiring-soon})

(defn- chroma-without-server [f]
  (let [prior-transport (client/transport)
        prior-provider  (active/get-embedding-provider)]
    (client/set-transport! (transport/->transport))
    (active/set-embedding-provider! (fixtures/->MockEmbedder 16))
    (conn/reset-collection-cache!)
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try
      (cfg/with-config {:embedder {:no-embed-types #{} :routes {}}} (f))
      (finally
        (mi/unstrument! {:filters [(mi/-filter-var contracted)]})
        (conn/reset-collection-cache!)
        (client/set-transport! prior-transport)
        (active/set-embedding-provider! prior-provider)))))

(use-fixtures :each chroma-without-server)

(defn- in-days [n] (str (.plusDays (java.time.ZonedDateTime/now) n)))

(defn- seed! []
  (doseq [[id dur exp] [["past"     "short"     (in-days -2)]
                        ["soon"     "short"     (in-days 2)]
                        ["later"    "medium"    (in-days 20)]
                        ["forever"  "permanent" nil]
                        ["alias"    "session"   nil]]]
    (crud/index-memory-entry! (cond-> {:id id :type "note" :content id :duration dur}
                                exp (assoc :expires exp)))))

;; cleanup runs FIRST: entries-expiring-soon reads through crud/query-entries,
;; which evicts expired entries asynchronously and would race the cleanup.
(deftest-golden expiring-window-and-cleanup
  "test/golden/chroma/maintenance-cleanup.edn"
  (do (seed!)
      (let [cleaned  (maint/cleanup-expired!)
            again    (maint/cleanup-expired!)
            soon-3   (mapv :id (maint/entries-expiring-soon 3))
            soon-30  (mapv :id (maint/entries-expiring-soon 30 :project-id "global"))]
        {:soon-3 soon-3
         :soon-30 soon-30
         :cleaned (update cleaned :deleted-ids (comp vec sort))
         :again again})))

(deftest cleanup-result-validates
  (seed!)
  (is (m/validate maint/CleanupResult (maint/cleanup-expired!))))
