(ns hive-mcp.tools.catchup.contract-fixtures
  "Shared fixtures for the catchup contract suites: a seeded StubMemoryStore
   installed as the active store (the IMemoryStore port, no driver), with the
   prior registry restored afterwards, and every catchup cache emptied."
  (:require [hive-mcp.test.stub.memory-store :as stub]
            [hive-spi.memory.registry :as registry]
            [hive-mcp.test-support.registry-isolation :as isolation]
            [hive-mcp.tools.catchup.axiom-cache :as axc]
            [hive-mcp.tools.catchup.block-cache :as blc]
            [hive-mcp.tools.catchup.bundle-cache :as bc]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def seed
  "Entries for project \"p\": a global axiom, a legacy convention-axiom, a
   scoped decision, a priority convention, a plain convention, a session
   summary and a sibling-project decision that must never surface for \"p\"."
  [{:id "e1" :type "axiom" :tags ["scope:global" "clojure"] :content "axiom one"
    :project-id "global" :created "2026-01-01T00:00:00Z"}
   {:id "e2" :type "convention" :tags ["axiom"] :content "legacy axiom"
    :project-id "p" :created "2026-02-01T00:00:00Z"}
   {:id "e3" :type "decision" :tags ["scope:project:p"] :content "decision one"
    :project-id "p" :created "2026-03-01T00:00:00Z"}
   {:id "e4" :type "convention" :tags ["scope:project:p" "catchup-priority"] :content "conv one"
    :project-id "p" :created "2026-01-15T00:00:00Z"}
   {:id "e5" :type "convention" :tags ["scope:project:p"] :content "conv two"
    :project-id "p" :created "2026-02-15T00:00:00Z"}
   {:id "e6" :type "decision" :tags ["scope:project:other"] :content "sibling decision"
    :project-id "other" :created "2026-03-15T00:00:00Z"}])

(defn reset-caches! []
  (axc/invalidate-axioms-cache!)
  (bc/reset-cache!)
  (blc/reset-cache!))

(defn with-seeded-store
  "clojure.test fixture: run T with a stub store over `seed` active."
  [t]
  (isolation/with-restored-store-registry
    (fn []
      (registry/set-store! (stub/->stub seed))
      (reset-caches!)
      (try (t) (finally (reset-caches!))))))
