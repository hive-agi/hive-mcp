(ns hive-mcp.test.stub.isolated-registry
  "A fresh, empty set of process-wide extension registries per test.

   The extension registries are defonce singletons shared by the whole JVM:
   the opaque extension slot, the dynamic tool slot, the owned schema ledger
   (hive-mcp.extensions.registry), the composite command contribution store
   (hive-addon.registry.commands) and the departures ledger
   (hive-mcp.extensions.departures). A test that reads them sees whatever an
   earlier test or a loaded addon left there; a test that writes them leaks
   into every later test.

   `with-fresh-registries` swaps every one of those stores for an empty value
   for the duration of F and puts the previous contents back afterwards, on
   normal and exceptional exit alike. Inside F a test sees only what it
   arranged itself; outside F nothing it did is visible.

   API:
     (with-fresh-registries f)  clojure.test :each fixture
     (registry-state)           {store-name contents}, for assertions
     (fresh-registries?)        true when every store is empty"
  (:require [hive-mcp.extensions.registry]
            [hive-mcp.extensions.departures]
            [hive-addon.registry.commands]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- store-atoms
  "Every process-wide registry store, keyed by a stable name, as the atom that
   holds its contents."
  []
  {:extensions    (:state @#'hive-mcp.extensions.registry/ext-slot)
   :tools         (:state @#'hive-mcp.extensions.registry/tool-slot)
   :schemas       @#'hive-mcp.extensions.registry/schema-ledger
   :contributions @#'hive-addon.registry.commands/contributions
   :departures    @#'hive-mcp.extensions.departures/ledger})

(defn registry-state
  "{store-name contents} for every process-wide registry store."
  []
  (update-vals (store-atoms) deref))

(defn fresh-registries?
  "True when every registry store is empty."
  []
  (every? empty? (vals (registry-state))))

(defn with-fresh-registries
  "clojure.test fixture: run F against empty registries, then restore exactly
   the contents every store held before."
  [f]
  (let [atoms (store-atoms)
        prior (update-vals atoms deref)]
    (try
      (doseq [a (vals atoms)] (reset! a {}))
      (f)
      (finally
        (doseq [[k a] atoms] (reset! a (get prior k)))))))
