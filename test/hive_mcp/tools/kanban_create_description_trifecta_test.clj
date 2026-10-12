(ns hive-mcp.tools.kanban-create-description-trifecta-test
  "Trifecta contract for the kanban create body (kanban 20260921210947-5c988302).

   Subject: `memory-kanban/create-description`. A kanban create that received
   the multi DSL's `content` body alias used to drop it and report success.
   :description wins; else a non-blank string :content is the body.

   Facets:
   - golden:   description only, content only, both, blank content, map content, neither
   - property: a non-blank string content always survives when no description is given
   - mutation: the original drop-content behaviour and a content-first resolver are caught"
  (:require [clojure.string :as str]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.tools.memory-kanban :as mk]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private cases
  {:description-only {:description "body"}
   :content-only     {:content "body via content"}
   :both             {:description "wins" :content "loses"}
   :blank-content    {:content "   "}
   :map-content      {:content {:x 1}}
   :neither          {:title "t"}})

(def ^:private gen-params
  (gen/fmap (fn [s] {:content (str "c" s)}) gen/string-alphanumeric))

(defn- content-kept?
  "Property predicate (receives OUTPUT): a non-blank content body is never dropped."
  [out]
  (and (string? out) (str/starts-with? out "c")))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(deftrifecta create-description-contract
  hive-mcp.tools.memory-kanban/create-description
  {:golden-path   "test/golden/tools/kanban/create-description.edn"
   :cases         cases
   :gen           gen-params
   :pred          content-kept?
   :property-type :pred
   :num-tests     100
   :mutations
   [["drop-content — the original bug" (fn [{:keys [description]}] description)]
    ["content-first" (fn [{:keys [description content]}] (or content description))]]})
