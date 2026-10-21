(ns hive-mcp.tools.memory.migration.core-contract-test
  "Contract facets for the memory migration handlers' refusal arms, which
   answer before any store is reached: a missing or equal project id."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.memory.migration.core :as mc]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private ErrorResponse
  [:map [:type [:= "text"]] [:text :string] [:isError [:= true]]])

(def ^:private Refused
  "Params every rename refuses: an id missing, or both ids equal."
  [:or
   [:map [:new-project-id [:enum "b" "c"]]]
   [:map [:old-project-id [:enum "a" "b"]]]
   [:map [:old-project-id [:= "same"]] [:new-project-id [:= "same"]]]])

(deftrifecta-from-schema rename-project-refuses-without-two-distinct-ids
  #'mc/handle-rename-project
  {:in Refused
   :out ErrorResponse
   :rel (fn [{:keys [old-project-id new-project-id]} out]
          (= (:text out)
             (cond (nil? old-project-id) "old-project-id is required"
                   (nil? new-project-id) "new-project-id is required"
                   :else "old-project-id and new-project-id must be different")))
   :classify (fn [{:keys [old-project-id new-project-id]} _]
               (cond (nil? old-project-id) :no-old (nil? new-project-id) :no-new :else :same))
   :classify-domain #{:no-old :no-new :same}
   :num-tests 150})

(golden/deftest-golden migrate-scoped-refusals-golden
  "test/golden/hive-mcp/tools/memory/migration-core-refusals.edn"
  (sorted-map
   :scoped-no-new    (mc/handle-migrate-scoped {:old-project-id "a"})
   :scoped-no-old    (mc/handle-migrate-scoped {:new-project-id "b"})
   :scoped-same      (mc/handle-migrate-scoped {:old-project-id "a" :new-project-id "a"})
   :scoped-no-target (mc/handle-migrate-scoped {:old-project-id "a" :new-project-id "b"})
   :rename-no-old    (mc/handle-rename-project {:new-project-id "b"})))

(deftest migrate-scoped-needs-ids-or-a-tag-filter
  (is (true? (:isError (mc/handle-migrate-scoped {:old-project-id "a" :new-project-id "b"})))))
