(ns hive-mcp.tools.memory.migration.scope-contract-test
  "Contract facets for the scope-tag migration handler's refusal arms, which
   answer before any store is reached."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.memory.migration.scope :as ms]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Refused
  [:or
   [:map [:old_scope [:enum "" " " "\t"]] [:new_scope [:enum "x" ""]]]
   [:map [:old_scope [:= "abc"]] [:new_scope [:enum "" "  "]]]
   [:map [:old_scope [:= "same"]] [:new_scope [:= "same"]]]])

(deftrifecta-from-schema migrate-scope-refuses-blank-or-equal-scopes
  #'ms/handle-migrate-scope
  {:in Refused
   :out [:map [:type [:= "text"]] [:text :string] [:isError [:= true]]]
   :rel (fn [{:keys [old_scope new_scope]} out]
          (= (:text out)
             (cond (str/blank? old_scope) "old_scope is required"
                   (str/blank? new_scope) "new_scope is required"
                   :else "old_scope and new_scope must be different")))
   :classify (fn [{:keys [old_scope new_scope]} _]
               (cond (str/blank? old_scope) :no-old
                     (str/blank? new_scope) :no-new
                     :else :same))
   :classify-domain #{:no-old :no-new :same}
   :num-tests 150})

(golden/deftest-golden migrate-scope-refusals-golden
  "test/golden/hive-mcp/tools/memory/migration-scope-refusals.edn"
  (sorted-map
   :no-old (ms/handle-migrate-scope {:new_scope "b"})
   :no-new (ms/handle-migrate-scope {:old_scope "a"})
   :same   (ms/handle-migrate-scope {:old_scope "a" :new_scope "a"})))

(deftest refusals-are-errors
  (is (true? (:isError (ms/handle-migrate-scope {})))))
