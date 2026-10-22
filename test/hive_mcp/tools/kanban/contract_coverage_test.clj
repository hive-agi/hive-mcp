(ns hive-mcp.tools.kanban.contract-coverage-test
  "Contract coverage gate for tools/kanban and tools/kanban/list (cov-mcp
   m28), plus facets for the kanban namespaces that had none: predicates,
   catchup-block and effects. Every public defn under src/hive_mcp/tools/kanban
   carries an m/=> contract; there are no exemptions."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.kanban.catchup-block :as cb]
            [hive-mcp.tools.kanban.effects :as effects]
            [hive-mcp.tools.kanban.predicates :as pred]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private kanban-dir "src/hive_mcp/tools/kanban")

(def ^:private kanban-nss
  '[hive-mcp.tools.kanban.catchup-block hive-mcp.tools.kanban.coeffects
    hive-mcp.tools.kanban.effects hive-mcp.tools.kanban.events
    hive-mcp.tools.kanban.filters hive-mcp.tools.kanban.migration
    hive-mcp.tools.kanban.port hive-mcp.tools.kanban.predicates
    hive-mcp.tools.kanban.transitions hive-mcp.tools.kanban.update
    hive-mcp.tools.kanban.list.plan hive-mcp.tools.kanban.list.source])

(coverage/deftest-contract-coverage tools-kanban-contracts
  {:paths [kanban-dir]})

(deftest tools-kanban-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths [kanban-dir]})]
    (is (= 83 (count (:universe report))) "every public defn of tools/kanban and tools/kanban/list")
    (is (= 83 (count (:covered report))))))

(deftest every-tools-kanban-contract-generates-both-sides
  (apply require kanban-nss)
  (doseq [ns-sym kanban-nss
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

(def ^:private Status
  [:maybe [:enum "todo" "doing" "review" "done" "inprogress" "inreview" "bogus"]])

(deftrifecta-from-schema normalize-status-maps-the-aliases
  #'pred/normalize-status
  {:in [:cat Status]
   :out [:maybe :string]
   :rel (fn [[s] out] (= out (get {"inprogress" "doing" "inreview" "review"} s s)))
   :golden-path "test/golden/hive-mcp/tools/kanban/normalize-status.edn"
   :num-tests 100})

(deftrifecta-from-schema done-is-exactly-done
  #'pred/done?
  {:in [:cat Status]
   :out :boolean
   :rel (fn [[s] out] (= out (= "done" s)))
   :golden-path "test/golden/hive-mcp/tools/kanban/done.edn"
   :num-tests 100})

(def ^:private Summary
  [:maybe [:map {:closed true}
           [:counts [:map-of [:enum :todo :doing :done] [:int {:min 0 :max 5}]]]
           [:recent-todos [:vector {:max 2} [:map {:closed true}
                                             [:id [:enum "a" "b"]] [:title [:enum "t"]] [:tags [:= []]]]]]
           [:scope-tag [:maybe [:enum "scope:project:x"]]]]])

(deftrifecta-from-schema catchup-section-is-the-summary-plus-a-hint
  #'cb/catchup-section
  {:in [:cat Summary]
   :out [:maybe [:map [:hint :string]]]
   :rel (fn [[{:keys [counts recent-todos] :as s}] out]
          ;; an empty board (no counts, no todos) is not news: nil
          (if (or (pos? (apply + (vals (or counts {})))) (seq recent-todos))
            (= s (dissoc out :hint))
            (nil? out)))
   :classify (fn [_ out] (if out :news :empty))
   :classify-domain #{:news :empty}
   :golden-path "test/golden/hive-mcp/tools/kanban/catchup-section.edn"
   :num-tests 100})

(deftrifecta-from-schema archive-task-data-keeps-the-id
  #'effects/archive-task-data
  {:in [:cat [:maybe [:map {:closed true}
                      [:id [:enum "a" "b"]]
                      [:tags [:vector {:max 2} [:enum "kanban" "x"]]]]]
        [:enum "a" "b"]]
   :out [:map [:id any?] [:tags any?]]
   :rel (fn [[e tid] out]
          (and (= tid (:id out)) (= (or (:tags e) []) (:tags out))))
   :golden-path "test/golden/hive-mcp/tools/kanban/archive-task-data.edn"
   :num-tests 100})

(deftest valid-status-and-entry-predicates
  (is (true? (pred/valid-status? "todo")))
  (is (false? (pred/valid-status? "bogus")))
  (is (true? (pred/kanban-task-type? {:task-type "kanban"})))
  ;; the contract promises [:maybe [:= true]]: a non-kanban type is nil
  (is (nil? (pred/kanban-task-type? {:task-type "note"}))))

