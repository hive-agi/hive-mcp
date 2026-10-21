(ns hive-mcp.tools.kanban.predicates-test
  "Trifectas for the kanban status predicates, driven by their contracts and
   the kanban schema generators (StatusText, MaybeEntry)."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.kanban.predicates :as p]
            [hive-mcp.tools.kanban.schema :as ks]
            [hive-schemas.test :refer [deftrifecta-from-schema deftrifecta-predicate]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private enum->tag
  {"inprogress" "doing" "inreview" "review"})

(deftrifecta-from-schema normalize-status-maps-mcp-spellings
  hive-mcp.tools.kanban.predicates/normalize-status
  {:in          [:maybe ks/StatusText]
   :out         [:maybe :string]
   :rel         (fn [in out] (= out (get enum->tag in in)))
   :n-cases     12
   :golden-path "test/golden/tools/kanban/predicates-normalize-status.edn"})

(deftrifecta-from-schema valid-status-is-canonical-only
  hive-mcp.tools.kanban.predicates/valid-status?
  {:in          [:maybe ks/StatusText]
   :out         :boolean
   :rel         (fn [in out] (= out (contains? #{"todo" "doing" "review" "done"} in)))
   :n-cases     12
   :golden-path "test/golden/tools/kanban/predicates-valid-status.edn"})

(deftrifecta-from-schema done-is-done-under-either-spelling
  hive-mcp.tools.kanban.predicates/done?
  {:in          [:maybe ks/StatusText]
   :out         :boolean
   :rel         (fn [in out] (= out (= "done" in)))
   :n-cases     12
   :golden-path "test/golden/tools/kanban/predicates-done.edn"})

(def ^:private Entry
  "Entries whose content marks (or not) the kanban task type, under keyword
   and string keys, plus non-map content."
  [:maybe
   [:map
    [:id [:enum "t1" "t2"]]
    [:content [:or :nil :string
               [:map [:task-type [:enum "kanban" "note"]]]
               [:map-of [:enum "task-type"] [:enum "kanban" "research"]]]]]])

(deftrifecta-from-schema kanban-entry-reads-the-task-type-marker
  hive-mcp.tools.kanban.predicates/kanban-entry?
  {:in          Entry
   :out         :boolean
   :rel         (fn [in out]
                  (let [c (:content in)]
                    (= out (and (map? c)
                                (or (= "kanban" (:task-type c)) (= "kanban" (get c "task-type")))))))
   :num-tests   200
   :n-cases     12
   :golden-path "test/golden/tools/kanban/predicates-kanban-entry.edn"})

(deftrifecta-predicate kanban-entry-detects-the-schema-entries
  hive-mcp.tools.kanban.predicates/kanban-entry?
  {:schema [:map [:content [:map [:task-type [:= "kanban"]]]]]})

(deftest status-tag-set-covers-both-spellings
  (is (= #{"todo" "doing" "review" "done" "inprogress" "inreview"} p/status-tag-set))
  (is (< (p/priority-order "high") (p/priority-order "priority-medium") (p/priority-order "low"))))
