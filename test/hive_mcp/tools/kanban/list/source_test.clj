(ns hive-mcp.tools.kanban.list.source-test
  "Facets for the in-memory board source (the IBoardSource adapter every
   scoped-board consumer is held to).

   ->seq-source answers a record, which EDN cannot read back, so its trifecta
   carries no golden; the golden below pins what the built source ANSWERS
   through the port, over each scope arm."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.kanban.list.source :as src]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Entry
  [:map
   [:id [:enum "a" "b" "c" "d"]]
   [:tags [:vector {:max 3} [:enum "kanban" "todo" "scope:project:p1" "scope:project:p2"]]]])

(def ^:private plan {:required-tags ["kanban"] :window 100})

(defn- project-of [e]
  (some #(when (.startsWith ^String % "scope:project:") (subs % 14)) (:tags e)))

(deftrifecta-from-schema seq-source-answers-every-required-row-on-the-board
  hive-mcp.tools.kanban.list.source/->seq-source
  {:in        [:vector {:max 6} Entry]
   :out       src/BoardSource
   :rel       (fn [in out]
                (let [all (src/scoped-board out {:scope "all"} plan)
                      p1  (src/scoped-board out {:project-id "p1"} plan)]
                  (and (= (filterv #(some #{"kanban"} (:tags %)) in) (:entries all))
                       (true? (:multi-project? all))
                       (every? #(= "p1" (project-of %)) (:entries p1)))))
   ;; the :out schema is a satisfies? predicate: it has no shape to mutate
   :mutation  false
   :num-tests 200})

(def ^:private board
  (src/->seq-source
   [{:id "a" :tags ["kanban" "todo" "scope:project:p1"]}
    {:id "b" :tags ["kanban" "scope:project:p2"]}
    {:id "c" :tags ["todo" "scope:project:p1"]}
    {:id "d" :project-id "p1" :tags ["kanban" "todo"]}]))

(deftest-golden scoped-board-arms
  "test/golden/tools/kanban/list-source-scoped-board.edn"
  {:all          (src/scoped-board board {:scope "all"} plan)
   :no-project   (src/scoped-board board {} plan)
   :p1           (src/scoped-board board {:project-id "p1"} plan)
   :p2-todo      (src/scoped-board board {:project-id "p2"} {:required-tags ["kanban" "todo"] :window 10})
   :p1-window-1  (src/scoped-board board {:project-id "p1"} {:required-tags [] :window 1})
   :empty-source (src/scoped-board (src/->seq-source nil) {:project-id "p1"} plan)})

(deftest seq-source-satisfies-the-port
  (is (satisfies? src/IBoardSource (src/->seq-source [])))
  (is (= [] (:entries (src/scoped-board (src/->seq-source nil) {} plan)))))
