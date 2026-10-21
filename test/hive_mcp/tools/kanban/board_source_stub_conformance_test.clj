(ns hive-mcp.tools.kanban.board-source-stub-conformance-test
  "StubBoardSource (hive-mcp.test.stub.board-source) answers what the real
   adapter, FacadeBoardSource (hive-mcp.tools.memory-kanban.query), answers
   over a stub memory store holding the same cards under the same hierarchy.

   Golden: named scope specs over the fixture board, stub and facade agreeing.
   Property: generated hierarchies, boards and scope specs, stub = facade.
   Mutants of the stub's scope resolution must be caught. A last check runs
   the kanban catchup block once through each source and compares."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.test.stub.board-source :as board]
            [hive-mcp.test.stub.hierarchy :as hier]
            [hive-mcp.tools.catchup.hcr-fixture :as fx]
            [hive-mcp.tools.kanban.catchup-block :as cb]
            [hive-mcp.tools.kanban.list.plan :as list-plan]
            [hive-mcp.tools.kanban.list.source :as src]
            [hive-mcp.tools.memory-kanban.query :as mkq]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- answer [source spec required-tags]
  (let [{:keys [entries multi-project?]}
        (src/scoped-board source spec {:required-tags required-tags :window list-plan/whole-board})]
    {:ids (vec (sort (map :id entries))) :multi-project? multi-project?}))

(defn- both*
  [[parents cards spec required-tags :as in]]
  {:in in
   :stub (answer (board/->board-source parents cards) spec required-tags)
   :facade (hier/with-hierarchy parents
             #(fx/with-kanban-store cards
                (fn [] (answer (mkq/->FacadeBoardSource) spec required-tags))))})

(defn both
  "[parents cards scope-spec required-tags] -> {:in :stub :facade}."
  [in]
  (both* in))

(defn- agreed [{:keys [stub facade]}]
  (if (= stub facade) stub {:diverged {:stub stub :facade facade}}))

(def ^:private real-visible-ids @#'board/visible-ids)

(defn- stub-under
  "Probe mutant: `both` with the stub's scope resolution replaced by F."
  [f]
  (fn [in]
    (let [v #'board/visible-ids]
      (try
        (alter-var-root v (constantly f))
        (both* in)
        (finally (alter-var-root v (constantly real-visible-ids)))))))

(defn- fixture-case [spec & [tags]]
  [hier/fixture fx/kanban-cards spec (or tags ["kanban"])])

(def ^:private gen-case
  (gen/let [n      (gen/choose 1 4)
            picks  (gen/vector (gen/choose -1 3) n)
            specs  (gen/vector (gen/tuple (gen/choose -1 4) (gen/elements ["todo" "done"])) 0 15)
            caller (gen/choose -1 4)
            desc?  gen/boolean
            anc    (gen/elements [true false nil])
            all?   (gen/frequency [[5 (gen/return nil)] [1 (gen/return "all")]])
            todo?  gen/boolean]
    (let [pid     #(str "pbs-" %)
          parents (into {} (map-indexed (fn [i p] [(pid i) (when (< -1 p i) (pid p))]) picks))
          project #(cond (neg? %) "global" (< % n) (pid %) :else "pbs-far")
          cards   (vec (map-indexed (fn [i [p status]]
                                      {:id (str "c" i) :type "note" :project-id (project p)
                                       :tags ["kanban" status]
                                       :content {:task-type "kanban" :title (str "c" i) :status status}})
                                    specs))]
      [parents cards
       {:project-id (when (<= 0 caller) (project caller))
        :include-descendants? desc? :include-ancestors? anc :scope all?}
       (if todo? ["kanban" "todo"] ["kanban"])])))

(deftrifecta stub-board-source-matches-facade
  hive-mcp.tools.kanban.board-source-stub-conformance-test/both
  {:golden-path "test/golden/kanban/board-source-conformance.edn"
   :cases {:a-self-only             (fixture-case {:project-id "a" :include-descendants? false :include-ancestors? false})
           :a-self-and-descendants  (fixture-case {:project-id "a" :include-descendants? true :include-ancestors? false})
           :a-with-ancestors        (fixture-case {:project-id "a" :include-descendants? false :include-ancestors? true})
           :a-todo-tag              (fixture-case {:project-id "a" :include-descendants? true :include-ancestors? false}
                                                  ["kanban" "todo"])
           :ws-self-and-descendants (fixture-case {:project-id "ws" :include-descendants? true :include-ancestors? false})
           :whole-board-all         (fixture-case {:project-id "a" :scope "all"})
           :whole-board-global      (fixture-case {:project-id "global" :include-descendants? true})
           :global-self-only        (fixture-case {:project-id "global" :include-descendants? false :include-ancestors? false})}
   :xf agreed
   :gen gen-case
   :pred (fn [{:keys [stub facade]}] (= stub facade))
   :num-tests 40
   :mutations [["ancestors-always" (stub-under (fn [parents spec]
                                                 (real-visible-ids parents (assoc spec :include-ancestors? true))))]
               ["descendants-never" (stub-under (fn [parents spec]
                                                  (real-visible-ids parents (assoc spec :include-descendants? false))))]]})

(deftest the-block-reads-the-same-summary-through-either-source
  (hier/with-hierarchy hier/fixture
    (fn []
      (doseq [pid ["a" "ws" "b" "a1" "far" "global"]]
        (let [via-stub   (binding [mkq/*board-source* (board/->board-source hier/fixture fx/kanban-cards)]
                           (cb/gather-kanban-summary pid))
              via-facade (fx/with-kanban-store fx/kanban-cards
                           #(binding [mkq/*board-source* (mkq/->FacadeBoardSource)]
                              (cb/gather-kanban-summary pid)))]
          (is (= via-stub via-facade) (str "summary from " pid)))))))
