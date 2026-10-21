(ns hive-mcp.tools.kanban.catchup-block-hcr-trifecta-test
  "The kanban catchup block reads ONE board: the caller's project and its
   descendants (never its ancestors), through the IBoardSource seam
   (`mkq/*board-source*`), gated by the same tag-authoritative HCR rule as
   memory, session todos dropped, buckets counted by status tag. From
   \"global\" it reads the cards stored under project_id \"global\".

   Stubbed world: the fixture hierarchy (hive-mcp.test.stub.hierarchy) and a
   StubBoardSource over the fixture cards (hive-mcp.tools.catchup.hcr-fixture),
   the stub held to FacadeBoardSource by board-source-stub-conformance-test.
   The `-via-facade` golden cases read through FacadeBoardSource over a
   seeded stub memory store instead.

   Golden: counts, project count, recent todos and scope tag per caller.
   Property: over generated hierarchies and boards the summary equals the one
   recomputed from the HCR-filtered stub board. Mutants: ancestors on the
   board, ancestor scope tags admitted, gate skipped, hidden tags kept."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.project.scope :as project-scope]
            [hive-mcp.test.stub.board-source :as board]
            [hive-mcp.test.stub.hierarchy :as hier]
            [hive-mcp.tools.catchup.hcr-fixture :as fx]
            [hive-mcp.tools.catchup.scope-filter :as sf]
            [hive-mcp.tools.kanban.catchup-block :as cb]
            [hive-mcp.tools.kanban.list.plan :as list-plan]
            [hive-mcp.tools.memory-kanban.query :as mkq]
            [hive-mcp.tools.kanban.list.source :as src]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- summary-view [summary]
  (update summary :recent-todos #(mapv :id %)))

(defn- kanban-view*
  "Summary for PID over CARDS under PARENTS, read through StubBoardSource,
   or through FacadeBoardSource over a seeded stub memory store when VIA is
   :facade."
  [[parents cards pid via :as in]]
  {:in in
   :summary (hier/with-hierarchy parents
              #(summary-view
                (if (= :facade via)
                  (fx/with-kanban-store cards
                    (fn [] (binding [mkq/*board-source* (mkq/->FacadeBoardSource)]
                             (cb/gather-kanban-summary pid))))
                  (binding [mkq/*board-source* (board/->board-source parents cards)]
                    (cb/gather-kanban-summary pid)))))})

(defn kanban-view
  "[parents cards pid] -> {:in .. :summary (gather-kanban-summary pid)}."
  [in]
  (kanban-view* in))

(defn- under
  "Probe mutant: `kanban-view` with var V's root bound to MUTANT."
  [v mutant]
  (fn [in]
    (let [original @v]
      (try
        (alter-var-root v (constantly mutant))
        (kanban-view* in)
        (finally (alter-var-root v (constantly original)))))))

(def ^:private real-board-view cb/board-view)

(defn- board-view-with-ancestors
  "Mutant: the parent's board read and admitted as the caller's backlog."
  [project-id]
  (let [{:keys [scope-spec view] :as real} (real-board-view project-id)]
    (if-not (sf/in-project? project-id)
      real
      (let [ancestors (remove (hash-set project-id "global") (project-scope/visible-scopes project-id))]
        {:scope-spec (assoc scope-spec :include-ancestors? true)
         :view       (-> view
                         (update :visible-ids into ancestors)
                         (update :scope-tags into (map project-scope/scope->tag ancestors)))}))))

(defn- board-view-admitting-ancestor-tags
  "Mutant: the board read stays self + descendants, but the gate admits a
   card tagged for an ancestor (`sf/hcr-view`'s default scope tags)."
  [project-id]
  (let [{:keys [view] :as real} (real-board-view project-id)]
    (if-not (sf/in-project? project-id)
      real
      (assoc real :view (sf/hcr-view project-id (disj (:visible-ids view) "global"))))))

;; =============================================================================
;; Oracle
;; =============================================================================

(def ^:private project-prefix "scope:project:")

(defn- tagged? [e t] (boolean (some #{t} (:tags e))))

(defn- visible-on-board?
  "Card visibility on PID's board: fetched by :project-id in self+descendants,
   then the tag rule over self+descendants+global."
  [parents pid card]
  (let [board    (hier/board-ids parents pid)
        v        (conj board "global")
        projects (keep #(when (str/starts-with? % project-prefix) (subs % (count project-prefix)))
                       (:tags card))]
    (and (contains? board (:project-id card))
         (if (or (tagged? card "scope:global") (seq projects))
           (boolean (or (tagged? card "scope:global") (some v projects)))
           true))))

(defn- oracle-summary [parents cards pid]
  (let [rows (->> cards
                  (filter #(tagged? % "kanban"))
                  (remove #(tagged? % list-plan/session-todo-tag))
                  (filter #(visible-on-board? parents pid %)))]
    {:counts       (into {} (map (fn [[k t]] [k (count (filter #(tagged? % t) rows))]))
                         [[:todo "todo"] [:inprogress "doing"] [:inreview "review"] [:done "done"]])
     :recent-todos (->> rows (filter #(tagged? % "todo"))
                        (sort-by :updated #(compare %2 %1)) (take 10) (mapv :id))}))

(def ^:private gen-board
  "[parents cards caller]: a forest of 2-5 `pbk-*` projects and up to 30 cards."
  (gen/let [n      (gen/choose 2 5)
            picks  (gen/vector (gen/choose -1 4) n)
            specs  (gen/vector (gen/tuple (gen/choose 0 5)
                                          (gen/elements ["todo" "doing" "review" "done"])
                                          (gen/vector (gen/elements [:global :p0 :p1 :p2 :p3 :p4 :p5
                                                                     "session-todo" :no-kanban])
                                                      0 2))
                               0 30)
            caller (gen/choose 0 4)]
    (let [pid     #(str "pbk-" %)
          parents (into {} (map-indexed (fn [i p] [(pid i) (when (< -1 p i) (pid p))]) picks))
          project #(if (< % n) (pid %) "pbk-far")
          cards   (vec (map-indexed
                        (fn [i [p status extras]]
                          {:id (str "c" i) :type "note" :project-id (project p)
                           :tags (vec (distinct
                                       (concat (when-not (some #{:no-kanban} extras) ["kanban"])
                                               [status]
                                               (keep #(cond (= :global %) "scope:global"
                                                            (= :no-kanban %) nil
                                                            (keyword? %) (str project-prefix
                                                                              (project (parse-long (subs (name %) 1))))
                                                            :else %)
                                                     extras))))
                           :content {:task-type "kanban" :title (str "c" i) :status status}
                           :updated (format "2026-10-01T10:%02d:%02dZ" (quot i 60) (mod i 60))})
                        specs))]
      [parents cards (pid (mod caller n))])))

(defn- matches-oracle?
  [{[parents cards pid] :in summary :summary}]
  (= (oracle-summary parents cards pid)
     (select-keys summary [:counts :recent-todos])))

(defn- fixture-case
  ([pid] [hier/fixture fx/kanban-cards pid])
  ([pid via] [hier/fixture fx/kanban-cards pid via]))

(deftrifecta kanban-block-is-hcr-scoped
  hive-mcp.tools.kanban.catchup-block-hcr-trifecta-test/kanban-view
  {:golden-path "test/golden/kanban/hcr-catchup-block.edn"
   :cases {:from-a                 (fixture-case "a")
           :from-a1                (fixture-case "a1")
           :from-ws                (fixture-case "ws")
           :from-b                 (fixture-case "b")
           :from-far               (fixture-case "far")
           :from-global            (fixture-case "global")
           :from-a-via-facade      (fixture-case "a" :facade)
           :from-ws-via-facade     (fixture-case "ws" :facade)
           :from-global-via-facade (fixture-case "global" :facade)}
   :xf :summary
   :gen gen-board
   :pred matches-oracle?
   :num-tests 60
   :mutations [["ancestors-on-the-board"  (under #'cb/board-view board-view-with-ancestors)]
               ["ancestor-tags-admitted"  (under #'cb/board-view board-view-admitting-ancestor-tags)]
               ["gate-skipped"            (under #'cb/gate-board (fn [_ es] (vec es)))]
               ["session-todos-kept"      (under #'list-plan/drop-hidden (fn [es _] (vec es)))]]})

(deftest one-board-fetch-self-and-descendants
  (let [source (board/->board-source hier/fixture fx/kanban-cards)]
    (hier/with-hierarchy hier/fixture
      #(binding [mkq/*board-source* source]
         (cb/gather-kanban-summary "a")))
    (is (= [[{:project-id "a" :include-descendants? true :include-ancestors? false}
             {:required-tags ["kanban"] :window list-plan/whole-board}]]
           (board/calls source)))))

(deftest a-failing-board-read-degrades-to-the-empty-summary
  (binding [mkq/*board-source* (reify src/IBoardSource
                                 (scoped-board [_ _ _] (throw (ex-info "board down" {}))))]
    (is (= {:counts {} :recent-todos [] :scope-tag "scope:project:a"}
           (cb/gather-kanban-summary "a")))))
