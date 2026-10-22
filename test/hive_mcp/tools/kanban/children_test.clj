(ns hive-mcp.tools.kanban.children-test
  "Open KG children surfaced on kanban completion, on a stub KG + stub store."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.kanban.children :as children]
            [hive-mcp.tools.memory-kanban :as mk]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- card [id status]
  {:id id
   :tags ["kanban" status]
   :content {:task-type "kanban" :title (str "card " id)
             :status status :priority "medium"}})

(def ^:private store
  {"parent"   (card "parent" "doing")
   "slice-a"  (card "slice-a" "todo")
   "slice-b"  (card "slice-b" "doing")
   "slice-c"  (card "slice-c" "done")
   "deriv"    (card "deriv" "review")
   "contained" (card "contained" "todo")
   "planned"  (card "planned" "todo")
   "note"     {:id "note" :content "plain memory, not a card"}
   "upstream" (card "upstream" "todo")})

(defn- edge [from rel to] {:kg-edge/from from :kg-edge/relation rel :kg-edge/to to})

(def ^:private edges
  [(edge "slice-a" :depends-on "parent")     ; child waits on parent
   (edge "slice-b" "depends-on" "parent")    ; string relation tolerated
   (edge "slice-c" :depends-on "parent")     ; done: skipped
   (edge "deriv" :derived-from "parent")
   (edge "parent" :contains "contained")
   (edge "planned" :implements "parent")
   (edge "slice-a" :derived-from "parent")   ; second link: listed once
   (edge "parent" :depends-on "slice-b")     ; parent's own dependency: not a child
   (edge "note" :derived-from "parent")      ; not a kanban entry
   (edge "ghost" :depends-on "parent")       ; missing entry
   (edge "parent" :derived-from "upstream")  ; parent's own source: not a child
   (edge "upstream" :relates "parent")       ; unrelated relation
   (edge "parent" :relates "slice-b")])

(def ^:private stub-deps
  {:edges-from (fn [id] (filter #(= id (:kg-edge/from %)) edges))
   :edges-to   (fn [id] (filter #(= id (:kg-edge/to %)) edges))
   :get-entry  store})

(deftest open-children-on-stub-kg
  (let [kids (children/open-children "parent" stub-deps)
        ids  (mapv :id kids)]
    (testing "every open linked card, once each"
      (is (= #{"slice-a" "slice-b" "deriv" "contained" "planned"} (set ids)))
      (is (= (count ids) (count (distinct ids)))))
    (testing "done, non-kanban, missing and non-child links are skipped"
      (is (not-any? #{"slice-c" "note" "ghost" "upstream"} ids)))
    (testing "each row says how it is linked"
      (is (= {:id "slice-a" :title "card slice-a" :status "todo" :priority "medium"
              :relation "depends-on" :direction "incoming"}
             (first (filter #(= "slice-a" (:id %)) kids))))
      (is (= ["contains" "outgoing"]
             ((juxt :relation :direction) (first (filter #(= "contained" (:id %)) kids))))))))

(deftest no-children-is-empty
  (is (= [] (children/open-children "upstream-less" stub-deps))))

(deftest with-open-children-only-on-done
  (let [slim {:id "parent" :status "done"}]
    (testing "a move to done attaches open children and a hint"
      (let [r (mk/with-open-children slim "parent" "done" stub-deps)]
        (is (= 5 (count (:open_children r))))
        (is (re-find #"5 linked card" (:hint r)))))
    (testing "any other status leaves the view alone and never reads the KG"
      (let [boom {:edges-from (fn [_] (throw (ex-info "read" {})))
                  :edges-to   (fn [_] (throw (ex-info "read" {})))
                  :get-entry  (fn [_] (throw (ex-info "read" {})))}]
        (doseq [s ["todo" "inprogress" "inreview" nil]]
          (is (= slim (mk/with-open-children slim "parent" s boom))))))
    (testing "no open children: the view is unchanged"
      (is (= slim (mk/with-open-children slim "slice-a" "done" stub-deps))))
    (testing "a failing lookup is non-fatal"
      (is (= slim (mk/with-open-children
                   slim "parent" "done"
                   (assoc stub-deps :edges-to (fn [_] (throw (ex-info "kg down" {}))))))))))

(deftest read-only
  (let [calls (atom [])
        spy   (fn [k f] (fn [& args] (swap! calls conj k) (apply f args)))
        deps  {:edges-from (spy :edges-from (:edges-from stub-deps))
               :edges-to   (spy :edges-to (:edges-to stub-deps))
               :get-entry  (spy :get-entry store)}]
    (mk/with-open-children {:id "parent"} "parent" "done" deps)
    (is (every? #{:edges-from :edges-to :get-entry} @calls)
        "only the injected reads are used")))
