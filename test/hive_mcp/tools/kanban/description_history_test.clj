(ns hive-mcp.tools.kanban.description-history-test
  "Regression for tool-friction 20261109131421-47d2a184: `kanban update
   description=` replaced the text with no copy kept, and a coordinator lost
   five card descriptions that way. A replaced description now lands in
   `:description-history`; `kanban history` lists it and `kanban restore`
   brings it back."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.test.stub.memory-store :as ms]
            [hive-mcp.tools.kanban.events :as events]
            [hive-mcp.tools.kanban.transitions :as kt]
            [hive-mcp.tools.memory-kanban :as mk]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private entry
  {:id      "t1"
   :type    "note"
   :tags    ["kanban" "todo" "priority-medium" "scope:project:p"]
   :content {:task-type "kanban" :title "T" :description "original" :status "todo" :priority "medium"}})

(defn- content-of [e edits]
  (:new-content (kt/edit-transition e edits)))

(deftest replace-keeps-the-old-description
  (let [c (content-of entry {:description "new" :at "2026-11-10T00:00:00+0000" :agent-id "coord"})]
    (is (= "new" (:description c)))
    (is (= [{:description "original" :replaced-at "2026-11-10T00:00:00+0000" :agent "coord"}]
           (:description-history c)))))

(deftest a-second-replace-stacks-oldest-first
  (let [c1 (content-of entry {:description "v2" :at "a"})
        c2 (content-of (assoc entry :content c1) {:description "v3" :at "b"})]
    (is (= "v3" (:description c2)))
    (is (= ["original" "v2"] (mapv :description (:description-history c2))))
    (is (not-any? #(contains? % :agent) (:description-history c2))
        "no agent key when no agent was known")))

(deftest append-and-no-ops-record-nothing
  (testing "append keeps the old text inside the new one"
    (let [c (content-of entry {:append-description "note"})]
      (is (= "original\n\nnote" (:description c)))
      (is (not (contains? c :description-history)))))
  (testing "replacing with the same text"
    (is (not (contains? (content-of entry {:description "original"}) :description-history))))
  (testing "a title edit"
    (is (not (contains? (content-of entry {:title "x"}) :description-history))))
  (testing "replacing a blank description"
    (is (not (contains? (content-of (assoc-in entry [:content :description] nil) {:description "x"})
                        :description-history))))
  (testing "replace + append still saves the replaced text"
    (let [c (content-of entry {:description "new" :append-description "note"})]
      (is (= "new\n\nnote" (:description c)))
      (is (= ["original"] (mapv :description (:description-history c)))))))

(deftest history-is-bounded
  (let [final (reduce (fn [e i]
                        (assoc e :content (content-of e {:description (str "v" i) :at (str i)})))
                      entry
                      (range 1 (+ 5 kt/description-history-limit)))
        h     (get-in final [:content :description-history])]
    (is (= kt/description-history-limit (count h)))
    (is (= (str "v" (+ 3 kt/description-history-limit)) (:description (last h)))
        "the newest replaced text is kept, the oldest dropped")))

(deftest json-roundtripped-history-is-read
  (let [c1 (content-of entry {:description "v2" :at "a"})
        rt (json/read-str (json/write-str c1))
        c2 (content-of (assoc entry :content rt) {:description "v3" :at "b"})]
    (is (= ["original" "v2"] (mapv :description (:description-history c2))))
    (is (not (contains? c2 "description-history")) "no stale string-keyed twin survives")))

(deftest edit-fx-threads-the-agent
  (let [fx (events/edit-fx {:kanban/entry entry}
                           [:kanban/edit {:task-id "t1" :description "new" :agent-id "coord"}])]
    (is (= [{:description "original" :agent "coord"}]
           (mapv #(select-keys % [:description :agent])
                 (get-in fx [:kanban/facade-update :payload :content :description-history]))))))

(deftest history-view-and-revision-lookup
  (let [e (assoc entry :content (content-of (assoc entry :content (content-of entry {:description "v2" :at "a"}))
                                            {:description "v3" :at "b"}))]
    (is (= {:id "t1" :title "T" :description "v3"
            :revisions [{:rev 1 :description "original" :replaced-at "a"}
                        {:rev 2 :description "v2" :replaced-at "b"}]}
           (mk/history-view "t1" e)))
    (is (= {:ok "original"} (mk/revision-description e 1)))
    (is (= {:ok "v2"} (mk/revision-description e "2")))
    (is (:error (mk/revision-description e 3)))
    (is (:error (mk/revision-description e 0)))
    (is (:error (mk/revision-description e nil)))
    (is (:error (mk/revision-description e "x")))))

;; =============================================================================
;; Through the handlers, on the stub memory store
;; =============================================================================

(use-fixtures :each (fn [f] (ms/with-stub-store (fn [] (ms/install! (ms/->stub [entry])) (f)))))

(defn- body [resp] (json/read-str (:text resp) :key-fn keyword))

(deftest update-history-restore-roundtrip
  (is (not (:isError (mk/handle-mem-kanban-move {:task_id "t1" :description "overwritten"
                                                 :agent_id "coord"}))))
  (let [h (body (mk/handle-mem-kanban-history {:task_id "t1"}))]
    (is (= "overwritten" (:description h)))
    (is (= [{:rev 1 :description "original" :agent "coord"}]
           (mapv #(select-keys % [:rev :description :agent]) (:revisions h)))))
  (let [g (body (mk/handle-mem-kanban-get {:task_id "t1"}))]
    (is (= 1 (:description_revisions g)) "get shows that history exists"))
  (is (not (:isError (mk/handle-mem-kanban-restore {:task_id "t1" :rev 1}))))
  (let [h (body (mk/handle-mem-kanban-history {:task_id "t1"}))]
    (is (= "original" (:description h)) "restore brought the text back")
    (is (= ["original" "overwritten"] (mapv :description (:revisions h)))
        "and the restore is itself undoable"))
  (is (:isError (mk/handle-mem-kanban-restore {:task_id "t1" :rev 9})))
  (is (:isError (mk/handle-mem-kanban-history {:task_id "nope"}))))
