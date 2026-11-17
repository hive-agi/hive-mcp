(ns hive-mcp.tools.kanban.claim-test
  "Card claims (tool-friction 20261109113824-1b6c42b4): a move into
   inprogress records owner + since; a second owner is refused naming the
   holder unless it steals; a dead holder is taken over; todo/done clear."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.kanban.claim :as claim]
            [hive-mcp.tools.kanban.events :as events]
            [hive-mcp.tools.kanban.update :as update]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private tid "20261109000000-34932f81")

(defn- card
  ([status] (card status nil))
  ([status claim]
   {:id      tid
    :tags    ["kanban" status "priority-high" "scope:project:hive-mcp"]
    :content (cond-> {:task-type "kanban" :title "x" :status status :priority "high"}
               claim (assoc :claim claim))}))

(defn- move [entry status owner & {:keys [steal live? lineage?] :or {live? true lineage? false}}]
  (events/move-fx {:kanban/entry      entry
                   :kanban/project-id "hive-mcp"
                   :kanban/claim-ctx  {:holder-live? live? :same-lineage? lineage?}}
                  [:kanban/move {:task-id tid :new-status status
                                 :owner owner :steal-claim? steal}]))

(defn- written-claim [fx] (get-in fx [:kanban/facade-update :payload :content :claim]))

(deftest owner-key-folds-coordinator-project-suffix
  (is (= "coordinator:50571" (claim/owner-key "coordinator:50571-hive-mcp")))
  (is (= "coordinator:50571" (claim/owner-key "coordinator:50571")))
  (is (= "ling-a" (claim/owner-key "ling-a")))
  (is (nil? (claim/owner-key "  "))))

(deftest inprogress-records-owner-and-since
  (let [fx (move (card "todo") "doing" "coordinator:50571")]
    (is (= "coordinator:50571" (:owner (written-claim fx))))
    (is (string? (:since (written-claim fx))))
    (is (= "doing" (get-in fx [:kanban/facade-update :payload :content :status])))))

(deftest second-owner-is-refused-naming-holder-and-since
  (let [held {:owner "coordinator:50571" :since "2026-11-09T13:53:00-0300"}
        fx   (move (card "doing" held) "doing" "coordinator:68736")]
    (is (= #{:kanban/claim-refused} (set (keys fx))) "nothing is written")
    (is (= "coordinator:50571" (get-in fx [:kanban/claim-refused :holder])))
    (is (= "2026-11-09T13:53:00-0300" (get-in fx [:kanban/claim-refused :since])))
    (let [msg (claim/refusal-message tid (:kanban/claim-refused fx))]
      (is (re-find #"coordinator:50571" msg))
      (is (re-find #"2026-11-09T13:53:00" msg))
      (is (re-find #"steal_claim" msg)))))

(deftest steal-takes-the-card
  (let [held {:owner "coordinator:50571" :since "t0"}
        fx   (move (card "doing" held) "doing" "coordinator:68736" :steal true)]
    (is (= "coordinator:68736" (:owner (written-claim fx))))
    (is (not= "t0" (:since (written-claim fx))))))

(deftest dead-holder-is-reclaimed
  (let [fx (move (card "doing" {:owner "coordinator:1" :since "t0"}) "doing" "coordinator:2"
                 :live? false)]
    (is (= "coordinator:2" (:owner (written-claim fx))))))

(deftest holder-and-its-lineage-keep-the-claim
  (testing "the holder itself, under another lane spelling"
    (let [fx (move (card "todo" {:owner "coordinator:7" :since "t0"}) "doing" "coordinator:7-hive")]
      (is (= {:owner "coordinator:7" :since "t0"} (written-claim fx)))))
  (testing "a ling the holder spawned for the card"
    (let [fx (move (card "doing" {:owner "coordinator:7" :since "t0"}) "doing" "ling-x"
                   :lineage? true)]
      (is (= {:owner "coordinator:7" :since "t0"} (written-claim fx))))))

(deftest todo-and-done-clear-the-claim
  (doseq [s ["todo" "done"]]
    (let [fx (move (card "doing" {:owner "coordinator:7" :since "t0"}) s "coordinator:9")]
      (is (nil? (written-claim fx)) (str s " clears the claim"))
      (is (contains? fx :kanban/facade-update)))))

(deftest review-keeps-the-claim-and-is-never-refused
  (let [fx (move (card "doing" {:owner "coordinator:7" :since "t0"}) "review" "coordinator:9")]
    (is (= {:owner "coordinator:7" :since "t0"} (written-claim fx)))))

(deftest no-owner-neither-records-nor-refuses
  (let [fx (move (card "doing" {:owner "coordinator:7" :since "t0"}) "doing" nil)]
    (is (contains? fx :kanban/facade-update))
    (is (= {:owner "coordinator:7" :since "t0"} (written-claim fx)))))

(deftest claim-event-without-move
  (let [fx (events/claim-fx {:kanban/entry (card "todo") :kanban/claim-ctx {}}
                            [:kanban/claim {:task-id tid :owner "coordinator:7"}])]
    (is (= "coordinator:7" (:owner (written-claim fx))))
    (is (= "todo" (get-in fx [:kanban/facade-update :payload :content :status]))))
  (let [fx (events/claim-fx {:kanban/entry (card "todo" {:owner "coordinator:7" :since "t0"})
                             :kanban/claim-ctx {:holder-live? true :same-lineage? false}}
                            [:kanban/claim {:task-id tid :owner "coordinator:9"}])]
    (is (= "coordinator:7" (get-in fx [:kanban/claim-refused :holder]))))
  (testing "a done card is never claimed"
    (let [fx (events/claim-fx {:kanban/entry (card "done") :kanban/claim-ctx {}}
                              [:kanban/claim {:task-id tid :owner "coordinator:7"}])]
      (is (nil? (written-claim fx))))))

(deftest move-plus-edit-honours-a-refusal
  (let [fx (update/update-fx {:kanban/entry      (card "doing" {:owner "coordinator:7" :since "t0"})
                              :kanban/project-id "hive-mcp"
                              :kanban/claim-ctx  {:holder-live? true :same-lineage? false}}
                             [:kanban/update {:task-id tid :new-status "doing" :title "y"
                                              :owner "coordinator:9"}])]
    (is (= #{:kanban/claim-refused} (set (keys fx))) "neither half is written")))

(deftest holder-live-reads-coordinator-pids
  (with-redefs [claim/same-lineage? (constantly false)]
    (let [self (str (.pid (java.lang.ProcessHandle/current)))]
      (is (true? (claim/holder-live? (str "coordinator:" self))))
      (is (false? (claim/holder-live? "coordinator:999999999")))
      (is (true? (claim/holder-live? "some-unregistered-agent"))
          "an owner that cannot be checked counts as live"))))
