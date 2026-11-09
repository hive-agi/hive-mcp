(ns hive-mcp.tools.memory.crud.batch-get-kanban-hint-test
  "batch-get names `project kanban get` for an id the kanban store holds."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [hive-mcp.tools.memory.crud.retrieve :as retrieve]
            [hive-mcp.protocols.memory :as mem-proto]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord MapStore [entries]
  mem-proto/IMemoryStore
  (connect! [_ _] nil)
  (disconnect! [_] nil)
  (connected? [_] true)
  (health-check [_] {:healthy? true})
  (add-entry! [_ _] nil)
  (get-entry [_ id] (get entries id))
  (update-entry! [_ _ _] nil)
  (delete-entry! [_ _] nil)
  (query-entries [_ _] [])
  (search-similar [_ _ _] [])
  (supports-semantic-search? [_] false)
  (cleanup-expired! [_] nil)
  (entries-expiring-soon [_ _ _] nil)
  (find-duplicate [_ _ _ _] nil)
  (store-status [_] {:configured? true})
  (reset-store! [_] nil))

(defn- with-slots
  "Register DEFAULT and KANBAN stores for the duration of F, then restore."
  [default kanban f]
  (let [prior (mem-proto/registered-stores)]
    (try
      (mem-proto/register-store! :default default)
      (mem-proto/register-store! :kanban kanban)
      (f)
      (finally
        (doseq [slot [:default :kanban]]
          (if-let [p (get prior slot)]
            (mem-proto/register-store! slot p)
            (mem-proto/unregister-store! slot)))))))

(deftest batch-get-reports-kanban-ids
  (let [card {:id "card-1" :type "kanban" :content "a card" :tags ["kanban"]}
        answer (with-slots (->MapStore {}) (->MapStore {"card-1" card})
                 #(json/read-str (:text (retrieve/handle-batch-get {:ids ["card-1" "ghost"]}))
                                 :key-fn keyword))]
    (testing "an id held only by the kanban store is named as such"
      (is (= ["card-1"] (:in_kanban answer)))
      (is (re-find #"project kanban get" (str (:kanban_hint answer)))))
    (testing "an id held nowhere stays plainly missing"
      (is (= ["ghost"] (:missing answer))))
    (is (= 0 (:count answer)))))

(deftest missing-payload-is-pure
  (is (= {} (retrieve/missing-payload [] [])))
  (is (= {:missing ["a"]} (retrieve/missing-payload ["a"] [])))
  (is (= ["b"] (:in_kanban (retrieve/missing-payload ["a" "b"] ["b"])))))
