(ns hive-mcp.tools.kanban.update-atomicity-test
  "kanban update never drops a field: concurrent verbs on one id keep every
   field, a done card takes a description edit, and a write the store refuses
   fails loudly. Friction 20261017193106-00e06d69."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.tools.kanban.events :as events]
            [hive-mcp.tools.memory-kanban :as mem-kanban]
            [hive-mcp.vectordb.kanban-facade :as facade]))

(defn- card [id status]
  {:id id
   :content {:task-type "kanban" :title "card" :status status
             :priority "medium" :description "original"}
   :tags ["kanban" status "priority-medium" "scope:project:hive"]})

(defn- slow-store
  "An in-memory IMemoryStore whose reads take `read-ms`: wide enough that two
   unserialized read-modify-writes on one id overlap. `update-answer`, when
   given, replaces the store's answer to update-entry! (a refusal)."
  [entries read-ms & [update-answer]]
  (reify proto/IMemoryStore
    (connect! [_ _] true) (disconnect! [_] nil) (connected? [_] true)
    (health-check [_] {:healthy? true}) (add-entry! [_ entry] (:id entry))
    (get-entry [_ id] (Thread/sleep (long read-ms)) (get @entries id))
    (update-entry! [_ id updates]
      (if update-answer
        update-answer
        (when (contains? @entries id)
          (swap! entries update id merge updates)
          id)))
    (delete-entry! [_ id] (swap! entries dissoc id) true)
    (query-entries [_ _] [])
    (search-similar [_ _ _] []) (supports-semantic-search? [_] false)
    (cleanup-expired! [_] nil) (entries-expiring-soon [_ _ _] [])
    (find-duplicate [_ _ _ _] nil) (store-status [_] {}) (reset-store! [_] nil)))

(defn- with-store* [store f]
  (let [previous (proto/registered-stores)]
    (try
      (proto/register-store! :default store)
      (f)
      (finally
        (proto/unregister-store! :default)
        (when-let [old (get previous :default)] (proto/register-store! :default old))))))

(deftest parallel-move-and-edit-keep-both-fields
  (let [id      "fx3-atomic-parallel"
        entries (atom {id (card id "todo")})]
    (with-store* (slow-store entries 60)
      (fn []
        (let [move (future (events/dispatch-move! {:task-id id :new-status "inreview"}))
              edit (future (events/dispatch-edit! {:task-id id :description "the result block"}))]
          (is (r/ok? @move))
          (is (r/ok? @edit))
          (let [stored (:content (get @entries id))]
            (is (= "review" (:status stored)) "the move survived the edit")
            (is (= "the result block" (:description stored)) "the edit survived the move")))))))

(deftest three-parallel-status-plus-description-updates-keep-every-description
  (let [ids     ["fx3-par-a" "fx3-par-b" "fx3-par-c"]
        entries (atom (into {} (map (fn [id] [id (card id "todo")])) ids))]
    (with-store* (slow-store entries 40)
      (fn []
        (let [calls (mapv (fn [id]
                            (future (mem-kanban/handle-mem-kanban-move
                                     {:task_id id :new_status "inreview"
                                      :description (str "desc " id)})))
                          ids)]
          (doseq [c calls] (is (not (:isError @c)) (pr-str @c)))
          (doseq [id ids]
            (is (= (str "desc " id) (get-in @entries [id :content :description])))
            (is (= "review" (get-in @entries [id :content :status])))))))))

(deftest done-card-accepts-a-description-edit
  (let [id      "fx3-done-edit"
        entries (atom {id (card id "done")})]
    (with-store* (slow-store entries 0)
      (fn []
        (let [res (mem-kanban/handle-mem-kanban-move {:task_id id :description "late result"})]
          (is (not (:isError res)) (pr-str res))
          (is (= "late result" (get-in @entries [id :content :description])))
          (is (= "done" (get-in @entries [id :content :status]))))
        (testing "status + description on a card already in done"
          (let [res (mem-kanban/handle-mem-kanban-move {:task_id id :new_status "done"
                                                        :description "second result"})]
            (is (not (:isError res)) (pr-str res))
            (is (= "second result" (get-in @entries [id :content :description])))))))))

(deftest refused-write-fails-loudly
  (let [id      "fx3-refused"
        entries (atom {id (card id "done")})
        refusal {:success? false :error :qdrant/embed-failed :message "embedder returned no vector"}]
    (with-store* (slow-store entries 0 refusal)
      (fn []
        (let [res (events/dispatch-edit! {:task-id id :description "lost?"})]
          (is (r/err? res))
          (is (= :kanban/write-rejected (:error res)))
          (is (str/includes? (str (:message res)) "embedder returned no vector")))
        (let [res (mem-kanban/handle-mem-kanban-move {:task_id id :description "lost?"})]
          (is (:isError res) "the MCP boundary reports the refusal, not a success row"))
        (is (= "original" (get-in (facade/get-entry-by-id id) [:content :description]))
            "a refused write is not served from the read-your-writes overlay")))))

(deftest write-rejected-vocabulary
  (is (facade/write-rejected? nil))
  (is (facade/write-rejected? {:success? false :error :x}))
  (is (not (facade/write-rejected? "id")))
  (is (not (facade/write-rejected? true)))
  (is (not (facade/write-rejected? {:op :update-entry! :id "x"}))))
