(ns hive-mcp.tools.kanban.update-roundtrip-test
  "kanban update, read back through `kanban get`: every field one update
   names is on the card afterwards, and an update the store refuses leaves
   the card as it was. Kanban 20261020212530-132e3ea7, frictions
   20261020204949-2ec7c321 and 20261020212506-6669102d.

   The store is hive-spi's atom-backed stub, registered through the memory
   port. A refusing decorator around it stands in for a store that answers
   a write with a hive-spi failure map (hive-qdrant's embed refusal)."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.tools.memory-kanban :as mem-kanban]
            [hive-spi.memory.stub :as stub]))

;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- card [id]
  {:id      id
   :type    "note"
   :content {:task-type "kanban" :title "card v0" :status "todo"
             :priority "medium" :description "desc v0"}
   :tags    ["kanban" "todo" "priority-medium" "scope:project:hive-mcp"]})

(defn- counting-store
  "`inner` behind the memory port, counting the update-entry! calls that
   reached it in `commits`. `refuse?`, given the updates, answers a hive-spi
   failure map instead of writing."
  [inner commits refuse?]
  (reify proto/IMemoryStore
    (connect! [_ c] (proto/connect! inner c))
    (disconnect! [_] (proto/disconnect! inner))
    (connected? [_] true)
    (health-check [_] {:healthy? true})
    (add-entry! [_ e] (proto/add-entry! inner e))
    (get-entry [_ id] (proto/get-entry inner id))
    (update-entry! [_ id updates]
      (if (refuse? updates)
        {:success? false :error :qdrant/embed-failed :message "embedder returned no vector"}
        (do (swap! commits inc)
            (proto/update-entry! inner id updates))))
    (delete-entry! [_ id] (proto/delete-entry! inner id))
    (query-entries [_ opts] (proto/query-entries inner opts))
    (search-similar [_ q opts] (proto/search-similar inner q opts))
    (supports-semantic-search? [_] false)
    (cleanup-expired! [_] (proto/cleanup-expired! inner))
    (entries-expiring-soon [_ d o] (proto/entries-expiring-soon inner d o))
    (find-duplicate [_ t h o] (proto/find-duplicate inner t h o))
    (store-status [_] (proto/store-status inner))
    (reset-store! [_] (proto/reset-store! inner))))

(defn- with-board
  "Run (f commits) with a stub holding `card` for `id` registered under both
   slots the kanban facade reads, so the test holds whatever kanban-store
   mode the config names. `commits` counts the writes that reached the stub."
  ([id f] (with-board id (constantly false) f))
  ([id refuse? f]
   (let [inner    (stub/create-store)
         _        (proto/add-entry! inner (card id))
         commits  (atom 0)
         store    (counting-store inner commits refuse?)
         previous (proto/registered-stores)]
     (try
       (proto/register-store! :default store)
       (proto/register-store! :kanban store)
       (f commits)
       (finally
         (doseq [slot [:default :kanban]]
           (proto/unregister-store! slot)
           (when-let [old (get previous slot)] (proto/register-store! slot old))))))))

(defn- update! [params] (mem-kanban/handle-mem-kanban-move params))

(defn- get-card
  "The card as `kanban get` answers it."
  [id]
  (let [res (mem-kanban/handle-mem-kanban-get {:id id})]
    (is (not (:isError res)) (pr-str res))
    (json/read-str (:text res) :key-fn keyword)))

(deftest description-and-status-in-one-update-both-land
  (let [id "rt-desc-status"]
    (with-board id
      (fn [commits]
        (let [res (update! {:task_id id :description "desc v1" :new_status "inreview"})]
          (is (not (:isError res)) (pr-str res))
          (is (= "review" (:status (json/read-str (:text res) :key-fn keyword)))
              "the response reports the new status"))
        (let [got (get-card id)]
          (is (= "desc v1" (:description got)))
          (is (= "review" (:status got)))
          (is (some #{"review"} (:tags got)) "the status tag moved with the content"))
        (is (= 1 @commits) "one update is one commit")))))

(deftest description-only-update-lands
  (let [id "rt-desc-only"]
    (with-board id
      (fn [_]
        (is (not (:isError (update! {:task_id id :description "desc v1"}))))
        (let [got (get-card id)]
          (is (= "desc v1" (:description got)))
          (is (= "todo" (:status got))))))))

(deftest title-only-update-lands
  (let [id "rt-title-only"]
    (with-board id
      (fn [_]
        (is (not (:isError (update! {:task_id id :title "card v1"}))))
        (let [got (get-card id)]
          (is (= "card v1" (:title got)))
          (is (= "desc v0" (:description got)))
          (is (= "todo" (:status got))))))))

(deftest status-only-update-lands
  (let [id "rt-status-only"]
    (with-board id
      (fn [_]
        (is (not (:isError (update! {:task_id id :new_status "done"}))))
        (let [got (get-card id)]
          (is (= "done" (:status got)))
          (is (= "desc v0" (:description got))))))))

(deftest every-field-in-one-update-lands
  (let [id "rt-all-fields"]
    (with-board id
      (fn [commits]
        (is (not (:isError (update! {:task_id id :new_status "inprogress" :title "card v1"
                                     :description "desc v1" :priority "high"}))))
        (let [got (get-card id)]
          (is (= ["doing" "card v1" "desc v1" "high"]
                 (mapv got [:status :title :description :priority])))
          (is (some #{"priority-high"} (:tags got))))
        (is (= 1 @commits))))))

(deftest a-refused-update-changes-nothing
  (testing "the store refuses the description: the status must not land alone"
    (let [id "rt-refused"]
      (with-board id
        (fn [updates] (= "refused desc" (get-in updates [:content :description])))
        (fn [_]
          (let [res (update! {:task_id id :description "refused desc" :new_status "done"})]
            (is (:isError res) "the boundary reports the refusal"))
          (let [got (get-card id)]
            (is (= "todo" (:status got)) "no half of a refused update lands")
            (is (= "desc v0" (:description got)))
            (is (some #{"todo"} (:tags got)))))))))
