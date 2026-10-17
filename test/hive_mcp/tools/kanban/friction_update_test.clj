(ns hive-mcp.tools.kanban.friction-update-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.tools.kanban.transitions :as kt]
            [hive-mcp.tools.kanban.events :as events]
            [hive-mcp.tools.consolidated.kanban :as kanban]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.vectordb.kanban-facade :as facade]))

(defn- lagging-store [snapshot writes]
  (reify proto/IMemoryStore
    (connect! [_ _] true) (disconnect! [_] nil) (connected? [_] true)
    (health-check [_] {:healthy? true}) (add-entry! [_ entry] (:id entry))
    (get-entry [_ id] (when (= id (:id snapshot)) snapshot))
    (update-entry! [_ id updates] (swap! writes conj [id updates]) true)
    (delete-entry! [_ _] true) (query-entries [_ _] [])
    (search-similar [_ _ _] []) (supports-semantic-search? [_] false)
    (cleanup-expired! [_] nil) (entries-expiring-soon [_ _ _] [])
    (find-duplicate [_ _ _ _] nil) (store-status [_] {}) (reset-store! [_] nil)))

(deftest append-description-preserves-history
  (let [entry {:content {:title "task" :status "done" :description "history"}
               :tags ["kanban" "done" "priority-low" "scope:project:hive"]}
        updated (kt/edit-transition entry {:append-description "decision"})]
    (is (= "history\n\ndecision" (get-in updated [:new-content :description])))
    (is (= "done" (get-in updated [:new-content :status])))
    (is (= #{:description} (:changed updated)))
    (is (= "replacement\n\ndecision"
           (get-in (kt/edit-transition entry {:description "replacement"
                                               :append-description "decision"})
                   [:new-content :description])))))

(deftest edit-fx-forwards-append
  (let [entry {:id "fx-edit" :content {:task-type "kanban" :title "t" :status "done"
                                       :description "history" :priority "low"}
               :tags ["kanban" "done" "priority-low" "scope:project:hive"]}
        effect (events/edit-fx {:kanban/entry entry}
                               [:kanban/edit {:task-id "fx-edit" :append-description "decision"}])]
    (is (= "history\n\ndecision"
           (get-in effect [:kanban/facade-update :payload :content :description])))))

(deftest write-overlay-preserves-sequential-moves-and-tags
  (let [id "fx-overlay-card"
        snapshot {:id id :content {:task-type "kanban" :title "task" :status "todo"
                                    :description "history" :priority "low"}
                  :tags ["kanban" "todo" "priority-low" "scope:project:hive"]}
        writes (atom []) previous (proto/registered-stores)]
    (try
      (proto/register-store! :default (lagging-store snapshot writes))
      (facade/update-entry! id {:content (assoc (:content snapshot) :status "done")
                                :tags ["kanban" "done" "priority-low" "scope:project:hive"]})
      (let [after-move (facade/get-entry-by-id id)]
        (is (= "done" (get-in after-move [:content :status])))
        (facade/update-entry! id {:tags (conj (:tags after-move) "new-tag")})
        (let [after-retag (facade/get-entry-by-id id)]
          (is (= "done" (get-in after-retag [:content :status])))
          (is (= ["kanban" "done" "priority-low" "scope:project:hive" "new-tag"]
                 (:tags after-retag)))
          (facade/update-entry! id {:content (:new-content (kt/edit-transition after-retag
                                                                          {:append-description "decision"}))})
          (is (= "history\n\ndecision"
                 (get-in (facade/get-entry-by-id id) [:content :description])))))
      (is (= 3 (count @writes)))
      (facade/delete-entry! id)
      (is (= "todo" (get-in (facade/get-entry-by-id id) [:content :status])))
      (finally
        (proto/unregister-store! :default)
        (when-let [old (get previous :default)] (proto/register-store! :default old))))))

(deftest update-operations-route-to-batch
  (let [result (kanban/handle-kanban {:command "update" :operations [{:task_id "missing" :new_status "done"}]})]
    (is (not= "Invalid status: ." (:text result)))
    (is (some? result))))

(defn run-append-description [{:keys [current addition]}]
  (kt/append-description current addition))

(deftrifecta append-description-contract
  hive-mcp.tools.kanban.friction-update-test/run-append-description
  {:golden-path "test/golden/kanban/append-description.edn"
   :cases       {:appends-after-blank-line {:current "history" :addition "decision"}
                 :blank-addition-keeps     {:current "history" :addition "   "}
                 :nil-addition-keeps       {:current "history" :addition nil}
                 :blank-current-yields-add {:current "" :addition "decision"}
                 :nil-current-yields-add   {:current nil :addition "decision"}}
   :gen         (gen/let [cur (gen/one-of [(gen/return nil) gen/string-alphanumeric])
                          add (gen/one-of [(gen/return nil) gen/string-alphanumeric])]
                  {:current cur :addition add})
   :pred        (fn [r] (or (nil? r) (string? r)))
   :num-tests   100
   :mutations   [["replaces" (fn [{:keys [addition]}] addition)]
                 ["drops-addition" (fn [{:keys [current]}] current)]
                 ["no-separator" (fn [{:keys [current addition]}] (str current addition))]]})

(deftest append-description-never-loses-history
  (doseq [cur ["history" "a\nb"] add ["x" "decision\n\nmore"]]
    (let [r (kt/append-description cur add)]
      (is (str/starts-with? r cur))
      (is (str/ends-with? r add)))))

(deftest rejected-write-is-not-overlaid
  (let [id "fx-overlay-absent"
        snapshot {:id id :content {:task-type "kanban" :title "task" :status "todo"}
                  :tags ["kanban" "todo"]}
        previous (proto/registered-stores)
        store (reify proto/IMemoryStore
                (connect! [_ _] true) (disconnect! [_] nil) (connected? [_] true)
                (health-check [_] {:healthy? true}) (add-entry! [_ entry] (:id entry))
                (get-entry [_ eid] (when (= eid id) snapshot))
                (update-entry! [_ _ _] nil)
                (delete-entry! [_ _] true) (query-entries [_ _] [])
                (search-similar [_ _ _] []) (supports-semantic-search? [_] false)
                (cleanup-expired! [_] nil) (entries-expiring-soon [_ _ _] [])
                (find-duplicate [_ _ _ _] nil) (store-status [_] {}) (reset-store! [_] nil))]
    (try
      (proto/register-store! :default store)
      (is (nil? (facade/update-entry! id {:content (assoc (:content snapshot) :status "done")})))
      (is (= "todo" (get-in (facade/get-entry-by-id id) [:content :status]))
          "a write the store refused is not served from the overlay")
      (finally
        (proto/unregister-store! :default)
        (when-let [old (get previous :default)] (proto/register-store! :default old))))))
