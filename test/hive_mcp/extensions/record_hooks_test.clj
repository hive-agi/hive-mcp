(ns hive-mcp.extensions.record-hooks-test
  "Record hooks: the registry, and each derived-record exit that applies
   them. Every exit test first shows what reaches the sink with no hook (the
   decrypted value, verbatim), then that a hook rewrites or drops it."
  {:clj-kondo/config '{:linters {:unresolved-symbol
                                 {:exclude [(hive-mcp.extensions.record-hooks-test/with-tree [report])]}}}}
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.extensions.record-hooks :as hooks]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.knowledge-graph.connection :as kg-conn]
            [hive-mcp.memory.temporal :as temporal]
            [hive-mcp.project.scope :as project-scope]
            [hive-mcp.project.tree :as tree]
            [hive-mcp.swarm.datascript.coordination.session-registry :as session-registry]
            [hive-mcp.tools.consolidated.project :as project-tool]
            [hive-mcp.tools.kanban.effects :as effects]
            [hive-mcp.tools.migration :as migration]
            [hive-mcp.vectordb.facade :as facade]))

(def ^:private points
  [:temporal/mutation :kanban/history :archive/record :export/entry :project/visible])

(defn- clean-hooks! []
  (doseq [p points id (hooks/registered p)] (hooks/unregister! p id)))

(defn- clean [t]
  (clean-hooks!)
  (try (t) (finally (clean-hooks!))))

(use-fixtures :each clean)

(def ^:private secret "the enclave's plaintext")

;; =============================================================================
;; Registry
;; =============================================================================

(deftest no-hook-passes-the-record-untouched
  (is (= {:a 1} (hooks/apply-hooks :export/entry {:a 1}))))

(deftest hooks-run-in-id-order-per-point
  (hooks/register! :export/entry :b/second #(update % :trail conj :second))
  (hooks/register! :export/entry :a/first #(update % :trail conj :first))
  (hooks/register! :archive/record :c/other #(assoc % :other true))
  (is (= [:a/first :b/second] (hooks/registered :export/entry)))
  (is (= {:trail [:first :second]} (hooks/apply-hooks :export/entry {:trail []})))
  (testing "a point runs only its own hooks"
    (is (= {:x 1 :other true} (hooks/apply-hooks :archive/record {:x 1})))))

(deftest nil-drops-and-stops-the-chain
  (let [ran (atom false)]
    (hooks/register! :export/entry :a/drop (constantly nil))
    (hooks/register! :export/entry :b/after (fn [r] (reset! ran true) r))
    (is (nil? (hooks/apply-hooks :export/entry {:a 1})))
    (is (false? @ran))))

(deftest a-throwing-hook-reaches-the-caller
  (hooks/register! :export/entry :x/boom (fn [_] (throw (ex-info "boom" {}))))
  (is (thrown? clojure.lang.ExceptionInfo (hooks/apply-hooks :export/entry {}))))

(defn tag-hook [r] (assoc r :tagged true))

(deftest a-var-is-a-hook-and-unregister-removes
  (hooks/register! :export/entry :x/var #'tag-hook)
  (is (= {:tagged true} (hooks/apply-hooks :export/entry {})))
  (hooks/unregister! :export/entry :x/var)
  (is (= {} (hooks/apply-hooks :export/entry {})))
  (is (= :x/none (hooks/unregister! :export/entry :x/none))))

(deftest an-unknown-point-is-refused
  (is (thrown? AssertionError (hooks/register! :no/such :x/h identity))))

;; =============================================================================
;; :temporal/mutation
;; =============================================================================

(defmacro ^:private with-trail
  "Run body with a temporal store whose transactions land in atom `txs`."
  [txs & body]
  `(with-redefs [kg-conn/temporal-store? (constantly true)
                 kg-conn/transact! (fn [tx#] (swap! ~txs into tx#) {:tx-data tx#})]
     ~@body))

(def ^:private edit {:entry-id "e1" :op :kanban-edit
                     :data {:title "new title"} :previous-value {:title secret}})

(deftest temporal-trail-copies-the-previous-value-verbatim
  (let [txs (atom [])]
    (with-trail txs (temporal/record-mutation! edit))
    (is (str/includes? (:mem-mutation/previous-value (first @txs)) secret)
        "without a hook the edited entry's old plaintext lands in the trail")))

(deftest temporal-hook-rewrites-the-mutation
  (let [txs (atom [])]
    (hooks/register! :temporal/mutation :t/seal
                     (fn [m] (-> m (assoc :data {:sealed "blob"}) (dissoc :previous-value))))
    (with-trail txs (is (:mutation-id (temporal/record-mutation! edit))))
    (let [row (first @txs)]
      (is (nil? (:mem-mutation/previous-value row)))
      (is (= {:sealed "blob"} (edn/read-string (:mem-mutation/data row))))
      (is (not (str/includes? (pr-str row) secret))))))

(deftest temporal-hook-drops-the-mutation
  (let [txs (atom [])]
    (hooks/register! :temporal/mutation :t/drop (constantly nil))
    (with-trail txs (is (= {:ok true :dropped true} (temporal/record-mutation! edit))))
    (is (empty? @txs))))

(deftest a-throwing-temporal-hook-writes-nothing
  (let [txs (atom [])]
    (hooks/register! :temporal/mutation :t/boom (fn [_] (throw (ex-info "locked" {}))))
    (with-trail txs (is (false? (:ok (temporal/record-mutation! edit)))))
    (is (empty? @txs))))

(deftest temporal-batch-applies-the-hooks
  (let [txs (atom [])]
    (hooks/register! :temporal/mutation :t/drop-e1 #(when-not (= "e1" (:entry-id %)) %))
    (with-trail txs
      (is (= 1 (:count (temporal/record-mutations-batch!
                        [edit {:entry-id "e2" :op :decay :data {:d 1}}])))))
    (is (= ["e2"] (map :mem-mutation/entry-id @txs)))
    (is (not (str/includes? (pr-str @txs) secret)))))

;; =============================================================================
;; :kanban/history
;; =============================================================================

(defmacro ^:private with-registry
  "Run body with the swarm session registry writing into atom `rows`."
  [rows & body]
  `(with-redefs [session-registry/impl! (fn [sym#] (fn [& args#] (swap! ~rows conj [sym# args#]) {}))]
     ~@body))

(deftest kanban-history-copies-the-title-verbatim
  (let [rows (atom [])]
    (with-registry rows
      (session-registry/register-kanban-movement! {:task-id "t1" :title secret :to "done"}))
    (is (str/includes? (pr-str @rows) secret))))

(deftest kanban-history-hook-rewrites-or-drops-the-title
  (let [rows (atom [])]
    (hooks/register! :kanban/history :k/hint #(when-not (= "t2" (:task-id %)) (assoc % :title "sealed task")))
    (with-registry rows
      (session-registry/register-kanban-movement! {:task-id "t1" :title secret :to "done"})
      (session-registry/register-kanban-movement! {:task-id "t2" :title secret :to "done"})
      (session-registry/register-completed-task! "t1" {:title secret :agent-id "a"}))
    (is (not (str/includes? (pr-str @rows) secret)))
    (testing "a dropped row is still written, without its title"
      (is (= {:task-id "t2" :to "done"} (first (second (second @rows))))))
    (testing "the completed-task row keeps its shape"
      (is (= ["t1" {:title "sealed task" :agent-id "a"}] (second (nth @rows 2)))))))

;; =============================================================================
;; :archive/record
;; =============================================================================

(def ^:private done-entry
  {:content {:title secret :description secret} :tags ["kanban" "scope:project:p1"]})

(defn- archive! [archived]
  (with-redefs [ext/get-extension (fn [k] (when (= k :da/archive!) #(swap! archived conj %)))]
    (#'effects/archive-external! {:entry done-entry :task-id "t1"})))

(deftest archive-copies-the-task-verbatim
  (let [archived (atom [])]
    (archive! archived)
    (is (= secret (:title (first @archived))))))

(deftest archive-hook-rewrites-or-drops
  (let [archived (atom [])]
    (hooks/register! :archive/record :a/hint #(assoc % :title "sealed" :description nil))
    (archive! archived)
    (is (= "sealed" (:title (first @archived))))
    (hooks/register! :archive/record :b/drop (constantly nil))
    (archive! archived)
    (is (= 1 (count @archived)) "a dropped payload is not archived")))

;; =============================================================================
;; :export/entry
;; =============================================================================

(def ^:private entries
  [{:id "m1" :type "note" :content secret} {:id "m2" :type "note" :content "public"}])

(defn- export []
  (with-redefs [facade/query-entries (fn [& _] entries)]
    (#'migration/export-memory-to-edn)))

(deftest export-copies-decrypted-content-verbatim
  (is (str/includes? (pr-str (export)) secret)))

(deftest export-hook-rewrites-or-drops-entries
  (hooks/register! :export/entry :e/seal #(if (= "m1" (:id %)) (assoc % :content "WORK-ENVELOPE") %))
  (let [out (export)]
    (is (not (str/includes? (pr-str out) secret)))
    (is (= 2 (:total out))))
  (hooks/register! :export/entry :f/drop #(when-not (= "m1" (:id %)) %))
  (let [out (export)]
    (is (= ["m2"] (map :id (:entries out))))
    (is (= {:note 1} (:counts out)))))

;; =============================================================================
;; :project/visible
;; =============================================================================

(defn- write-project! [dir cfg]
  (.mkdirs (io/file dir))
  (spit (io/file dir ".hive-project.edn") (pr-str cfg)))

(defn- enclaved? [entity]
  (contains? (edn/read-string (or (:project/config entity) "{}")) :seal/enclave))

(defn- hide-enclaved [entities] (filterv (complement enclaved?) entities))

(def ^:private ids ["rh-root" "rh-pub" "rh-sec"])

(defmacro ^:private with-tree
  "Scan a temp tree rh-root > {rh-pub, rh-sec (enclaved)}; bind `report`."
  [report & body]
  `(let [root# (str (System/getProperty "java.io.tmpdir") "/record-hooks-" (System/nanoTime))]
     (write-project! root# {:project-id "rh-root"})
     (write-project! (str root# "/pub") {:project-id "rh-pub" :parent-id "rh-root"})
     (write-project! (str root# "/sec") {:project-id "rh-sec" :parent-id "rh-root" :seal/enclave "e1"})
     (try
       (tree/reset-index!)
       (let [~report (tree/scan-project-tree! root# {:max-depth 3})]
         ~@body)
       (finally
         (tree/reset-index!)
         (doseq [id# ids] (project-scope/deregister-project-config! id#))
         (doseq [f# (reverse (file-seq (io/file root#)))] (io/delete-file f# true))))))

(deftest project-tree-shows-enclaved-projects-without-a-hook
  (with-tree report
    (is (= 3 (:projects-found report)))
    (is (= #{"rh-pub" "rh-sec"} (tree/get-descendant-ids "rh-root")))))

(deftest project-visible-hook-hides-enclaved-projects-at-every-exit
  (hooks/register! :project/visible :p/hide #'hide-enclaved)
  (with-tree report
    (testing "scan report"
      (is (= 2 (:projects-found report)))
      (is (not (str/includes? (pr-str report) "rh-sec"))))
    (testing "descendant ids, scope tags and scopes (catchup, piggyback, HCR)"
      (is (= #{"rh-pub"} (tree/get-descendant-ids "rh-root")))
      (is (= #{"scope:project:rh-pub"} (tree/get-descendant-scope-tags "rh-root")))
      (is (= ["rh-pub"] (project-scope/descendant-scopes "rh-root"))))
    (testing "children"
      (is (= ["rh-pub"] (mapv :project/id (tree/query-project-children "rh-root"))))
      (is (true? (tree/has-children? "rh-root"))))
    (testing "the project tree tool"
      (let [listing (pr-str (project-tool/handle-project-tree {}))
            one     (pr-str (project-tool/handle-project-tree {:project_id "rh-sec"}))]
        (is (str/includes? listing "rh-pub"))
        (is (not (str/includes? listing "rh-sec")))
        (is (not (str/includes? one "/sec")) "the hidden project's entity is not returned")))
    (testing "the index itself still holds it, for the host's own lookups"
      (is (some? (tree/query-project-by-id "rh-sec"))))))
