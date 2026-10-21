(ns hive-mcp.vectordb.contract-trifecta-test
  "Contracts of src/hive_mcp/vectordb (batch m30): the coverage gate, the
   schemas generating, and trifectas driven through a stub IMemoryStore
   registered on the port (no backend, no with-redefs: vectordb stays
   backend-free and so do its tests)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.vectordb.facade :as facade]
            [hive-mcp.vectordb.kanban-facade :as kf]
            [hive-mcp.vectordb.resilience]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; Gate
;;; ---------------------------------------------------------------------------

;; resilience.clj's two macros are not defns, so the gate does not count them.
(coverage/deftest-contract-coverage vectordb-contracts-gate
  {:paths ["src/hive_mcp/vectordb"]
   :exempt {}})

(deftest vectordb-gate-is-not-vacuous
  (let [r (coverage/coverage {:paths ["src/hive_mcp/vectordb"]})]
    (is (= 32 (count (:universe r))))
    (is (= 32 (count (:covered r))))))

(def ^:private vectordb-nses
  '[hive-mcp.vectordb.facade hive-mcp.vectordb.kanban-facade hive-mcp.vectordb.resilience])

(deftest every-vectordb-contract-generates-both-sides
  (doseq [ns-sym vectordb-nses
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (let [sch (m/schema schema)]
                  (if (= :function (m/type sch)) (m/children sch) [sch]))
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

(deftest resilience-names-carry-the-kernel-contracts
  (doseq [f '[kick-and-wait! transient-failure? call-with-resilience call-with-resilience-result]]
    (is (= (m/form (get-in (m/function-schemas) ['hive-mcp.resilience.store f :schema]))
           (m/form (get-in (m/function-schemas) ['hive-mcp.vectordb.resilience f :schema])))
        (str f))))

;;; ---------------------------------------------------------------------------
;;; A stub store on the port
;;; ---------------------------------------------------------------------------

(defn- stub-store
  "An IMemoryStore over an atom of id -> entry. `add-answer` decides what an
   add answers (an id, a failure map, nil) so each facade arm is reachable."
  [db add-answer]
  (reify proto/IMemoryStore
    (connect!       [_ _] {:success? true})
    (disconnect!    [_] nil)
    (connected?     [_] true)
    (health-check   [_] {:healthy? true})
    (add-entry!     [_ e] (let [a (add-answer e)]
                            (when (string? a) (swap! db assoc a (assoc e :id a)))
                            a))
    (get-entry      [_ id] (get @db id))
    (update-entry!  [_ id u] (when (contains? @db id) (swap! db update id merge u) (get @db id)))
    (delete-entry!  [_ id] (if (contains? @db id) (do (swap! db dissoc id) true) nil))
    (query-entries  [_ {:keys [type limit]}]
      (cond->> (sort-by :id (vals @db))
        type  (filter #(= type (:type %)))
        limit (take limit)
        true  vec))
    (search-similar [_ _ {:keys [limit]}] (vec (take (or limit 10) (sort-by :id (vals @db)))))
    (supports-semantic-search? [_] false)
    (cleanup-expired! [_] 0)
    (entries-expiring-soon [_ _ _] [])
    (find-duplicate [_ type h _] (some #(when (and (= type (:type %)) (= h (:content-hash %))) %) (vals @db)))
    (store-status   [_] {})
    (reset-store!   [_] (reset! db {}))))

(def ^:private db (atom {}))

(defn- answer-by-id
  "Adds answer the entry's own :id, a failure map for id \"fail\", nil for
   id \"nil\" (unpersisted)."
  [e]
  (case (:id e) "fail" {:success? false :error "down"} "nil" nil (:id e)))

(use-fixtures :each
  (fn [f]
    (let [snapshot (proto/registered-stores)]
      (reset! db {})
      (proto/register-store! :default (stub-store db answer-by-id))
      (proto/unregister-store! :kanban)
      (try (f)
           (finally
             (proto/unregister-store! :default)
             (proto/unregister-store! :kanban)
             (doseq [[k s] snapshot] (proto/register-store! k s)))))))

;;; ---------------------------------------------------------------------------
;;; facade
;;; ---------------------------------------------------------------------------

(def ^:private EntryGen
  [:map [:id [:enum "e1" "e2" "e3" "fail" "nil"]] [:type [:enum "note" "decision"]]
   [:tags [:vector {:max 2} [:enum "a" "b"]]]])

(defn index-one
  "Unary adapter: index ENTRY through the facade into a fresh stub store;
   a refused write is answered as its ex-data :error instead of a throw."
  [entry]
  (reset! db {})
  (try (facade/index-memory-entry! entry)
       (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))

(hst/deftrifecta-from-schema index-memory-entry-contract
  hive-mcp.vectordb.contract-trifecta-test/index-one
  {:in          EntryGen
   :out         [:or facade/EntryId [:= :vectordb/invalid-write-result]]
   :rel         (fn [in out]
                  (if (#{"fail" "nil"} (:id in))
                    (= :vectordb/invalid-write-result out)
                    (= (:id in) out)))
   :golden-path "test/golden/vectordb/index-memory-entry.edn"})

(hst/deftrifecta-from-schema index-memory-entries-contract
  hive-mcp.vectordb.facade/index-memory-entries!
  {:in          [:vector {:max 4} EntryGen]
   :out         [:vector [:maybe facade/EntryId]]
   ;; positional: each refused write is a nil in its own slot
   :rel         (fn [in out] (= (map #(when-not (#{"fail" "nil"} (:id %)) (:id %)) in) out))
   :golden-path "test/golden/vectordb/index-memory-entries.edn"})

(deftest crud-roundtrip-through-the-port
  (is (= "e1" (facade/index-memory-entry! {:id "e1" :type "note" :tags ["a"]})))
  (is (= "e1" (:id (facade/get-entry-by-id "e1"))))
  (is (= "e1" (:id (facade/get-entry-any-store "e1"))))
  (is (nil? (facade/get-entry-by-id "ghost")))
  (is (nil? (facade/get-entry-any-store "ghost")))
  (is (= ["e1"] (mapv :id (facade/get-entries-by-ids ["e1" "ghost" nil "e1"]))))
  (is (nil? (facade/get-entries-by-ids [nil])) "nothing to ask for is nil")
  (is (= ["e1"] (mapv :id (facade/query-entries :type "note"))))
  (is (= [] (facade/query-entries {:type "decision"})) "a trailing options map is accepted")
  (is (= ["e1"] (mapv :id (facade/search-similar "q" :limit 5))))
  (is (= "x" (:content (facade/update-entry! "e1" {:content "x"}))))
  (is (nil? (facade/update-entry! "ghost" {:content "x"})) "an unknown id is the :absent nil")
  (is (true? (facade/delete-entry! "e1")))
  (is (nil? (facade/delete-entry! "e1")))
  (is (= 0 (facade/cleanup-expired!))))

(deftest index-refusals-throw
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"did not return an entry ID"
                        (facade/index-memory-entry! {:id "fail"})))
  (is (thrown? clojure.lang.ExceptionInfo (facade/index-memory-entry! {:id "nil"}))))

(deftest utilities
  (is (= (facade/content-hash "x") (proto/content-hash "x")))
  (is (= 64 (count (facade/content-hash {:a 1}))))
  (is (m/validate (last (m/children (m/schema (get-in (m/function-schemas) ['hive-mcp.vectordb.facade 'generate-id :schema]))))
                  (facade/generate-id)))
  (is (true? (facade/available?)))
  (is (some? (facade/get-store)))
  (is (= (facade/get-store) (facade/get-store :default)))
  (is (thrown? Exception (facade/get-store :kanban)) "an empty slot throws")
  (is (boolean? (facade/embedding-configured?))))

(def ^:private HashOut
  "content-hash's own contracted output (an anchored hex :re with a
   generator), read from the registry so the test and the contract agree."
  (-> (m/function-schemas) (get-in ['hive-mcp.vectordb.facade 'content-hash :schema])
      m/schema m/children last))

(hst/deftrifecta-from-schema content-hash-contract
  hive-mcp.vectordb.facade/content-hash
  {:in          [:or [:enum "a" "b" "hello world" ""] [:map [:k [:enum 1 2]]]]
   :out         HashOut
   :rel         (fn [in out] (= out (proto/content-hash in)))
   :golden-path "test/golden/vectordb/content-hash.edn"})

;;; ---------------------------------------------------------------------------
;;; kanban-facade (mode :default; the routing modes are pinned in
;;; kanban-facade-trifecta-test)
;;; ---------------------------------------------------------------------------

(hst/deftrifecta-from-schema write-rejected-contract
  hive-mcp.vectordb.kanban-facade/write-rejected?
  {:in          [:or [:enum nil true "id-1"] [:enum {} {:queued? true}]
                 [:enum {} {:success? false :error "x"}] [:enum {} {:error :milvus/unavailable}]]
   :out         :boolean
   :rel         (fn [in out] (= out (or (nil? in)
                                        (and (map? in) (or (false? (:success? in)) (contains? in :error))))))
   :n-cases     8
   :golden-path "test/golden/vectordb/write-rejected.edn"})

(deftest kanban-facade-arms-in-any-mode
  ;; The mode is read from the host config. Both slots are registered over one
  ;; shared db, so every mode routes to a store holding the same entries and
  ;; these assertions hold whichever mode the config names.
  (proto/register-store! :kanban (stub-store db answer-by-id))
  (is (m/validate kf/Mode (kf/mode)))
  (is (= ({:default :default :kanban :kanban :dual-read :kanban} (kf/mode)) (kf/active-key)))
  (is (true? (kf/registered? :default)))
  (is (true? (kf/registered? :kanban)))
  (is (false? (kf/registered? :nope)))
  (is (true? (kf/available?)))
  (is (= {:mode (kf/mode) :ok? true :warning nil} (kf/validate-mode!)))
  (is (= "k1" (kf/add-entry! {:id "k1" :type "kanban" :tags ["kanban"]})))
  (is (= "k1" (:id (kf/get-entry-by-id "k1"))))
  (is (= ["k1"] (mapv :id (kf/query-entries :type "kanban"))))
  (testing "a write the store accepted is read back through the overlay"
    (kf/update-entry! "k1" {:content {:status "done"}})
    (is (= {:status "done"} (:content (kf/get-entry-by-id "k1")))))
  (testing "an unknown id is refused and not remembered"
    (is (nil? (kf/update-entry! "ghost" {:content {:status "x"}})))
    (is (nil? (kf/get-entry-by-id "ghost"))))
  (is (true? (kf/delete-entry! "k1")))
  (is (nil? (kf/get-entry-by-id "k1")) "the overlay is forgotten on delete"))

(deftest validate-mode-without-a-kanban-slot
  (let [v (kf/validate-mode!)]
    (is (m/validate [:map [:mode kf/Mode] [:ok? :boolean] [:warning [:maybe :string]]] v))
    (is (= (not= :kanban (:mode v)) (:ok? v)) "only :kanban mode requires the slot")
    (is (= (= :default (:mode v)) (nil? (:warning v))) ":default never warns")))
