(ns hive-mcp.tools.migrate.kanban.store-test
  "SEAL-KANBAN-MIGRATOR-BYPASS: the migrator's writer goes through the
   target store's add-entry!, so a decorator (the seal) over that store
   sees every migrated entry, and derived fields never travel.

   No live store: the target is hive-spi's atom-backed stub store, wrapped
   in a test decorator that marks content the way a cipher layer would."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-dsl.result :as r]
            [hive-mcp.tools.migrate.kanban.events :as mig-events]
            [hive-mcp.tools.migrate.kanban.ports :as ports]
            [hive-mcp.tools.migrate.kanban.pure :as pure]
            [hive-mcp.tools.migrate.kanban.qdrant :as mig-qdrant]
            [hive-mcp.tools.migrate.kanban.state :as state]
            [hive-mcp.tools.migrate.kanban.store :as mig-store]
            [hive-mcp.tools.migrate.kanban.usecases :as uc]
            [hive-spi.memory.ports :as sp]
            [hive-spi.memory.stub :as stub]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Stubs
;; =============================================================================

(defn- sealing-store
  "A decorator over INNER, as the seal chain installs one: add-entry!
   replaces :content with a ciphertext marker and records every entry it
   saw. Only add-entry! is implemented: the writer calls nothing else."
  [inner seen]
  (reify sp/IMemoryStore
    (add-entry! [_ e]
      (swap! seen conj e)
      (sp/add-entry! inner (assoc e :content (str "#sealed:" (hash (:content e))))))))

(defrecord MemState [atm]
  ports/IState
  (load-state [_]    (r/ok @atm))
  (save-state! [_ s] (reset! atm s) (r/ok s))
  (reset-state! [_]  (reset! atm state/initial-state) (r/ok state/initial-state)))

(defrecord MapReader [m]
  ports/IEntryReader
  (read-by-ids [_ ids] (r/ok (select-keys m ids))))

(defrecord FixedLister [ids]
  ports/IIdLister
  (list-ids [_] (r/ok {:ids ids :per-collection {"stub" (count ids)}})))

(defn- kanban-entry [id]
  {:id id :type :note :tags ["kanban" "todo"]
   :content {:task-type "kanban" :title (str "t-" id) :status "todo"}
   :embed-text (str "plaintext " id)
   :embedding [0.1 0.2]})

;; =============================================================================
;; pure/write-result
;; =============================================================================

(def gen-answer
  (gen/one-of [gen/string-alphanumeric
               (gen/return {:queued? true})
               (gen/return {:success? false :error "boom"})
               (gen/return {:error "x"})
               (gen/return nil)
               gen/small-integer]))

(deftrifecta write-result-trifecta
  hive-mcp.tools.migrate.kanban.pure/write-result
  {:gen (gen/tuple gen/string-alphanumeric gen-answer)
   :apply? true
   :pred (fn [r] (and (map? r) (boolean? (:ok? r)) (contains? r :id)))
   :num-tests 100
   :mutations [["always-ok"   (fn [id _] {:id id :ok? true :error nil})]
               ["always-fail" (fn [id _] {:id id :ok? false :error "x"})]]
   :assert (fn []
             (is (true?  (:ok? (pure/write-result "a" "a"))))
             (is (true?  (:ok? (pure/write-result "a" {:queued? true}))))
             (is (false? (:ok? (pure/write-result "a" {:success? false :error "e"}))))
             (is (= "e"  (:error (pure/write-result "a" {:error "e"}))))
             (is (false? (:ok? (pure/write-result "a" nil))))
             (is (false? (:ok? (pure/write-result "a" "")))))})

(deftrifecta strip-derived-trifecta
  hive-mcp.tools.migrate.kanban.pure/strip-derived
  {:gen (gen/one-of [(gen/return nil)
                     (gen/hash-map :id gen/string-alphanumeric
                                   :embed-text gen/string-alphanumeric
                                   :embedding (gen/vector gen/double))])
   :pred (fn [e] (or (nil? e) (not-any? #(contains? e %) pure/derived-fields)))
   :num-tests 100
   :mutations [["identity" identity]
               ["drop-only-embedding" (fn [e] (some-> e (dissoc :embedding)))]]
   :assert (fn []
             (is (nil? (pure/strip-derived nil)))
             (is (= {:id "a" :content "c"}
                    (pure/strip-derived {:id "a" :content "c" :embed-text "c" :embedding [1.0]}))))})

;; =============================================================================
;; Store writer: through the decorator, derived fields dropped
;; =============================================================================

(deftest writer-goes-through-the-decorated-store
  (let [inner  (stub/create-store)
        seen   (atom [])
        store  (sealing-store inner seen)
        w      (mig-store/make-entry-writer store)
        res    (ports/write-entries w [(kanban-entry "k1") (kanban-entry "k2")])]
    (is (r/ok? res))
    (is (= [{:id "k1" :ok? true :error nil} {:id "k2" :ok? true :error nil}] (:ok res)))
    (testing "the decorator saw every entry"
      (is (= #{"k1" "k2"} (set (map :id @seen)))))
    (testing "no derived field reached the store"
      (is (every? #(not-any? (partial contains? %) pure/derived-fields) @seen)))
    (testing "what landed is what the decorator wrote, not the plaintext"
      (is (= "#sealed:" (subs (:content (sp/get-entry inner "k1")) 0 8)))
      (is (nil? (:embed-text (sp/get-entry inner "k1")))))))

(deftest writer-isolates-a-throwing-entry
  (let [inner (stub/create-store)
        store (reify sp/IMemoryStore
                (add-entry! [_ e]
                  (if (= "bad" (:id e))
                    (throw (ex-info "refused" {}))
                    (sp/add-entry! inner e))))
        res   (ports/write-entries (mig-store/make-entry-writer store)
                                   [(kanban-entry "ok") (kanban-entry "bad")])]
    (is (= [true false] (mapv :ok? (:ok res))))
    (is (= "refused" (:error (second (:ok res)))))))

(deftest writer-without-store-is-an-err
  (is (r/err? (ports/write-entries (mig-store/make-entry-writer nil) [(kanban-entry "a")])))
  (is (= (r/ok []) (ports/write-entries (mig-store/make-entry-writer nil) []))))

(deftest migration-end-to-end-lands-through-decorator
  (mig-events/init!)
  (let [inner (stub/create-store)
        seen  (atom [])
        deps  {:source-lister (->FixedLister ["a" "b"])
               :source-reader (->MapReader {"a" (kanban-entry "a") "b" (kanban-entry "b")})
               :target-reader (->MapReader {})
               :writer        (mig-store/make-entry-writer (sealing-store inner seen))
               :state         (->MemState (atom state/initial-state))}
        _     (uc/init-ids! deps)
        res   (uc/run! deps {:batch-size 1 :max-steps 5})]
    (is (r/ok? res))
    (is (= 2 (count @seen)))
    (is (every? #(nil? (:embed-text %)) @seen))))

;; =============================================================================
;; Qdrant reader boundary strips derived fields
;; =============================================================================

(deftest reader-strips-embed-text
  (let [pt-id   (str (java.util.UUID/nameUUIDFromBytes (.getBytes "k1" "UTF-8")))
        decoded {:id pt-id :payload {:id "k1" :content "{\"task-type\":\"kanban\"}"
                                     :embed-text "plaintext" :embedding [1.0]}}
        out     (mig-qdrant/decode-results-by-id [decoded] ["k1"] identity)]
    (is (= {:task-type "kanban"} (get-in out ["k1" :content])))
    (is (not (contains? (get out "k1") :embed-text)))
    (is (not (contains? (get out "k1") :embedding)))))
