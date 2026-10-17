(ns hive-mcp.tools.migrate.kanban.store-test
  "SEAL-KANBAN-MIGRATOR-BYPASS: the migrator's writer AND target reader go
   through the target store's port, so a decorator (the seal) over that
   store sees every migrated entry, reads come back opened, and derived
   fields never travel.

   No live store: the target is hive-spi's atom-backed stub store, wrapped
   in test decorators that mark content the way a cipher layer would."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-dsl.result :as r]
            [hive-mcp.tools.migrate.kanban.api :as api]
            [hive-mcp.tools.migrate.kanban.events :as mig-events]
            [hive-mcp.tools.migrate.kanban.ports :as ports]
            [hive-mcp.tools.migrate.kanban.pure :as pure]
            [hive-mcp.tools.migrate.kanban.qdrant :as mig-qdrant]
            [hive-mcp.tools.migrate.kanban.state :as state]
            [hive-mcp.tools.migrate.kanban.store :as mig-store]
            [hive-mcp.tools.migrate.kanban.usecases :as uc]
            [hive-spi.memory.ports :as sp]
            [hive-spi.memory.stub :as stub]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-spi.memory.decorate :as decorate]))

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

(def ^:private sealed-prefix "#sealed:")

(defrecord CipherLikeLayer [inner]
  ;; Shaped like hive-knowledge's seal layers: a record with only [inner],
  ;; no backend fields (no :client-atom), declared a hive-spi decorator so
  ;; `decorate/base-store` peels it. Seals :content on write as an
  ;; EDN-printed marker, opens it on every read exit.
  decorate/IStoreDecorator
  (inner-store [_] inner)
  sp/IMemoryStore
  (add-entry! [_ e]
    (sp/add-entry! inner (assoc e :content (str sealed-prefix (pr-str (:content e))))))
  (get-entry [_ id]
    (let [e (sp/get-entry inner id)
          c (:content e)]
      (cond-> e
        (and (string? c) (.startsWith ^String c sealed-prefix))
        (assoc :content (read-string (subs c (count sealed-prefix))))))))

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
               (gen/return {:success? true :queued? true})
               (gen/return {:success? false :error "boom"})
               (gen/return {:error "x"})
               (gen/return nil)
               gen/small-integer]))

(deftrifecta write-result-trifecta
  hive-mcp.tools.migrate.kanban.pure/write-result
  {:gen (gen/tuple gen/string-alphanumeric gen-answer)
   :apply? true
   :pred (fn [r] (and (map? r) (boolean? (:ok? r)) (contains? r :id)
                      ;; a queued ack is never ok
                      (not (and (:ok? r) (:queued? r)))))
   :num-tests 100
   :mutations [["always-ok"   (fn [id _] {:id id :ok? true :error nil})]
               ["always-fail" (fn [id _] {:id id :ok? false :error "x"})]]
   :assert (fn []
             (is (true?  (:ok? (pure/write-result "a" "a"))))
             (is (false? (:ok? (pure/write-result "a" {:success? false :error "e"}))))
             (is (= "e"  (:error (pure/write-result "a" {:error "e"}))))
             (is (false? (:ok? (pure/write-result "a" nil))))
             (is (false? (:ok? (pure/write-result "a" "")))))})

(deftest a-queued-ack-is-not-a-durable-write
  (testing "hive-qdrant's open-circuit answer: not ok, flagged queued"
    (let [row (pure/write-result "a" {:success? true :queued? true :depth 1})]
      (is (false? (:ok? row)))
      (is (true? (:queued? row)))))
  (testing "a landed id is ok and not queued"
    (let [row (pure/write-result "a" "a")]
      (is (true? (:ok? row)))
      (is (not (:queued? row)))))
  (testing "a queued ack that also failed is a plain failure"
    (is (not (:queued? (pure/write-result "a" {:queued? true :success? false}))))))

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
               :target-probe  (mig-store/make-id-probe inner)
               :writer        (mig-store/make-entry-writer (sealing-store inner seen))
               :state         (->MemState (atom state/initial-state))}
        _     (uc/init-ids! deps)
        res   (uc/run! deps {:batch-size 1 :max-steps 5})]
    (is (r/ok? res))
    (is (= 2 (count @seen)))
    (is (every? #(nil? (:embed-text %)) @seen))))

;; =============================================================================
;; Store reader: through the decorated store's port, content opened
;; =============================================================================

(deftest reader-reads-through-the-decorator-opened
  (let [inner (stub/create-store)
        layer (->CipherLikeLayer inner)
        _     (sp/add-entry! layer (dissoc (kanban-entry "k1") :embed-text :embedding))
        res   (ports/read-by-ids (mig-store/make-entry-reader layer) ["k1" "absent"])]
    (is (string? (:content (sp/get-entry inner "k1"))) "at rest it is sealed")
    (is (r/ok? res))
    (is (= #{"k1"} (set (keys (:ok res)))))
    (is (= "kanban" (get-in res [:ok "k1" :content :task-type])) "read back opened")))

(deftest reader-uses-batch-port-and-strips-derived
  (let [inner (stub/create-store)
        _     (sp/add-entry! inner {:id "k1" :content {:task-type "kanban" :t 1}})
        res   (ports/read-by-ids (mig-store/make-entry-reader inner) ["k1"])]
    (is (= {:task-type "kanban" :t 1} (get-in res [:ok "k1" :content])))
    (is (not-any? #(contains? (get-in res [:ok "k1"]) %) pure/derived-fields))))

(deftest reader-surfaces-a-failure-map-as-err
  (let [store (reify sp/IMemoryStore
                (get-entry [_ _] {:success? false :error :circuit-open}))]
    (is (r/err? (ports/read-by-ids (mig-store/make-entry-reader store) ["a"]))))
  (is (= (r/ok {}) (ports/read-by-ids (mig-store/make-entry-reader nil) [])))
  (is (r/err? (ports/read-by-ids (mig-store/make-entry-reader nil) ["a"]))))

(deftest default-make-deps-over-a-decorator-migrates-an-entry
  ;; The registry hands make-deps the DECORATED store: a record with only
  ;; [inner]. A step through the default wiring must read and write
  ;; through it, and land the entry sealed.
  (let [inner  (stub/create-store)
        layer  (->CipherLikeLayer inner)
        src    (kanban-entry "m1")
        deps   (api/make-deps :target-store  layer
                              :source-lister (->FixedLister ["m1"])
                              :source-reader (->MapReader {"m1" src})
                              :state         (->MemState (atom state/initial-state)))
        _      (uc/init-ids! deps)
        res    (api/step! deps {:batch-size 10})]
    (is (r/ok? res) (pr-str res))
    (is (= :written (-> res :ok :batch-result :outcomes first :outcome)))
    (is (= 1 (-> res :ok :state :stats :written)))
    (testing "landed sealed at rest, without derived fields"
      (let [at-rest (sp/get-entry inner "m1")]
        (is (string? (:content at-rest)))
        (is (.startsWith ^String (:content at-rest) sealed-prefix))
        (is (nil? (:embed-text at-rest)))))
    (testing "a second pass reads it opened and does not overwrite it"
      (let [deps2 (assoc deps :state (->MemState (atom (assoc state/initial-state
                                                                :all-ids ["m1"]))))
            res2  (api/step! deps2 {:batch-size 10})]
        (is (= :already-full (-> res2 :ok :batch-result :outcomes first :outcome)))))))

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
