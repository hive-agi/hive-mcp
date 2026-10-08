(ns hive-mcp.multi.registry.owned-trifecta-test
  "Golden + property + mutation pinning for the generic owner-tagged registry
   primitive `hive-mcp.multi.registry.owned`.

   The subject is driven through a script of register!/deregister-by-owner!
   steps against a fresh state atom, so the whole policy is observable as data:
   per-step outcomes, the final index and the final owner index.

   Policy pinned: first-write-wins across owners (:conflict leaves the entry
   untouched), same-owner writes replace, deregistration removes only the
   owner's ids.

   Mutants are self-contained: none references the subject var, which
   alter-var-root has already rebound to the mutant by then."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as tc-prop]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.multi.registry.owned :as owned]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Adapter — a script of steps against a fresh registry, as data
;; =============================================================================

(defn run-script
  "Unary adapter: vector of [:register owner id entry] / [:deregister owner]
   -> {:steps [outcome-or-removed-set ...] :all index :by-owner owner-index}."
  [ops]
  (let [state (atom {:idx {} :by-owner {}})
        steps (mapv (fn [[op owner id entry]]
                      (case op
                        :register   (owned/register! state :idx owner id entry
                                                     "[owned-test]" :id)
                        :deregister (owned/deregister-by-owner! state :idx owner)))
                    ops)]
    {:steps    steps
     :all      (owned/all state :idx)
     :by-owner (:by-owner @state)}))

;; =============================================================================
;; Reference model used by the mutants (independent of the subject)
;; =============================================================================

(defn- model
  "Plain-map model of the registry. overwrite? makes foreign writes win;
   deregister? false turns deregistration into a no-op."
  [ops {:keys [overwrite? deregister?]}]
  (let [[steps idx by-owner]
        (reduce
         (fn [[steps idx by-owner] [op owner id entry]]
           (case op
             :register
             (let [existing (get idx id)
                   v (assoc entry :owner owner)]
               (cond
                 (nil? existing)
                 [(conj steps :ok) (assoc idx id v)
                  (update by-owner owner (fnil conj #{}) id)]
                 (= owner (:owner existing))
                 [(conj steps :replaced) (assoc idx id v) by-owner]
                 overwrite?
                 [(conj steps :replaced) (assoc idx id v) by-owner]
                 :else
                 [(conj steps :conflict) idx by-owner]))
             :deregister
             (if deregister?
               (let [ids (get by-owner owner #{})]
                 [(conj steps ids) (apply dissoc idx ids) (dissoc by-owner owner)])
               [(conj steps #{}) idx by-owner])))
         [[] {} {}]
         ops)]
    {:steps steps :all idx :by-owner by-owner}))

;; =============================================================================
;; Generators
;; =============================================================================

(def ^:private gen-owner (gen/elements [:a :b :c]))

(def ^:private gen-id (gen/elements ["x" "y" "z"]))

(def ^:private gen-op
  (gen/one-of
   [(gen/let [o gen-owner i gen-id v (gen/choose 0 9)]
      [:register o i {:v v}])
    (gen/let [o gen-owner]
      [:deregister o])]))

(def ^:private gen-script (gen/vector gen-op 0 25))

;; =============================================================================
;; 1. registry policy — golden + property + mutation
;; =============================================================================

(deftrifecta owned-registry-contract
  hive-mcp.multi.registry.owned-trifecta-test/run-script
  {:golden-path "test/golden/multi/registry-owned.edn"
   :cases       {:empty                       []
                 :fresh                       [[:register :a "x" {:v 1}]]
                 :replace                     [[:register :a "x" {:v 1}]
                                               [:register :a "x" {:v 2}]]
                 :conflict                    [[:register :a "x" {:v 1}]
                                               [:register :b "x" {:v 2}]]
                 :deregister-owner-only       [[:register :a "x" {:v 1}]
                                               [:register :b "y" {:v 2}]
                                               [:deregister :a]]
                 :deregister-unknown          [[:register :a "x" {:v 1}]
                                               [:deregister :zz]]
                 :reregister-after-deregister [[:register :a "x" {:v 1}]
                                               [:deregister :a]
                                               [:register :b "x" {:v 3}]]}
   :gen         gen-script
   :pred        map?
   :num-tests   200
   :mutations   [["constantly-empty"
                  (constantly {:steps [] :all {} :by-owner {}})]
                 ["last-write-wins — a foreign owner overwrites"
                  (fn [ops] (model ops {:overwrite? true :deregister? true}))]
                 ["deregister-is-noop — owner ids are never released"
                  (fn [ops] (model ops {:overwrite? false :deregister? false}))]]
   :assert      (fn []
                  (is (= [:ok :conflict]
                         (:steps (run-script [[:register :a "x" {:v 1}]
                                              [:register :b "x" {:v 2}]])))
                      "first write wins across owners")
                  (is (= {"y" {:v 2 :owner :b}}
                         (:all (run-script [[:register :a "x" {:v 1}]
                                            [:register :b "y" {:v 2}]
                                            [:deregister :a]])))
                      "deregistration removes only the owner's ids"))})

;; =============================================================================
;; 2. Invariants over arbitrary scripts
;; =============================================================================

(defspec owner-index-matches-index 200
  (tc-prop/for-all [ops gen-script]
    ;; by-owner[o] is exactly the set of ids whose entry is stamped with o.
    (let [{:keys [all by-owner]} (run-script ops)
          derived (reduce-kv (fn [acc id {:keys [owner]}]
                               (update acc owner (fnil conj #{}) id))
                             {} all)]
      (= derived by-owner))))

(defspec subject-agrees-with-model 200
  (tc-prop/for-all [ops gen-script]
    (= (run-script ops)
       (model ops {:overwrite? false :deregister? true}))))

(deftest lookup-reads-the-index-test
  (testing "lookup returns the stamped entry or nil"
    (let [state (atom {:idx {} :by-owner {}})]
      (owned/register! state :idx :a "x" {:v 1} "[owned-test]" :id)
      (is (= {:v 1 :owner :a} (owned/lookup state :idx "x")))
      (is (nil? (owned/lookup state :idx "missing"))))))
