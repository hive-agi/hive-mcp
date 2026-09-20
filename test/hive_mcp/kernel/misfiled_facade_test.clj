(ns hive-mcp.kernel.misfiled-facade-test
  "Three old names are facades over kernel code that used to sit under an
   extraction prefix: `vectordb.resilience` -> `resilience.store`,
   `knowledge-graph.slots.breaker` -> `resilience.breaker`,
   `memory.write-events` -> `events.write-events`.

   Two properties keep an old name honest: it names nothing the kernel does
   not have, and it calls through the kernel VAR, so a reload or a redef of
   the kernel reaches a caller that still spells the old name. Same contract
   as `hive-mcp.knowledge-graph.scope-facade-test` (memory
   20260919135346-7838bb58)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.events.write-events :as we]
            [hive-mcp.knowledge-graph.slots.breaker :as old-breaker]
            [hive-mcp.memory.write-events :as old-we]
            [hive-mcp.resilience.breaker :as breaker]
            [hive-mcp.resilience.store :as store]
            [hive-mcp.vectordb.resilience :as old-store]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- arglists [v] (:arglists (meta v)))

(defn- shapes
  "Arities of V as a set, each param erased to `_` and `&` kept in place. A
   facade delegates its argument whole, so it does not repeat the kernel's
   destructuring; arity and variadicity are what must agree."
  [v]
  (set (map (fn [al] (mapv #(if (= '& %) '& '_) al))
            (arglists v))))

(def ^:private facade->kernel
  {'hive-mcp.vectordb.resilience            'hive-mcp.resilience.store
   'hive-mcp.knowledge-graph.slots.breaker  'hive-mcp.resilience.breaker
   'hive-mcp.memory.write-events            'hive-mcp.events.write-events})

(deftest every-old-name-has-a-kernel-var-of-the-same-shape
  (doseq [[facade-ns kernel-ns] facade->kernel]
    (testing (str facade-ns)
      (let [kernel (ns-publics kernel-ns)
            facade (ns-publics facade-ns)]
        (is (seq facade))
        (doseq [[sym v] facade]
          (testing (str sym)
            (is (contains? kernel sym) "the old name points at nothing")
            (is (= (shapes (get kernel sym)) (shapes v)))))))))

(deftest a-redef-of-the-kernel-var-reaches-the-old-names
  (testing "resilience.store"
    (with-redefs [store/call-with-resilience (fn [_f & _] :redefined)
                  store/transient-failure?   (constantly :redefined)]
      (is (= :redefined (old-store/call-with-resilience (fn [] :never))))
      (is (= :redefined (old-store/transient-failure? (Exception. "x"))))))
  (testing "resilience.breaker"
    (with-redefs [breaker/decision (constantly :redefined)
                  breaker/fresh    (constantly :redefined)]
      (is (= :redefined (old-breaker/decision {:state :closed})))
      (is (= :redefined (old-breaker/fresh)))))
  (testing "events.write-events"
    (with-redefs [we/notify! (fn [_op _payload] :redefined)]
      (is (= :redefined (old-we/notify! :added {:id "x"}))))))

(deftest the-old-write-events-name-shares-one-listener-registry
  (let [seen (atom [])]
    (try
      (old-we/register-listener! ::facade (fn [w] (swap! seen conj w)))
      (is (contains? (we/listener-keys) ::facade)
          "the facade registered into the kernel's registry, not a second atom")
      (we/notify! :added {:id "shared" :memory-type "note"})
      (is (= [{:op :added :id "shared" :memory-type "note"}] @seen))
      (finally
        (old-we/unregister-listener! ::facade)))
    (is (not (contains? (we/listener-keys) ::facade)))))

(deftest the-old-macros-expand-to-the-kernel-fns
  (is (= 42 (old-store/with-resilience 42)))
  (is (= {:ok 43} (select-keys (old-store/with-resilience-result 43) [:ok]))))
