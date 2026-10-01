(ns hive-mcp.hot.reseat-test
  "The re-seat registry: which re-seaters a pass runs (pure, trifecta-pinned),
   that they run in load order with faults folded, and that `via-var` follows
   a var the way clj-reload replaces it. Every re-seater here is a recording
   stub; nothing real is re-seated."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.hot.reseat :as reseat]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; reseat-plan — golden + property + mutation
;; =============================================================================

(def ^:private gen-ns-sym
  (gen/elements '[a.one a.two a.three b.one b.two]))

(def ^:private gen-registry
  (gen/fmap (fn [ks] (into {} (map (fn [k] [k (keyword (name k))])) ks))
            (gen/vector gen-ns-sym 0 5)))

(def ^:private gen-loaded
  (gen/vector (gen/fmap str gen-ns-sym) 0 8))

(deftrifecta reseat-plan-contract
  hive-mcp.hot.reseat/reseat-plan
  {:golden-path "test/golden/hot/reseat-plan.edn"
   :apply?      true
   :cases       {:nothing-registered [{} ["a.one" "a.two"]]
                 :nothing-loaded     [{'a.one :one} []]
                 :only-loaded-run    [{'a.one :one 'b.two :two} ["a.two" "b.two"]]
                 :load-order-wins    [{'a.one :one 'a.two :two 'a.three :three}
                                      ["a.three" "a.one" "a.two"]]
                 :each-once          [{'a.one :one} ["a.one" "a.one"]]
                 :symbols-accepted   [{'a.one :one} ['a.one]]}
   :gen         (gen/tuple gen-registry gen-loaded)
   :pred        #(m/validate reseat/ReseatPlan %)
   :num-tests   300
   :mutations   [["runs-every-registered — ignores what was loaded"
                  (fn [registry _] (vec registry))]
                 ["registry-order — loses the load order"
                  (fn [registry loaded]
                    (let [l (set (map symbol loaded))]
                      (vec (filter (comp l key) (sort-by (comp str key) registry)))))]
                 ["no-dedupe — runs a re-seater twice"
                  (fn [registry loaded]
                    (into [] (keep (fn [n] (when-let [f (get registry (symbol n))] [(symbol n) f])))
                          loaded))]
                 ["runs-nothing" (fn [_ _] [])]]})

(defspec a-plan-runs-only-loaded-registered-namespaces-once-in-load-order 300
  (prop/for-all [registry gen-registry
                 loaded   gen-loaded]
    (let [plan (reseat/reseat-plan registry loaded)
          nses (map first plan)
          order (distinct (map symbol loaded))]
      (and (every? (set order) nses)
           (every? #(contains? registry %) nses)
           (= (count nses) (count (distinct nses)))
           (= nses (filter (set nses) order))
           (= (set nses) (set (filter #(contains? registry %) order)))))))

;; =============================================================================
;; run-plan! — the boundary, with recording stubs
;; =============================================================================

(defn- recorder [log n]
  (fn [loaded] (swap! log conj [n loaded]) {:seated n}))

(deftest the-re-seaters-run-in-load-order-for-loaded-namespaces-only
  (let [log      (atom [])
        registry {'x.a (recorder log :a) 'x.b (recorder log :b) 'x.c (recorder log :c)}
        loaded   ["x.c" "x.other" "x.a"]
        report   (reseat/run-plan! (reseat/reseat-plan registry loaded) loaded)]
    (is (= [[:c loaded] [:a loaded]] @log) "x.b was not loaded, so it did not run")
    (is (= [{:ns "x.c" :result {:seated :c}} {:ns "x.a" :result {:seated :a}}] report))
    (is (m/validate reseat/ReseatReport report))))

(deftest a-throwing-re-seater-is-folded-and-the-rest-still-run
  (let [log      (atom [])
        registry {'x.a (fn [_] (throw (ex-info "host gone" {}))) 'x.b (recorder log :b)}
        report   (reseat/run-plan! (reseat/reseat-plan registry ["x.a" "x.b"]) ["x.a" "x.b"])]
    (is (= [{:ns "x.a" :error "host gone"} {:ns "x.b" :result {:seated :b}}] report))
    (is (= [[:b ["x.a" "x.b"]]] @log))))

(deftest registration-is-keyed-so-a-reload-replaces-it
  (let [k 'hive-mcp.hot.reseat-test.probe]
    (try
      (reseat/register-reseater! k (constantly :old))
      (reseat/register-reseater! k (constantly :new))
      (is (= [{:ns (str k) :result :new}] (reseat/reseat! [(str k)])))
      (is (= [] (reseat/reseat! ["hive-mcp.not-loaded"])))
      (finally (reseat/unregister-reseater! k)))
    (is (not (contains? (reseat/registered) k)))))

;; =============================================================================
;; via-var — Capture-by-Var, including a namespace clj-reload re-creates
;; =============================================================================

(def ^:private probe-ns 'hive-mcp.hot.reseat-test.via-var-probe)

(deftest via-var-follows-the-var-through-a-namespace-re-creation
  (try
    (intern (create-ns probe-ns) 'f (fn [x] [:v1 x]))
    (let [call     (reseat/via-var (symbol (str probe-ns) "f"))
          captured @(ns-resolve probe-ns 'f)]
      (is (= [:v1 1] (call 1)))
      (testing "an in-place redefinition is followed"
        (intern probe-ns 'f (fn [x] [:v2 x]))
        (is (= [:v2 1] (call 1))))
      (testing "remove-ns + load, what clj-reload does, is followed too"
        (remove-ns probe-ns)
        (intern (create-ns probe-ns) 'f (fn [x] [:v3 x]))
        (is (= [:v3 1] (call 1)))
        (is (= [:v1 1] (captured 1)) "a captured value would still run the first body"))
      (testing "an absent var is a loud error, not a silent nil"
        (remove-ns probe-ns)
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not loaded" (call 1)))))
    (finally (remove-ns probe-ns))))
