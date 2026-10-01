(ns hive-mcp.system.addon-hot-test
  ":hive/addon-hot over reified ports: hive-hot is initialized once with every
   mounted addon's dirs and the reloadable addons are registered. No
   with-redefs; hive-hot's global registry is never touched."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.dns.result :as r]
            [hive-mcp.system.addon-hot :as ah]
            [hive-mcp.tools.consolidated.hot :as hot]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- catalog [specs host]
  (reify ah/IAddonCatalog
    (mounted-specs [_] specs)
    (mount-host [_] host)))

(defn- engine
  "A recording IHotEngine. OVERRIDES maps an op to a fn replacing its answer."
  [log & [overrides]]
  (let [rec (fn [op v] (swap! log conj [op v]) v)
        ov  (or overrides {})]
    (reify ah/IHotEngine
      (plan [_ _host specs]
        (rec :plan (if-let [f (:plan ov)]
                     (f specs)
                     {:hot/dirs #{"/a/src" "/b/src"}
                      :hot/no-reload #{'hive-addon.protocol}
                      :hot/registered (filterv :local? specs)
                      :hot/skipped (filterv (complement :local?) specs)})))
      (ensure-init! [_ opts]
        (rec :ensure-init! opts)
        {:initialized? true :fresh? true :dirs (:dirs opts) :added (:dirs opts)})
      (register! [_ _host specs]
        (rec :register! (if-let [f (:register! ov)]
                          (f specs)
                          {:ok? true :hot/registered (filterv :local? specs)})))
      (unregister! [_ specs] (rec :unregister! (mapv :addon/id specs))))))

(def specs [{:addon/id "hive.a" :local? true}
            {:addon/id "hive.b" :local? true}
            {:addon/id "hive.jar" :local? false}])

(deftest boot-initializes-with-every-mounted-addon-dir-and-registers
  (let [log (atom [])
        res (ah/boot! (catalog specs :host) (engine log) 1000)]
    (is (r/ok? res))
    (testing "hive-hot is initialized once, with the plan's dirs, interlock and baseline"
      (let [inits (filter #(= :ensure-init! (first %)) @log)]
        (is (= 1 (count inits)))
        (is (= {:dirs ["/a/src" "/b/src"]
                :no-reload #{'hive-addon.protocol}
                :since 1000}
               (second (first inits))))))
    (testing "the reloadable addons are registered; the jar one is reported skipped"
      (is (= ["hive.a" "hive.b"] (:registered (:ok res))))
      (is (= ["hive.jar"] (:skipped (:ok res))))
      (is (= 3 (:mounted (:ok res))))
      (is (true? (:fresh? (:ok res))))
      (is (true? (:ok? (:ok res)))))
    (testing "init precedes registration"
      (is (= [:plan :ensure-init! :register!] (mapv first @log))))))

(deftest no-mounted-addons-leaves-hive-hot-alone
  (let [log (atom [])
        res (ah/boot! (catalog [] :host) (engine log) nil)]
    (is (r/ok? res))
    (is (= 0 (:mounted (:ok res))))
    (is (empty? @log))))

(deftest a-missing-mount-host-is-an-error-not-an-init
  (let [log (atom [])
        res (ah/boot! (catalog specs nil) (engine log) nil)]
    (is (= :addon-hot/no-mount-host (:error res)))
    (is (empty? @log))))

(deftest a-failing-port-rides-the-railway
  (testing "plan throws: no init, no registration"
    (let [log (atom [])
          res (ah/boot! (catalog specs :host)
                        (engine log {:plan (fn [_] (throw (ex-info "boom" {})))})
                        nil)]
      (is (= :addon-hot/plan-failed (:error res)))
      (is (not-any? #(= :ensure-init! (first %)) @log))))
  (testing "the catalog throws"
    (let [res (ah/boot! (reify ah/IAddonCatalog
                          (mounted-specs [_] (throw (ex-info "x" {})))
                          (mount-host [_] :host))
                        (engine (atom [])) nil)]
      (is (= :addon-hot/catalog-failed (:error res))))))

(deftest registration-errors-are-reported
  (let [res (ah/boot! (catalog specs :host)
                      (engine (atom []) {:register! (fn [_] {:ok? false :errors ["hive.a: nope"]
                                                             :hot/registered []})})
                      nil)]
    (is (r/ok? res))
    (is (false? (:ok? (:ok res))))
    (is (= ["hive.a: nope"] (:errors (:ok res))))))

(deftest start-records-and-stop-deregisters
  (let [log   (atom [])
        cat   (catalog specs :host)
        state (ah/start! cat (engine log) nil)]
    (is (r/ok? (:result state)))
    (is (= (:result state) (ah/last-report)))
    (ah/stop! state)
    (is (= [:unregister! ["hive.a" "hive.b"]] (last @log))))
  (testing "a failed boot deregisters nothing"
    (let [log (atom [])]
      (ah/stop! (ah/start! (catalog specs nil) (engine log) nil))
      (is (empty? @log)))))

(deftest stop-deregisters-what-boot-registered-not-what-is-mounted-at-halt
  (let [log     (atom [])
        mounted (atom [{:addon/id "hive.a" :local? true}
                       {:addon/id "hive.b" :local? true}])
        cat     (reify ah/IAddonCatalog
                  (mounted-specs [_] @mounted)
                  (mount-host [_] :host))
        state   (ah/start! cat (engine log) nil)]
    (is (= ["hive.a" "hive.b"] (:registered (:ok (:result state)))))
    (testing "an addon activated after boot does not leak into halt"
      (swap! mounted conj {:addon/id "hive.c" :local? true})
      (is (= ["hive.a" "hive.b" "hive.c"] (mapv :addon/id (ah/mounted-specs cat))))
      (ah/stop! state)
      (is (= [[:unregister! ["hive.a" "hive.b"]]]
             (filterv #(= :unregister! (first %)) @log))))))

(deftest init-opts-is-pure-and-sorted
  (is (= {:dirs ["/a" "/z"] :no-reload #{}} (ah/init-opts {:hot/dirs #{"/z" "/a"}} nil)))
  (is (= 5 (:since (ah/init-opts {} 5)))))

(deftest hot-status-projects-the-boot-report
  (testing "a successful boot reads as :initialized with its report"
    (ah/start! (catalog specs :host) (engine (atom [])) nil)
    (let [b (hot/boot-status)]
      (is (= :initialized (:state b)))
      (is (= ["hive.a" "hive.b"] (:registered b)))))
  (testing "a failed boot reads as :failed with its category"
    (ah/start! (catalog specs nil) (engine (atom [])) nil)
    (let [b (hot/boot-status)]
      (is (= :failed (:state b)))
      (is (= :addon-hot/no-mount-host (:error b))))))
