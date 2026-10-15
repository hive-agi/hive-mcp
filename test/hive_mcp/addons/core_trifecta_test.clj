(ns hive-mcp.addons.core-trifecta-test
  "Trifecta for the addon registry (`hive-mcp.addons.core`).

   The registry is an atom, so the subject is driven the only honest way: a
   reified `IAddon` stub injected through `register-addon!` (no with-redefs),
   and a REGISTRATION HISTORY generated from a small vocabulary of lifecycle
   operations. The run function resets the registry, replays the history and
   answers a timestamp-free projection of `registry-status`, so the golden is
   deterministic. Every operation result is also validated against the
   contract's own `AddonOutcome` schema inside the property."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.protocol :as proto]
            [hive-mcp.addons.core :as addons]
            [hive-mcp.addons.schema :as schema]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f]
    (addons/reset-registry!)
    (try (f) (finally (addons/reset-registry!)))))

;; =============================================================================
;; The IAddon stub (a port implementation, not a redef)
;; =============================================================================

(defn- stub
  "A reified IAddon: `id`, advertising `caps`; `fail-init?` makes initialize!
   report failure the way a real addon does (a result map, not a throw)."
  [id caps fail-init?]
  (reify proto/IAddon
    (addon-id [_] id)
    (addon-type [_] :native)
    (capabilities [_] caps)
    (initialize! [_ _] (if fail-init?
                         {:success? false :errors ["init refused"]}
                         {:success? true :errors [] :metadata {}}))
    (shutdown! [_] {:success? true :errors []})
    (tools [_] [])
    (schema-extensions [_] {})
    (health [_] {:status :ok})))

(def ^:private ids [:a :b :c])
(def ^:private caps-of {:a #{:memory} :b #{:memory :carto} :c #{}})

(def ^:private gen-op
  (gen/tuple (gen/elements [:register :init :shutdown :unregister :init-all :shutdown-all])
             (gen/elements ids)))

(def ^:private gen-history (gen/vector gen-op 0 12))

(defn- apply-op!
  "Replay one operation; answer its result."
  [[op id]]
  (case op
    :register     (addons/register-addon! (stub id (caps-of id) (= id :c)))
    :init         (addons/init-addon! id)
    :shutdown     (addons/shutdown-addon! id)
    :unregister   (addons/unregister-addon! id)
    :init-all     (addons/init-all!)
    :shutdown-all (addons/shutdown-all!)))

(defn- valid-result? [op r]
  (case (first op)
    (:init-all :shutdown-all) (m/validate [:map-of schema/RegistryId schema/AddonOutcome] r)
    (m/validate schema/AddonOutcome r)))

(defn run-history
  "Unary adapter: a lifecycle history -> a timestamp-free registry projection."
  [history]
  (addons/reset-registry!)
  (doseq [op history] (apply-op! op))
  (let [st (addons/registry-status)]
    {:total      (:total st)
     :active     (:active st)
     :registered (:registered st)
     :error      (:error st)
     :states     (into (sorted-map) (map (juxt (comp str :name) :state)) (:addons st))
     :capabilities (into (sorted-map)
                         (map (fn [[k v]] [k (vec (sort-by str v))]))
                         (:capabilities st))}))

(deftrifecta registry-lifecycle-contract
  hive-mcp.addons.core-trifecta-test/run-history
  {:golden-path "test/golden/addons/core-registry-lifecycle.edn"
   :cases {:empty            []
           :register-one     [[:register :a]]
           :register-twice   [[:register :a] [:register :a]]
           :init-and-shut    [[:register :a] [:init :a] [:shutdown :a]]
           :failed-init      [[:register :c] [:init :c]]
           :caps-aggregate   [[:register :a] [:register :b] [:init-all :a]]
           :unregister-live  [[:register :b] [:init :b] [:unregister :b]]
           :shutdown-all     [[:register :a] [:register :b] [:init-all :a] [:shutdown-all :a]]}
   :gen gen-history
   :pred (fn [r] (= (:total r) (+ (:active r) (:registered r) (:error r))))
   :num-tests 100
   :mutations [["counts only active entries"
                (fn [h] (let [r (run-history h)] (assoc r :total (:active r))))]
               ["forgets capabilities"
                (fn [h] (assoc (run-history h) :capabilities {}))]
               ["drops the registry"
                (constantly nil)]]
   :assert (fn []
             (is (= {:total 1 :active 1 :registered 0 :error 0}
                    (select-keys (run-history [[:register :a] [:init :a]])
                                 [:total :active :registered :error])))
             (is (= {":c" :error} (:states (run-history [[:register :c] [:init :c]])))
                 "a refused init leaves the entry in :error, still registered"))})

(defspec every-lifecycle-result-satisfies-its-contract 100
  (prop/for-all [history gen-history]
    (addons/reset-registry!)
    (every? (fn [op] (valid-result? op (apply-op! op))) history)))

(defspec capabilities-index-lists-only-active-addons 100
  (prop/for-all [history gen-history]
    (let [r (run-history history)
          indexed (set (mapcat val (:capabilities r)))]
      ;; every addon named by the capability index is active
      (every? (fn [id] (= :active (get (:states r) (str id)))) indexed))))

;; =============================================================================
;; Reads that are pure given the registry
;; =============================================================================

(deftest reads-agree-with-the-registry-test
  (addons/register-addon! (stub :a #{:memory} false))
  (addons/register-addon! (stub :b #{:memory :carto} false))
  (addons/init-addon! :a)
  (testing "get-addon / addon-registered? / get-addon-entry"
    (is (addons/addon-registered? :a))
    (is (not (addons/addon-registered? :zzz)))
    (is (m/validate schema/IAddonInstance (addons/get-addon :a)))
    (is (nil? (addons/get-addon :zzz)))
    (is (m/validate schema/AddonEntry (addons/get-addon-entry :a)))
    (is (nil? (addons/get-addon-entry :zzz))))
  (testing "capability queries see only ACTIVE addons"
    (is (= [:a] (addons/addons-with-capability :memory)))
    (is (= [] (addons/addons-with-capability :carto)))
    (is (= {:memory [:a]} (addons/all-capabilities))))
  (testing "list-addons carries every registered addon"
    (is (= #{:a :b} (set (map :name (addons/list-addons)))))
    (is (every? #(m/validate schema/AddonListing %) (addons/list-addons))))
  (testing "check-dependencies on a registered addon with none declared"
    (is (= {:satisfied? true :missing #{} :available #{}}
           (addons/check-dependencies :a)))
    (is (= {:satisfied? false :missing #{:zzz} :available #{}}
           (addons/check-dependencies :zzz))))
  (testing "no tools contributed -> no installed tools and an empty resolution"
    (is (= [] (addons/active-addon-tools)))
    (is (= {:installed [] :refused [] :claims []} (addons/resolve-addon-tools)))
    (is (= [] (addons/addon-tools-by-name :a)))
    (is (= #{} (addons/claimed-core-names (addons/resolve-addon-tools))))))

(deftest declared-config-is-a-map-test
  (is (= {} (addons/addon-declared-config :no-such-addon))
      "an addon with no declaration reads as the empty map, never nil"))

(deftest status-of-the-empty-registry-test
  (is (m/validate schema/RegistryStatus (addons/registry-status)))
  (is (= 0 (:total (addons/registry-status)))))

(deftest contracts-discriminate-test
  (testing "the lifecycle outcome demands a success flag and an addon name"
    (is (not (m/validate schema/AddonOutcome {:success? true})))
    (is (not (m/validate schema/AddonOutcome {:addon-name :a}))))
  (testing "a state outside the lifecycle is rejected"
    (is (not (m/validate schema/AddonState :zombie)))))
