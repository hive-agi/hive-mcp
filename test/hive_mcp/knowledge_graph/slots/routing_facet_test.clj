;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.knowledge-graph.slots.routing-facet-test
  "Goldens over the factory, the registry and the SlotInit ADT, with an
   injected factory fn and in-memory DataScript stores only."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dsl.adt :as adt]
            [hive-mcp.knowledge-graph.slots.config :as cfg]
            [hive-mcp.knowledge-graph.slots.factory :as factory]
            [hive-mcp.knowledge-graph.slots.protocol :as p]
            [hive-mcp.knowledge-graph.slots.registry :as registry]
            [hive-mcp.knowledge-graph.slots.schema :as ss]
            [hive-mcp.knowledge-graph.store.datascript :as ds-store]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

(golden/deftest-golden factory-golden
  "test/golden/hive-mcp/m14-slots-factory.edn"
  (let [seen (atom [])
        two-arity (factory/->factory (fn [backend opts] (swap! seen conj [backend opts]) :store-2))
        one-arity (factory/->factory (fn [backend] (swap! seen conj [backend]) :store-1))
        nil-fn (factory/->factory (constantly nil))]
    {:two [(p/make-store two-arity :datascript) (p/make-store two-arity :datalevin {:recovery-policy :x})]
     :one (p/make-store one-arity :datahike {:ignored true})
     :nil (p/make-store nil-fn :proximum)
     :seen @seen
     :supported (sort (p/supported-backends (factory/->factory)))}))

(defn- registry-run []
  (let [lookup (fn [path] (when (= :backend (last path))
                            (case (nth path 3) :alpha "datascript" :beta "missing" nil)))
        made (atom 0)
        fac (factory/->factory (fn [backend _opts]
                                 (when (= :datascript backend)
                                   (swap! made inc)
                                   (ds-store/create-store))))
        reg (registry/->registry (cfg/->resolver lookup {}) fac {:max-failures 1})]
    (try
      (let [alpha (p/describe-slot reg :alpha)
            again (p/describe-slot reg :alpha)
            beta (p/describe-slot reg :beta)
            beta-open (p/describe-slot reg :beta)]
        {:alpha (adt/adt-variant alpha)
         :alpha-cached (adt/adt-variant again)
         :made @made
         :beta (select-keys beta [:backend :reason])
         :beta-breaker (select-keys beta-open [:reason])
         :registered (p/registered reg)
         :store-satisfies? (m/validate ss/Registry reg)})
      (finally (p/close-all! reg)))))

(golden/deftest-golden registry-golden
  "test/golden/hive-mcp/m14-slots-registry.edn"
  (registry-run))

(golden/deftest-golden slot-init-golden
  "test/golden/hive-mcp/m14-slots-protocol.edn"
  (mapv (fn [[variant data]]
          (let [init (p/slot-init variant data)]
            [(adt/adt-variant init) (dissoc init :store)]))
        [[:slot/ok {:slot :carto :backend :datascript :store :s}]
         [:slot/missing-backend {:slot :carto}]
         [:slot/factory-failed {:slot :carto :backend :datalevin :reason :factory-returned-nil}]]))

(deftest every-slots-schema-generates
  (doseq [[sym v] (ns-publics 'hive-mcp.knowledge-graph.slots.schema)]
    (testing (str sym)
      (let [xs (mg/sample @v {:size 5 :seed 14})]
        (is (= 5 (count xs)))
        (is (every? #(m/validate @v %) xs))))))
