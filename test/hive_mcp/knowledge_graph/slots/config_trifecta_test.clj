;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.knowledge-graph.slots.config-trifecta-test
  "Trifecta for resolve-recovery-policy over injected config lookups, and a
   golden over the resolver's Strategy chain."
  (:require [hive-mcp.knowledge-graph.slots.config :as cfg]
            [hive-mcp.knowledge-graph.slots.protocol :as p]
            [hive-mcp.knowledge-graph.slots.schema :as ss]
            [hive-schemas.test :as hst]
            [hive-test.golden :as golden]
            [malli.core :as m]))

(defn- out-of-arity [nsym fsym n]
  (let [s (get-in (m/function-schemas) [nsym fsym :schema])]
    (second (m/children (nth (m/children s) n)))))

(def ^:private Slot [:enum :carto :sessions :memory :default :carto-vec :unknown])

;; No :golden-path: the lookup fns in the input are not EDN. The golden below
;; pins the same arms by name.
(hst/deftrifecta-from-schema resolve-recovery-policy-contract
  hive-mcp.knowledge-graph.slots.config/resolve-recovery-policy
  {:in [:cat ss/PathLookup Slot]
   :out (out-of-arity 'hive-mcp.knowledge-graph.slots.config 'resolve-recovery-policy 1)
   :rel (fn [[lookup slot] out]
          (let [raw (lookup [:services :kg :slots slot :recovery-policy])]
            (if (map? raw)
              (and (keyword? (:strategy out)) (= (:max-attempts raw) (:max-attempts out)))
              (= out (get cfg/default-slot->recovery-policy slot)))))
   :classify (fn [[lookup slot] out]
               (cond (map? (lookup [:services :kg :slots slot :recovery-policy])) :config
                     (some? out) :default
                     :else :none))
   :classify-domain #{:config :default :none}
   :num-tests 200})

(defn- lookup-of [m] (fn [path] (get-in m path)))

(golden/deftest-golden resolver-chain-golden
  "test/golden/hive-mcp/m14-slots-config.edn"
  (let [conf {:services {:kg {:slots {:carto {:backend "datascript"
                                              :recovery-policy {:strategy ["truncate" :throw]}}
                                      :memory {:backend :datalevin
                                               :recovery-policy {:strategy "audit" :max-attempts 2}}
                                      :sessions {:recovery-policy "not-a-map"}}}}}
        r (cfg/->resolver (lookup-of conf))
        bare (cfg/->resolver (constantly nil) {:only :datascript})]
    {:policy (into (sorted-map)
                   (for [slot [:carto :memory :sessions :default :unknown]]
                     [slot (cfg/resolve-recovery-policy (lookup-of conf) slot)]))
     :backend (into (sorted-map)
                    (for [slot [:carto :memory :sessions :carto-vec :unknown]]
                      [slot (p/resolve-backend r slot)]))
     :custom-defaults [(p/resolve-backend bare :only) (p/resolve-backend bare :carto)
                       (p/default-mapping bare)]
     :production-default-mapping (p/default-mapping (cfg/->resolver))}))
