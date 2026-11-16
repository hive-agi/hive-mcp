(ns hive-mcp.protocols.reexport-late-extend-test
  "Every hive-mcp re-export of a protocol METHOD reaches an impl registered by
   map-based `extend` AFTER the re-exporting namespace loaded.

   A `(def m ports/m)` alias captures the method fn as it was at load time;
   `extend` rebuilds the method behind the var, so the alias keeps the stale
   dispatch cache and throws `No implementation of method` for the late impl
   (the 2026-09-17 proximum incident, memory 20260728135559-5c7368e6). A `defn`
   delegate reads the var at call time. Kanban 20260917114708-1178ca7a."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-addon.vessel :as vessel-port]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.embeddings.protocol :as emb]
            [hive-mcp.protocols.kg :as kg]
            [hive-mcp.protocols.lifecycle :as lifecycle]
            [hive-mcp.protocols.vessel :as vessel]
            [hive-spi.embeddings.ports :as emb-ports]
            [hive-spi.kg.protocol :as kg-spi]
            [hive-spi.lifecycle.ports :as lc-ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord LateImpl [])

(defn- late [& _] ::late)

(defn- all [ks] (zipmap ks (repeat late)))

(extend LateImpl
  emb-ports/EmbeddingProvider (all [:embed-text :embed-batch :embedding-dimension])
  lc-ports/IShutdownHook      (all [:shutdown-priority :shutdown-name :shutdown!])
  lc-ports/ISweepable         (all [:sweep-interval-s :sweep-name :sweep!])
  lc-ports/IResourceOwner     (all [:owner-id :owned-resources :release-all!])
  lc-ports/IShutdownBudget    (all [:shutdown-timeout-ms])
  vessel-port/IVessel         (all [:vessel-id :capabilities :resolve-context :addon
                                    :initialize! :shutdown!])
  kg-spi/IKGStore             (all [:ensure-conn! :db-snapshot :transact! :reset-conn! :entid
                                    :query :entity :eids-by-attr :close! :pull-entity])
  kg-spi/IPersistentKGStore   (all [:delete-database!])
  kg-spi/ITemporalKGStore     (all [:history-db :as-of-db :since-db :query-history
                                    :query-as-of]))

(def ^:private s (->LateImpl))

(def re-exports
  "[label fn args] for every re-exported protocol method this card names."
  [["embeddings.protocol/embed-text"          emb/embed-text [s "t"]]
   ["embeddings.protocol/embed-batch"         emb/embed-batch [s ["t"]]]
   ["embeddings.protocol/embedding-dimension" emb/embedding-dimension [s]]
   ["embeddings.active/embedding-dimension"   active/embedding-dimension [s]]
   ["lifecycle/shutdown-priority"             lifecycle/shutdown-priority [s]]
   ["lifecycle/shutdown-name"                 lifecycle/shutdown-name [s]]
   ["lifecycle/shutdown!"                     lifecycle/shutdown! [s {}]]
   ["lifecycle/sweep-interval-s"              lifecycle/sweep-interval-s [s]]
   ["lifecycle/sweep-name"                    lifecycle/sweep-name [s]]
   ["lifecycle/sweep!"                        lifecycle/sweep! [s {}]]
   ["lifecycle/owner-id"                      lifecycle/owner-id [s]]
   ["lifecycle/owned-resources"               lifecycle/owned-resources [s]]
   ["lifecycle/release-all!"                  lifecycle/release-all! [s]]
   ["lifecycle/shutdown-timeout-ms"           lifecycle/shutdown-timeout-ms [s]]
   ["vessel/vessel-id"                        vessel/vessel-id [s]]
   ["vessel/capabilities"                     vessel/capabilities [s]]
   ["vessel/resolve-context"                  vessel/resolve-context [s "agent"]]
   ["vessel/addon"                            vessel/addon [s :terminal]]
   ["vessel/initialize!"                      vessel/initialize! [s {}]]
   ["vessel/shutdown!"                        vessel/shutdown! [s]]
   ["kg/ensure-conn!"                         kg/ensure-conn! [s]]
   ["kg/reset-conn!"                          kg/reset-conn! [s]]
   ["kg/close!"                               kg/close! [s]]
   ["kg/delete-database!"                     kg/delete-database! [s true]]
   ["kg/history-db"                           kg/history-db [s]]
   ["kg/as-of-db"                             kg/as-of-db [s 1]]
   ["kg/since-db"                             kg/since-db [s 1]]])

(deftest re-exports-dispatch-to-impls-extended-after-the-ns-loaded
  (testing "the table is not vacuous"
    (is (= 27 (count re-exports))))
  (doseq [[label f args] re-exports]
    (testing label
      (is (= ::late (try (apply f args)
                         (catch IllegalArgumentException e (ex-message e))))
          label))))
