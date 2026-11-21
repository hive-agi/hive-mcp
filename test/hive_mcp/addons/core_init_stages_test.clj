(ns hive-mcp.addons.core-init-stages-test
  "Characterization of `init-addon!` across its stages: the registry entry
   and outcome for a succeeding, a failing and a throwing initialize!, and
   the hook routing that `register-hooks!` performs (owner-keyed namespaces
   vs the extension registry)."
  (:require [clojure.test :refer [use-fixtures]]
            [hive-addon.protocol :as proto]
            [hive-mcp.addons.core :as core]
            [hive-mcp.extensions.registry :as ext]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private ids [:stg/ok :stg/fails :stg/throws])

(defn- clean! []
  (doseq [id ids] (core/unregister-addon! id))
  (doseq [k [:stg/hook-a :stg/ext-a]] (ext/deregister! k)))

(use-fixtures :each (fn [f] (clean!) (try (f) (finally (clean!)))))

(defn- addon [id init-fn]
  (reify proto/IAddon
    (addon-id [_] id)
    (addon-type [_] :native)
    (capabilities [_] #{:tools})
    (initialize! [_ config] (init-fn config))
    (shutdown! [_] {:success? true})
    (tools [_] [])
    (schema-extensions [_] {})
    (health [_] {:status :ok})
    (excluded-tools [_] #{})
    (hooks [_] {:stg/hook-a (fn [] :hook)})))

(defn- entry-view [id]
  (-> (core/get-addon-entry id)
      (select-keys [:state :hook-keys])
      (assoc :init-time? (some? (:init-time (core/get-addon-entry id))))))

(defn- outcome-view [o]
  (cond-> (dissoc o :elapsed-ms)
    (:metadata o) (update-in [:metadata :extensions] #(some-> % keys sort vec))))

(deftest-golden init-addon-stage-outcomes
  "test/golden/addons/core-init-stages.edn"
  (let [_ (core/register-addon!
           (addon :stg/ok (fn [cfg] {:success? true
                                     :metadata {:extensions {:stg/ext-a (fn [] :ext)}}
                                     :saw-ports? (contains? cfg :runtime/ports)})))
        _ (core/register-addon! (addon :stg/fails (fn [_] {:success? false :errors ["nope"]})))
        _ (core/register-addon! (addon :stg/throws (fn [_] (throw (ex-info "kaboom" {})))))
        ok (outcome-view (core/init-addon! :stg/ok))
        again (core/init-addon! :stg/ok)
        fails (outcome-view (core/init-addon! :stg/fails))
        throws (core/init-addon! :stg/throws)]
    {:ok ok
     :ok-entry (entry-view :stg/ok)
     :again again
     :hook-registered? (some? (ext/get-extension :stg/hook-a))
     :metadata-ext-registered? (some? (ext/get-extension :stg/ext-a))
     :fails fails
     :fails-entry (entry-view :stg/fails)
     :throws (update throws :errors #(mapv (fn [m] (boolean (re-find #"kaboom" (str m)))) %))
     :throws-entry (entry-view :stg/throws)
     :unregistered (core/init-addon! :stg/never)}))
