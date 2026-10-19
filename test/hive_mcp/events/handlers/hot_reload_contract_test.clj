(ns hive-mcp.events.handlers.hot-reload-contract-test
  "Contract facets for the hot-reload event handlers. Every generated event
   carries its own :timestamp, so the goldens never read the clock."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.hot-reload :as hot-reload]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private NoCoeffects [:map {:closed true}])
(def ^:private Trigger [:enum :file-change :manual :flush])
(def ^:private InfoLog [:map [:level [:= :info]] [:message :string]])

(deftrifecta-from-schema hot-reload-start-labels-the-trigger
  #'hot-reload/handle-hot-reload-start
  {:in [:cat NoCoeffects
        [:tuple [:= :hot/reload-start]
         [:map [:dirs {:optional true} [:vector [:enum "src" "test" "dev"]]]
          [:trigger {:optional true} Trigger]
          [:timestamp inst?]]]]
   :out [:map
         [:channel-publish [:map [:event [:= :hot-reload-start]]
                            [:data [:map [:dirs [:vector :string]] [:trigger :keyword]
                                    [:timestamp inst?]]]]]
         [:log InfoLog]
         [:prometheus [:map [:counter [:= :hot_reload_started]]
                       [:labels [:map [:trigger [:enum "file-change" "manual" "flush" "unknown"]]]]]]]
   :rel (fn [[_ [_ {:keys [dirs trigger timestamp]}]] out]
          (and (= {:dirs (or dirs []) :trigger (or trigger :unknown) :timestamp timestamp}
                  (get-in out [:channel-publish :data]))
               (= (name (or trigger :unknown)) (get-in out [:prometheus :labels :trigger]))))
   :golden-path "test/golden/hive-mcp/events/handlers/hot-reload-start.edn"
   :num-tests 100})

(deftrifecta-from-schema hot-reload-success-records-the-duration
  #'hot-reload/handle-hot-reload-success
  {:in [:cat NoCoeffects
        [:tuple [:= :hot/reload-success]
         [:map [:loaded [:vector :symbol]]
          [:unloaded {:optional true} [:vector :symbol]]
          [:ms {:optional true} [:int {:min 0 :max 60000}]]
          [:timestamp inst?]]]]
   :out [:map
         [:channel-publish [:map [:event [:= :hot-reload-success]]
                            [:data [:map [:loaded [:vector :symbol]] [:unloaded [:vector :symbol]]
                                    [:duration-ms [:maybe :int]] [:timestamp inst?]]]]]
         [:log InfoLog]
         [:prometheus [:map [:counter [:= :hot_reload_success]]
                       [:labels [:map {:closed true}]]
                       [:histogram [:map [:name [:= :hot_reload_duration_seconds]]
                                    [:value [:maybe :double]]]]]]
         [:tool-registry-refresh [:= true]]]
   :rel (fn [[_ [_ {:keys [loaded unloaded ms]}]] out]
          (and (= (when ms (/ ms 1000.0)) (get-in out [:prometheus :histogram :value]))
               (= (str "Hot-reload success: " (count loaded) " loaded, " (count unloaded) " unloaded"
                       (when ms (str " in " ms "ms")))
                  (get-in out [:log :message]))
               (= (or unloaded []) (get-in out [:channel-publish :data :unloaded]))))
   :golden-path "test/golden/hive-mcp/events/handlers/hot-reload-success.edn"
   :num-tests 100})

(deftrifecta-from-schema file-changed-labels-the-event-type
  #'hot-reload/handle-file-changed
  {:in [:cat NoCoeffects
        [:tuple [:= :file/changed]
         [:map [:file [:enum "/p/src/a.clj" "/p/test/b.clj"]]
          [:event-type {:optional true} [:enum :modify :create :delete]]
          [:timestamp inst?]]]]
   :out [:map
         [:channel-publish [:map [:event [:= :file-changed]]
                            [:data [:map [:file :string] [:event-type :keyword] [:timestamp inst?]]]]]
         [:log [:map [:level [:= :debug]] [:message :string]]]
         [:prometheus [:map [:counter [:= :file_changed]]
                       [:labels [:map [:event_type [:enum "modify" "create" "delete"]]]]]]]
   :rel (fn [[_ [_ {:keys [file event-type timestamp]}]] out]
          (and (= {:file file :event-type (or event-type :modify) :timestamp timestamp}
                  (get-in out [:channel-publish :data]))
               (= (name (or event-type :modify)) (get-in out [:prometheus :labels :event_type]))))
   :golden-path "test/golden/hive-mcp/events/handlers/file-changed.edn"
   :num-tests 100})

(deftest register-handlers-installs-the-hot-reload-events
  (ev/with-clean-registry
    (let [events [:hot/reload-start :hot/reload-success :file/changed]
          reg (hot-reload/register-handlers!)]
      (is (every? #(contains? reg %) events))
      (is (every? ev/handler-registered? events)))))
