(ns hive-mcp.events.handlers.kg-contract-test
  "Contract facets for the Knowledge Graph event handlers: generated edge
   events through the contracted handlers, goldens over the seeded cases and
   the schema mutants of each effects map."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.kg :as kg]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:string {:min 1 :max 10}])
(def ^:private NoCoeffects [:map {:closed true}])
(def ^:private Relation [:enum :implements :refines :depends-on :supersedes])
(def ^:private Confidence [:double {:min 0.0 :max 1.0}])
(def ^:private InfoLog [:map [:level [:= :info]] [:message :string]])

(deftrifecta-from-schema kg-edge-created-publishes-the-edge
  #'kg/handle-kg-edge-created
  {:in [:cat NoCoeffects
        [:tuple [:= :kg/edge-created]
         [:map [:edge-id Id] [:from Id] [:to Id] [:relation Relation]
          [:scope {:optional true} Id]
          [:confidence {:optional true} Confidence]
          [:created-by {:optional true} Id]]]]
   :out [:map
         [:log InfoLog]
         [:channel-publish
          [:map [:event-type [:= :kg-change]]
           [:data [:map [:action [:= :created]] [:edge-id Id] [:from Id] [:to Id]
                   [:relation Relation] [:scope [:maybe Id]] [:confidence Confidence]
                   [:created-by [:maybe Id]]]]]]]
   :rel (fn [[_ [_ {:keys [edge-id from to relation scope confidence created-by]}]] out]
          (and (= {:action :created :edge-id edge-id :from from :to to :relation relation
                   :scope scope :confidence (or confidence 1.0) :created-by created-by}
                  (get-in out [:channel-publish :data]))
               (str/includes? (get-in out [:log :message]) (str "[id: " edge-id "]"))))
   :golden-path "test/golden/hive-mcp/events/handlers/kg-edge-created.edn"
   :num-tests 100})

(defn- direction [delta]
  (cond (nil? delta) "confidence changed"
        (pos? delta) "confidence increased"
        (neg? delta) "confidence decreased"
        :else "confidence unchanged"))

(deftrifecta-from-schema kg-edge-updated-reports-the-delta
  #'kg/handle-kg-edge-updated
  {:in [:cat NoCoeffects
        [:tuple [:= :kg/edge-updated]
         [:map [:edge-id Id]
          [:old-confidence {:optional true} Confidence]
          [:new-confidence Confidence]
          [:reason {:optional true} Id]
          [:updated-by {:optional true} Id]]]]
   :out [:map
         [:log InfoLog]
         [:channel-publish
          [:map [:event-type [:= :kg-change]]
           [:data [:map [:action [:= :updated]] [:edge-id Id]
                   [:old-confidence [:maybe Confidence]] [:new-confidence Confidence]
                   [:delta [:maybe :double]]
                   [:reason [:maybe Id]] [:updated-by [:maybe Id]]]]]]]
   :rel (fn [[_ [_ {:keys [old-confidence new-confidence]}]] out]
          (let [delta (when old-confidence (- new-confidence old-confidence))]
            (and (= delta (get-in out [:channel-publish :data :delta]))
                 (str/includes? (get-in out [:log :message]) (direction delta)))))
   :golden-path "test/golden/hive-mcp/events/handlers/kg-edge-updated.edn"
   :num-tests 100})

(deftrifecta-from-schema kg-edge-removed-publishes-the-removal
  #'kg/handle-kg-edge-removed
  {:in [:cat NoCoeffects
        [:tuple [:= :kg/edge-removed]
         [:map [:edge-id Id]
          [:from {:optional true} Id] [:to {:optional true} Id]
          [:relation {:optional true} Relation]
          [:reason {:optional true} Id] [:removed-by {:optional true} Id]]]]
   :out [:map
         [:log InfoLog]
         [:channel-publish
          [:map [:event-type [:= :kg-change]]
           [:data [:map [:action [:= :removed]] [:edge-id Id]
                   [:from [:maybe Id]] [:to [:maybe Id]] [:relation [:maybe Relation]]
                   [:reason [:maybe Id]] [:removed-by [:maybe Id]]]]]]]
   :rel (fn [[_ [_ {:keys [edge-id from to relation reason removed-by]}]] out]
          (and (= {:action :removed :edge-id edge-id :from from :to to :relation relation
                   :reason reason :removed-by removed-by}
                  (get-in out [:channel-publish :data]))
               (str/starts-with? (get-in out [:log :message]) (str "KG edge removed: " edge-id))))
   :golden-path "test/golden/hive-mcp/events/handlers/kg-edge-removed.edn"
   :num-tests 100})

(deftrifecta-from-schema kg-node-promoted-shouts-the-edge-count
  #'kg/handle-kg-node-promoted
  {:in [:cat NoCoeffects
        [:tuple [:= :kg/node-promoted]
         [:map [:node-id Id] [:from-scope Id] [:to-scope Id]
          [:edges-promoted {:optional true} [:vector Id]]
          [:promoted-by {:optional true} Id]]]]
   :out [:map
         [:log InfoLog]
         [:channel-publish [:map [:event-type [:= :kg-promotion]] [:data :map]]]
         [:shout [:map [:agent-id :string] [:event-type [:= :kg-promotion]] [:data [:map [:node-id Id] [:from-scope Id] [:to-scope Id]
                          [:edge-count [:int {:min 0}]]]]]]]
   :rel (fn [[_ [_ {:keys [node-id from-scope to-scope edges-promoted promoted-by]}]] out]
          (and (= (count edges-promoted) (get-in out [:shout :data :edge-count]))
               (= (or promoted-by "coordinator") (get-in out [:shout :agent-id]))
               (= {:node-id node-id :from-scope from-scope :to-scope to-scope
                   :edges-promoted edges-promoted :promoted-by promoted-by}
                  (get-in out [:channel-publish :data]))))
   :golden-path "test/golden/hive-mcp/events/handlers/kg-node-promoted.edn"
   :num-tests 100})

(deftrifecta-from-schema kg-edges-batch-created-aggregates-by-relation
  #'kg/handle-kg-edges-batch-created
  {:in [:cat NoCoeffects
        [:tuple [:= :kg/edges-batch-created]
         [:map [:edges [:vector [:map [:edge-id Id] [:relation Relation]]]]
          [:scope {:optional true} Id]
          [:created-by {:optional true} Id]]]]
   :out [:map
         [:log InfoLog]
         [:channel-publish
          [:map [:event-type [:= :kg-batch-change]]
           [:data [:map [:action [:= :batch-created]] [:count [:int {:min 0}]]
                   [:by-relation [:map-of Relation [:int {:min 1}]]]
                   [:scope [:maybe Id]] [:created-by [:maybe Id]]]]]]]
   :rel (fn [[_ [_ {:keys [edges]}]] out]
          (and (= (count edges) (get-in out [:channel-publish :data :count]))
               (= (frequencies (map :relation edges))
                  (get-in out [:channel-publish :data :by-relation]))))
   :golden-path "test/golden/hive-mcp/events/handlers/kg-edges-batch-created.edn"
   :num-tests 100})

(deftest register-handlers-installs-the-kg-events
  (ev/with-clean-registry
    (let [events [:kg/edge-created :kg/edge-updated :kg/edge-removed
                  :kg/node-promoted :kg/edges-batch-created]
          reg (kg/register-handlers!)]
      (is (every? #(contains? reg %) events))
      (is (every? ev/handler-registered? events)))))
