(ns hive-mcp.events.handlers.crystal-contract-test
  "Contract facets for the wrap/crystallize handlers: the producer->consumer
   stats projection, the wrap notification built on it, and the wrap request."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.crystal :as crystal]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:string {:min 1 :max 10}])
(def ^:private Count [:int {:min 0 :max 9}])
(def ^:private MemoryType [:enum "decision" "convention" "note" "axiom" "custom-kind"])

(def ^:private Summary
  "A harvest summary or a consumer-shaped stats map, as the two producers send it."
  [:maybe
   [:map
    [:created-by-type {:optional true} [:map-of MemoryType [:int {:min 1 :max 9}]]]
    [:decisions {:optional true} Count]
    [:conventions {:optional true} Count]
    [:notes {:optional true} Count]
    [:created-count {:optional true} [:maybe Count]]
    [:kg-edge-count {:optional true} [:maybe Count]]
    [:kanban-closed-transitions {:optional true} [:maybe Count]]]])

(def ^:private type->bucket
  {"decision" :decisions "convention" :conventions "note" :notes "axiom" :axioms})

(defn- expected-bucket
  "The count the consumer should read for bucket K: a per-type count when the
   summary carries one, else the pre-set count, else 0."
  [summary k]
  (let [typed (reduce-kv (fn [acc t c] (update acc (get type->bucket t :other) (fnil + 0) c))
                         {} (:created-by-type summary {}))]
    (get typed k (get summary k 0))))

(deftrifecta-from-schema project-wrap-stats-reconciles-the-vocabulary
  #'crystal/project-wrap-stats
  {:in [:cat Summary]
   :out [:map
         [:decisions :int] [:conventions :int] [:notes :int] [:axioms :int] [:other :int]
         [:principles [:= 0]] [:snippets [:= 0]] [:plans [:= 0]] [:knowledge [:= 0]]
         [:kanban-closures :int] [:kg-edges :int] [:memories-created :int]]
   :rel (fn [[summary] out]
          (let [m (or summary {})]
            (and (every? #(= (expected-bucket m %) (get out %))
                         [:decisions :conventions :notes :axioms :other])
                 (= (or (:kanban-closed-transitions m) 0) (:kanban-closures out))
                 (= (or (:kg-edge-count m) 0) (:kg-edges out))
                 (= (or (:created-count m) 0) (:memories-created out)))))
   :golden-path "test/golden/hive-mcp/events/handlers/project-wrap-stats.edn"
   :num-tests 200})

(deftrifecta-from-schema wrap-notify-counts-the-written-entries
  #'crystal/handle-crystal-wrap-notify
  {:in [:cat [:map {:closed true}]
        [:tuple [:= :crystal/wrap-notify]
         [:map [:agent-id Id] [:session-id Id] [:project-id Id]
          [:created-ids [:vector [:maybe [:enum "m-1" "m-2" "m-3"]]]]
          [:stats Summary]]]]
   :out [:map
         [:log [:map [:level [:= :info]] [:message :string]]]
         [:wrap-notify [:map [:agent-id Id] [:session-id Id] [:project-id Id]
                        [:stats [:map [:wrapped [:int {:min 0 :max 3}]] [:decisions :int]]]]]
         [:shout [:map [:agent-id Id] [:event-type [:= :wrap_notify]]
                  [:data [:map [:message [:re #"^Session wrapped: .*"]]]]]]]
   :rel (fn [[_ [_ {:keys [created-ids]}]] out]
          (let [n (count (distinct (remove nil? created-ids)))]
            (and (= n (get-in out [:wrap-notify :stats :wrapped]))
                 (= (get-in out [:wrap-notify :stats]) (get-in out [:shout :data :stats]))
                 (str/includes? (get-in out [:shout :data :message]) (str "[wrote " n " ")))))
   :golden-path "test/golden/hive-mcp/events/handlers/crystal-wrap-notify.edn"
   :num-tests 100})

(def ^:private Items [:vector {:max 3} Id])

;; No golden: the session id and the summary note carry today's date.
(deftrifecta-from-schema wrap-request-counts-every-bucket
  #'crystal/handle-crystal-wrap-request
  {:in [:cat [:map [:agent-context [:map [:agent-id Id]]]]
        [:tuple [:= :crystal/wrap-request]
         [:map [:accomplishments Items] [:decisions Items] [:conventions Items]
          [:principles {:optional true} Items] [:completed-tasks {:optional true} Items]
          [:project {:optional true} Id]]]]
   :out [:map
         [:log [:map [:level [:= :info]] [:message :string]]]
         [:memory-write [:map [:type [:= "note"]] [:content [:re #"^## Session Summary: .*"]]]]
         [:wrap-notify [:map [:agent-id Id] [:session-id [:re #"^session:.*"]]
                        [:stats [:map-of :keyword [:int {:min 0 :max 3}]]]]]
         [:shout [:map [:agent-id Id] [:event-type [:= :completed]] [:message :string]]]]
   :rel (fn [[cofx [_ {:keys [accomplishments decisions conventions principles completed-tasks]}]] out]
          (let [stats (get-in out [:wrap-notify :stats])]
            (and (= (get-in cofx [:agent-context :agent-id]) (get-in out [:wrap-notify :agent-id]))
                 (= [(count accomplishments) (count decisions) (count conventions)
                     (count principles) (count completed-tasks)]
                    (mapv stats [:accomplishments :decisions :conventions :principles :kanban-closures]))
                 (str/starts-with? (get-in out [:shout :message])
                                   (str "Session wrapped: " (count decisions) " decisions, "
                                        (count conventions) " conventions")))))
   :num-tests 100})

(deftest register-handlers-installs-the-crystal-events
  (ev/with-clean-registry
    (let [reg (crystal/register-handlers!)]
      (is (every? #(contains? reg %) [:crystal/wrap-request :crystal/wrap-notify]))
      (is (every? ev/handler-registered? [:crystal/wrap-request :crystal/wrap-notify])))))
