(ns hive-mcp.channel.audience-lineage-test
  "HIVEMIND-PIGGYBACK-LEAK (kanban 20261008232949-5b0f7835).

   A coordinator session's piggyback must carry only what it owns: its own
   lineage, its project scope, and admitted broadcasts. Two pure rules carry
   that, and both are pinned here:

     root-visible?       may a ROOT-level shout reach this coordinator lane?
     peer-traffic-digest which directed conversations does this lane
                         supervise?

   The schema-synthesized facet states the root rule as a law over closed
   enumerations, so the law is a literal table and never calls the subject.
   The metamorphic facets state the property the incident broke: adding
   another session's traffic must not change what this session reads."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-schemas.test :as hst]
            [hive-test.properties :refer [defprop-metamorphic]]
            [hive-mcp.channel.audience :as aud]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; root-visible? — schema-synthesized
;; =============================================================================

(def RootCase
  [:map
   [:reader [:enum "coordinator:7-hive" "coordinator:9-hive" "coordinator-hive"]]
   [:author [:enum "coordinator:7" "coordinator:9" "team:delib-x" "wave-1-m0" "irs-b"]]
   [:project [:maybe [:enum "global" "hive"]]]
   [:opt-in :boolean]])

(defn root-visible-case
  "One-arg subject: a RootCase -> boolean."
  [{:keys [reader author project opt-in]}]
  (aud/root-visible? reader {:agent-id author :project-id project} opt-in))

(def ^:private session-of
  "Literal reader -> session table; the law reads this, not the subject."
  {"coordinator:7-hive" "7" "coordinator:9-hive" "9" "coordinator-hive" nil})

(def ^:private author-session
  {"coordinator:7" "7" "coordinator:9" "9"})

(hst/deftrifecta-from-schema root-shout-reaches-only-its-owner
  hive-mcp.channel.audience-lineage-test/root-visible-case
  {:in  RootCase
   :out :boolean
   :rel (fn [{:keys [reader author project opt-in]} out]
          (= out (boolean
                  (or (nil? (session-of reader)) (= (session-of reader) (author-session author)) (and (nil? (author-session author)) (or (= "hive" project) opt-in))))))
   :mutation false
   :num-tests 300})

;; =============================================================================
;; Metamorphic — another session's traffic never changes this session's read
;; =============================================================================

(def ^:private gen-own-msg
  "A shout in coordinator:7's lineage or root scope."
  (gen/let [agent (gen/elements ["ling-a" "ling-b" "coordinator:7"])
            kind  (gen/elements [:child :root :directed])
            ctx   (gen/elements ["c1" "c2"])
            ts    gen/nat]
    (case kind
      :child    {:agent-id agent :parent-id "coordinator:7" :project-id "global" :timestamp ts}
      :root     {:agent-id "coordinator:7" :project-id "global" :timestamp ts}
      :directed {:agent-id agent :parent-id "coordinator:7" :to "ling-z"
                 :context-id ctx :timestamp ts})))

(def ^:private gen-foreign-msg
  "Traffic coordinator:7 does not own: another session's lings, an unowned
   team runner, a parentless wave member, a foreign directed exchange."
  (gen/let [kind (gen/elements [:child :team :orphan :directed])
            ctx  (gen/elements ["x1" "x2"])
            ts   gen/nat]
    (case kind
      :child    {:agent-id "irs-b" :parent-id "coordinator:t300b" :project-id "global" :timestamp ts}
      :team     {:agent-id "team:delib-x" :project-id "global" :timestamp ts}
      :orphan   {:agent-id "wave-1-m0" :timestamp ts}
      :directed {:agent-id "irs-d4" :parent-id "coordinator:t300b" :to "irs-x"
                 :context-id ctx :timestamp ts})))

(def ^:private gen-read
  (gen/let [own     (gen/vector gen-own-msg 0 12)
            foreign (gen/vector gen-foreign-msg 1 12)]
    {:reader "coordinator:7-hive" :msgs own :foreign foreign}))

(defn- with-foreign
  "Mix the foreign traffic into the read."
  [{:keys [msgs foreign] :as x}]
  (assoc x :msgs (vec (sort-by :timestamp (concat msgs foreign)))))

(defn read-rows
  "What the reader receives: addressed rows + peer-traffic rows."
  [{:keys [reader msgs]}]
  {:rows (mapv #(select-keys % [:agent-id :timestamp :to])
               (aud/filter-messages reader (sort-by :timestamp msgs)))
   :peer (aud/peer-traffic-digest reader msgs)})

(defprop-metamorphic foreign-traffic-never-reaches-a-session
  read-rows
  with-foreign
  =
  gen-read
  {:num-tests 300})

(defprop-metamorphic peer-digest-ignores-arrival-order
  (fn [{:keys [reader msgs]}] (set (aud/peer-traffic-digest reader msgs)))
  (fn [x] (update x :msgs (comp vec reverse)))
  =
  gen-read
  {:num-tests 200})

;; =============================================================================
;; The incident, replayed
;; =============================================================================

(deftest measured-leak-rows-reach-no-foreign-session-test
  (testing "rows measured on the live coordinator 2026-10-08 (30 leaked rows,
            all parentless): none reaches an unrelated session"
    (let [leaked [{:agent-id "team:delib-constitutional-review-1791512672794"
                   :project-id "global" :event-type :progress}
                  {:agent-id "irs-b" :project-id "global"}
                  {:agent-id "coordinator:t300b" :project-id "global"}
                  {:agent-id "wave-da4ea70d-m0" :parent-id "wave-da4ea70d"
                   :project-id "global"}]]
      (is (= [] (aud/filter-messages "coordinator:246159-hive-mcp" leaked)))
      (is (= [{:agent-id "coordinator:t300b" :project-id "global"}]
             (aud/filter-messages "coordinator:t300b-hive" leaked))
          "the author session still sees its own root shout")))
  (testing "explicit broadcasts and directed messages are unchanged"
    (is (aud/addressed-to? "coordinator:1-hive"
                           {:agent-id "team:x" :project-id "global" :broadcast? true}))
    (is (aud/addressed-to? "coordinator:1-hive"
                           {:agent-id "team:x" :to "coordinator:1"}))))
