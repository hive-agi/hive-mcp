(ns hive-mcp.tools.agent.helpers-contract-test
  "Contract facets for the agent response helpers: a trifecta over
   format-agents (one formatted agent per non-nil row, counts and
   frequencies consistent with the agents), a trifecta over
   merge-with-elisp-lings with the elisp rows and the liveness probe
   injected (DataScript rows win, every surviving elisp row is reconciled),
   and a golden over format-agent."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.agent.helpers :as h]
            [hive-mcp.tools.agent.reconcile :as rc]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Row
  [:map
   [:slave/id [:enum "a" "b" "c"]]
   [:slave/status [:enum :idle :working]]
   [:slave/depth [:enum 0 1 2]]
   [:slave/parent {:optional true} [:enum "coordinator:s1"]]])

(deftrifecta-from-schema format-agents-counts-what-it-formats
  #'h/format-agents
  {:in [:cat [:vector {:max 5} [:maybe Row]]]
   :out h/FormattedAgents
   :rel (fn [[rows] out]
          (let [live (remove nil? rows)]
            (and (= (count live) (:count out) (count (:agents out)))
                 (= (map :slave/id live) (map :id (:agents out)))
                 (= (frequencies (map :slave/status live)) (:by-status out))
                 (= (count live) (reduce + 0 (vals (:by-type out)))))))
   :classify (fn [[rows] _] (if (some nil? rows) :with-nil :all-rows))
   :classify-domain #{:with-nil :all-rows}
   :classify-floor 10
   :golden-path "test/golden/hive-mcp/tools/agent/helpers-format-agents.edn"
   :num-tests 100})

(def ^:private ElispRow
  [:map [:slave/id [:enum "a" "b" "c" "d"]] [:slave/status [:enum :idle :working]]])

(defn- merged-ids
  "Ids merge-with-elisp-lings answers for DS rows and ELISP rows, with no
   agent registered or heard and none retired."
  [ds elisp]
  (map :slave/id (h/merge-with-elisp-lings ds {:elisp-lings elisp :probe (rc/->known-agents #{} #{})})))

;; No :golden-path: the input holds a KnownAgents probe, which reads back
;; from EDN as a plain map; format-agent-golden pins a literal merge.
(deftrifecta-from-schema merge-keeps-datascript-and-appends-elisp-only
  #'h/merge-with-elisp-lings
  {:in [:cat [:vector {:max 3} ElispRow]
        [:map
         [:elisp-lings [:vector {:max 3} ElispRow]]
         [:probe [:fn {:gen/elements [(rc/->known-agents #{} #{})]} #(satisfies? rc/ILivenessEvidence %)]]]]
   :out [:sequential rc/AgentRow]
   :rel (fn [[ds {elisp :elisp-lings}] out]
          (let [ds-ids (set (map :slave/id ds))]
            (= (concat (map :slave/id ds) (remove ds-ids (map :slave/id elisp)))
               (map :slave/id out))))
   :classify (fn [[ds {elisp :elisp-lings}] _]
               (if (some (set (map :slave/id ds)) (map :slave/id elisp)) :overlap :disjoint))
   :classify-domain #{:overlap :disjoint}
   :classify-floor 10
   :num-tests 100})

(golden/deftest-golden format-agent-golden
  "test/golden/hive-mcp/tools/agent/helpers-format-agent.edn"
  {:nil      (h/format-agent nil)
   :bare     (h/format-agent {:slave/id "a" :slave/depth 1})
   :full     (h/format-agent {:slave/id "a" :slave/status :orphaned :slave/depth 1 :slave/cwd "/w"
                              :slave/project-id "p" :slave/parent "c" :slave/presets ["tdd"]
                              :slave/orphan-reason "r" :slave/last-event-at "t"
                              :slave/liveness-age-ms 0})
   :merge-ds (merged-ids [{:slave/id "a"}] [{:slave/id "a"} {:slave/id "b"}])})

(deftest generated-ids-carry-their-type-prefix
  (is (re-matches #"ling-[0-9a-f-]{36}" (h/generate-agent-id :ling)))
  (is (not= (h/generate-agent-id :ling) (h/generate-agent-id :ling)))
  (is (nil? (h/query-elisp-lings)) "no swarm addon in the unit JVM: unavailable, not empty"))
