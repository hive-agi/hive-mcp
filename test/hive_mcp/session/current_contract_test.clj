(ns hive-mcp.session.current-contract-test
  "Contract facets for the session adapter. Every subject is driven with an
   explicit env map or world snapshot, so nothing reads the swarm store and
   the goldens leave out the one field the process env supplies
   (:session/agent-id, from CLAUDE_SWARM_SLAVE_ID)."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.session.current :as cur]
            [hive-mcp.session.identity :as sid]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private EnvKey [:enum "HIVE_SESSION_ID" "CLAUDE_SESSION_ID" "CLAUDE_SWARM_SLAVE_ID" "PATH"])

(def ^:private host-keys ["HIVE_SESSION_ID" "CLAUDE_SESSION_ID" "CLAUDE_SWARM_SLAVE_ID"])

(deftrifecta-from-schema host-session-id-takes-the-first-non-blank
  #'cur/host-session-id
  {:in [:cat [:map-of EnvKey [:maybe [:or [:enum "" "id-1" "id-2"] :int]]]]
   :out [:maybe :string]
   :rel (fn [[env] out]
          (= out (some (fn [k] (let [v (get env k)] (when (and (string? v) (seq v)) v)))
                       host-keys)))
   :classify (fn [_ out] (if out :found :none))
   :classify-domain #{:found :none}
   :num-tests 200})

(def ^:private Sid [:enum "s-a" "s-b" "s-c"])

(def ^:private TWorld
  [:map
   [:slaves [:map-of [:enum "a" "b" "c"]
             [:map
              [:slave/id [:enum "a" "b" "c"]]
              [:slave/parent-id [:maybe [:enum "a" "b" "c"]]]
              [:slave/depth [:enum 0 1]]
              [:slave/session-id [:maybe Sid]]]]]
   [:coordinators [:= {} {}]]])

(deftrifecta-from-schema parent-of-maps-child-session-to-parent-session
  #'cur/parent-of
  {:in [:cat TWorld]
   :out [:map-of :string :string]
   :rel (fn [[{:keys [slaves]}] out]
          (and (every? (fn [[child parent]]
                         (some (fn [[_ s]]
                                 (and (= child (:slave/session-id s))
                                      (= parent (some-> (:slave/parent-id s) slaves :slave/session-id))))
                               slaves))
                       out)
               (not-any? (fn [[c p]] (= c p)) out)))
   :num-tests 200})

(def ^:private world
  {:slaves {"coord-a" {:slave/id "coord-a" :slave/depth 0
                       :slave/session-id "s-a" :slave/project-id "hive"}
            "ling-a1" {:slave/id "ling-a1" :slave/depth 1 :slave/parent-id "coord-a"
                       :slave/session-id "s-a1" :slave/project-id "hive"}}
   :coordinators {"coord-a" {:coordinator/id "coord-a" :coordinator/session-id "s-a"
                             :coordinator/project "hive" :coordinator/status :active}}})

(golden/deftest-golden session-ref-over-an-explicit-world-golden
  "test/golden/hive-mcp/session/current-session-ref.edn"
  (into (sorted-map)
        (for [[k opts] {:adopted  {:world world :project-id "hive" :session-id "s-ed"}
                        :loose    {:world world :project-id "other" :session-id "s-ed"}
                        :no-world {:world {:slaves {} :coordinators {}} :session-id "s-ed"}
                        ;; BUG (pinned): a blank id is not replaced by the
                        ;; per-process one, so the "always valid" ref is not
                        :blank-id {:world {:slaves {} :coordinators {}} :session-id ""}}]
          [k (dissoc (cur/session-ref opts) :session/agent-id)])))

(golden/deftest-golden parent-lookups-golden
  "test/golden/hive-mcp/session/current-parent-of.edn"
  (let [f (cur/parent-of-fn world)]
    {:parent-of (cur/parent-of world)
     :fn        (into (sorted-map) (for [s ["s-a" "s-a1" "s-x"]] [s (f s)]))}))

(deftest the-no-arg-arities-answer-their-contracted-shapes
  (is (m/validate cur/WorldSnapshot (cur/world-snapshot)))
  (is (m/validate sid/NormalizedRef (cur/session-ref)))
  (is (string? (cur/session-id)) "the per-process id fills in when nothing else does")
  (is (map? (cur/parent-of)))
  (is (nil? ((cur/parent-of-fn) "s-never")))
  (is (m/validate [:maybe :string] (cur/agent-id))))
