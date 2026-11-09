(ns hive-mcp.server.routes.session-start-key-test
  "The request middleware records a session start under the SAME key a wrap
   later reads it by (`crystal/resolve-agent`, i.e. `ctx/session-agent-id`)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.server.routes.middleware :as mw]
            [hive-mcp.spi.session :as session]
            [hive-mcp.tools.crystal :as tc]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private recorded (atom []))

(def ^:private recording-impl
  (reify session/ISessionLifecycle
    (-record-session-start! [_ agent-id] (swap! recorded conj agent-id) nil)
    (-register-session-hooks! [_ _] nil)))

(use-fixtures :each
  (fn [f]
    (reset! recorded [])
    (session/install! recording-impl)
    (try (f) (finally (session/uninstall!)))))

(defn- call-through-context
  "Run ARGS through wrap-handler-context and answer the agent a wrap would
   resolve inside that request."
  [args]
  (let [seen (atom nil)
        h    (mw/wrap-handler-context
              (fn [a] (reset! seen (tc/resolve-agent a)) [{:type "text" :text "ok"}]))]
    (h args)
    @seen))

(deftest start-key-matches-wrap-agent
  (doseq [[label args] [["coordinator role with caller id"
                         {:command "wrap" :agent_id "coordinator" :_caller_id "coordinator:345563"}]
                        ["no agent id, caller id only"
                         {:command "wrap" :_caller_id "coordinator:345563"}]
                        ["specific ling id"
                         {:command "wrap" :agent_id "ling-7" :_caller_id "ling-7"}]]]
    (testing label
      (reset! recorded [])
      (let [wrap-agent (call-through-context args)]
        (is (= [wrap-agent] @recorded))))))

(deftest coordinator-sessions-do-not-share-a-start
  (call-through-context {:agent_id "coordinator" :_caller_id "coordinator:a"})
  (call-through-context {:agent_id "coordinator" :_caller_id "coordinator:b"})
  (is (= ["coordinator:a" "coordinator:b"] @recorded)))
