(ns hive-mcp.tools.agent.dag-contract-test
  "Contract facets for the dag compat shim. Every handler delegates to
   hive-agent through requiring-resolve; hive-agent is not on the unit
   classpath, so the shim's contract here is that it refuses LOUDLY with an
   ex-info naming the missing addon and the var, never a silent nil. The
   trifecta drives the three handlers through that refusal; a golden pins
   the messages."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.agent.dag :as dag]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private handlers
  {:start  #'dag/handle-dag-start
   :status #'dag/handle-dag-status
   :stop   #'dag/handle-dag-stop})

(defn- refusal
  "The ex-message a dag handler throws, or the response it answered."
  [f params]
  (try (f params) (catch clojure.lang.ExceptionInfo e (ex-message e))))

(defn- dag-refusal
  "Drive the dag handler named OP with PARAMS; the refusal message."
  [op params]
  (refusal (handlers op) params))

;; The subject is a thin dispatcher over the three contracted shims: each
;; one throws by design without hive-agent, so the facet is the message.
(deftrifecta-from-schema dag-shims-refuse-loudly-without-hive-agent
  dag-refusal
  {:in [:cat [:enum :start :status :stop]
        [:map [:plan_id {:optional true} [:enum "p1"]] [:cwd {:optional true} [:enum "/w"]]]]
   :out :string
   :rel (fn [[op _] out]
          (and (str/includes? out "hive-agent")
               (str/includes? out (str "hive-agent.swarm.tools.agent.dag/handle-dag-" (name op)))))
   :golden-path "test/golden/hive-mcp/tools/agent/dag-refusal.edn"
   :num-tests 30})

(golden/deftest-golden dag-refusal-golden
  "test/golden/hive-mcp/tools/agent/dag-messages.edn"
  {:start  (refusal dag/handle-dag-start {:plan_id "p" :cwd "/w"})
   :status (refusal dag/handle-dag-status {})
   :stop   (refusal dag/handle-dag-stop nil)})

(deftest shims-are-variadic-delegates
  (is (= '([{:keys [plan_id cwd max_slots presets project_id]}])
         (:arglists (meta #'dag/handle-dag-start)))))
