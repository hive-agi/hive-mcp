(ns hive-mcp.tools.consolidated.workflow.readiness-backend-trifecta-test
  "A ling whose spawn-mode names a registered headless backend (hive-agent's
   :hive-agent) is ready when that backend reports it :idle or :running,
   never by asking Emacs. The backend is a stub in the real registry."
  (:require [clojure.test.check.generators :as gen]
            [hive-mcp.agent.ling.headless-registry :as registry]
            [hive-mcp.tools.consolidated.workflow.readiness :as readiness]
            [hive-spi.addon.headless :as headless]
            [hive-test.trifecta :refer [deftrifecta]]))

(def ^:private stub-id :readiness-stub-backend)

(defn- status-stub [status]
  (reify headless/IHeadlessBackend
    (headless-id [_] stub-id)
    (headless-spawn! [_ ctx _] (:id ctx))
    (headless-dispatch! [_ _ _] true)
    (headless-status [_ ctx _] (when status {:slave/id (:id ctx) :slave/status status}))
    (headless-kill! [_ _] nil)
    (headless-interrupt! [_ _] nil)))

(defn ready-case
  "ling-cli-ready? for a ling of the stub backend's mode, the backend
   reporting STATUS (nil = it does not know the ling)."
  [status]
  (registry/register-headless! stub-id (status-stub status))
  (try
    (boolean (readiness/ling-cli-ready? "ling-1" stub-id))
    (finally (registry/deregister-headless! stub-id))))

(def statuses [:idle :running :done :error :killed nil])

(deftrifecta registered-backend-readiness
  hive-mcp.tools.consolidated.workflow.readiness-backend-trifecta-test/ready-case
  {:golden-path "test/golden/tools/consolidated/workflow/readiness_backend.edn"
   :cases       (into {} (map (fn [s] [(pr-str s) s])) statuses)
   :gen         (gen/elements statuses)
   :pred        boolean?
   :num-tests   30
   :mutations   [["registry-ignored" (fn [_] false)]
                 ["any-known-ling-ready" (fn [s] (some? s))]]})
