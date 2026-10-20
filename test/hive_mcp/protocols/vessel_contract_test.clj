(ns hive-mcp.protocols.vessel-contract-test
  "Contract facets for the vessel registry and the agent-context resolution.
   Every test owns the registry: it starts with a stub vessel and the noop
   vessel, and the prior vessels are registered again afterwards."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.protocols.vessel :as vs]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord StubVessel [id agents]
  hive-addon.vessel/IVessel
  (vessel-id [_] id)
  (capabilities [_] #{:terminal})
  (resolve-context [_ agent-id] (get agents agent-id))
  (addon [_ _] nil)
  (initialize! [_ _] nil)
  (shutdown! [_] nil))

(def ^:private agents
  {"ling-1" {:project-id "hive-mcp" :cwd "/w/hive-mcp" :session-id "s1"}
   "ling-2" {:project-id "hive-agent" :cwd "/w/hive-agent" :session-id "s2"}})

(defn- own-the-registry [f]
  (let [prior (vec (vs/get-vessels))]
    (vs/clear-vessels!)
    (vs/register-vessel! (vs/noop-vessel))
    (vs/register-vessel! (->StubVessel :m17-tmux agents))
    (try (f)
         (finally (vs/clear-vessels!)
                  (run! vs/register-vessel! prior)))))

(use-fixtures :each own-the-registry)

(deftrifecta-from-schema resolve-agent-context-asks-every-vessel
  #'vs/resolve-agent-context
  {:in [:cat [:enum "ling-1" "ling-2" "ling-absent" nil]]
   :out [:maybe :map]
   :rel (fn [[a] out] (= (get agents a) out))
   :classify (fn [_ out] (if out :resolved :unknown))
   :classify-domain #{:resolved :unknown}
   :golden-path "test/golden/hive-mcp/protocols/vessel-resolve-agent-context.edn"
   :num-tests 60})

(deftrifecta-from-schema get-vessel-finds-what-was-registered
  #'vs/get-vessel
  {:in [:cat [:enum :m17-tmux :noop :m17-absent]]
   :out [:maybe vs/Vessel]
   :rel (fn [[id] out] (if (= id :m17-absent) (nil? out) (= id (vs/vessel-id out))))
   :num-tests 40})

(golden/deftest-golden registry-lifecycle-golden
  "test/golden/hive-mcp/protocols/vessel-lifecycle.edn"
  (let [ids #(set (map vs/vessel-id (vs/get-vessels)))
        start (ids)
        replaced (vs/vessel-id (vs/register-vessel! (->StubVessel :m17-tmux {})))
        after-replace (vs/resolve-agent-context "ling-1")
        unregistered (vs/unregister-vessel! :noop)
        after-unregister (ids)
        cleared (vs/clear-vessels!)]
    {:start start :replaced replaced :after-replace after-replace
     :unregistered unregistered :after-unregister after-unregister
     :cleared cleared :after-clear (vs/get-vessels)
     :resolve-with-none (vs/resolve-agent-context "ling-1")}))

(deftest register-rejects-a-non-vessel
  (is (thrown? AssertionError (vs/register-vessel! {:vessel-id :fake}))))
