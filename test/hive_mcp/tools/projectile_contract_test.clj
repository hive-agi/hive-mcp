(ns hive-mcp.tools.projectile-contract-test
  "Contract facets for the projectile handlers. The :vessel :dispatch port
   is the stub swarm host (hive-mcp.test.stub.swarm-host), installed by a
   fixture, never an Emacs client. The trifecta points at the contracted
   handler var; its oracle is the envelope rule of dispatch-projectile."
  (:require [clojure.test :refer [use-fixtures]]
            [hive-mcp.test.stub.swarm-host :as sh]
            [hive-mcp.tools.projectile :as projectile]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private down-pattern
  "The search pattern the stub vessel refuses; every other op succeeds."
  "down")

(defn- stub-vessel
  "Answers a projectile op: refused for the down pattern, else success with
   a result naming the op's pattern."
  [op _timeout]
  (if (= down-pattern (:pattern op))
    {:success false :error "projectile not loaded"}
    {:success true :result (str "[\"" (:pattern op) "\"]")}))

(use-fixtures :each
  (fn [t]
    (let [prior (sh/install! (sh/->host stub-vessel))]
      (try (t) (finally (sh/restore! prior))))))

(deftrifecta-from-schema projectile-search-keeps-the-text-envelope
  #'projectile/handle-projectile-search
  {:in [:cat [:map [:pattern [:enum "defn" "ns" "down"]]]]
   :out projectile/ProjectileResponse
   :rel (fn [[{p :pattern}] out]
          (if (= down-pattern p)
            (and (true? (:isError out)) (= "Error: projectile not loaded" (:text out)))
            (= {:type "text" :text (str "[\"" p "\"]")} out)))
   :classify (fn [[{p :pattern}] _] (if (= down-pattern p) :refused :ok))
   :classify-domain #{:refused :ok}
   :golden-path "test/golden/hive-mcp/tools/projectile-search-envelope.edn"
   :num-tests 60})

(golden/deftest-golden projectile-envelopes-golden
  "test/golden/hive-mcp/tools/projectile-envelopes.edn"
  {:files   (projectile/handle-projectile-files {:pattern "clj"})
   :blank   (projectile/handle-projectile-files {:pattern " "})
   :find    (projectile/handle-projectile-find-file {:filename "core.clj"})
   :recent  (projectile/handle-projectile-recent {})
   :list    (projectile/handle-projectile-list-projects {})
   :info    (projectile/handle-projectile-info {:directory "/r"})
   :refused (projectile/handle-projectile-files {:pattern down-pattern})})
