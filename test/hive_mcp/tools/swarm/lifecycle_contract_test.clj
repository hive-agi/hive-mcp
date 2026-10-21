(ns hive-mcp.tools.swarm.lifecycle-contract-test
  "Contract facets for the deprecated swarm spawn/kill handlers: with no
   swarm host every call is refused with the host-unavailable error, whatever
   the params."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.tools.swarm.lifecycle :as lc]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn hostless-kill
  "Subject: kill with no swarm host registered."
  [params]
  (when-not (core/swarm-addon-available?) (lc/handle-swarm-kill params)))

(deftrifecta-from-schema kill-without-a-host-is-refused
  #'hostless-kill
  {:in [:map [:slave_id {:optional true} [:enum "s1" "all"]]
        [:force_cross_project {:optional true} :boolean]]
   :out core/SwarmError
   :rel (fn [_ out] (= out (core/addon-not-loaded-error)))
   :num-tests 30})

(golden/deftest-golden lifecycle-hostless-golden
  "test/golden/hive-mcp/tools/swarm/lifecycle-hostless.edn"
  {:kill  (lc/handle-swarm-kill {:slave_id "s1"})
   :spawn (lc/handle-swarm-spawn {:name "n"})})

(deftest spawn-without-a-host-is-refused
  (is (true? (:isError (lc/handle-swarm-spawn {})))))
