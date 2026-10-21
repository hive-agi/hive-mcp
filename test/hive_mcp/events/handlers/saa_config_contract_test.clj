(ns hive-mcp.events.handlers.saa-config-contract-test
  "Contract facets for the SAA handler config. Only caller-supplied ids reach
   the goldens: the fallback id depends on the environment the suite runs in."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.handlers.saa-config :as saa-config]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:string {:min 1 :max 10}])

(deftrifecta-from-schema effective-agent-id-prefers-the-caller
  #'saa-config/effective-agent-id
  {:in [:cat Id]
   :out Id
   :rel (fn [[agent-id] out] (= agent-id out))
   :golden-path "test/golden/hive-mcp/events/handlers/saa-effective-agent-id.edn"
   :num-tests 100})

(deftrifecta-from-schema resolve-honours-an-override
  #'saa-config/resolve!
  {:in [:cat [:map {:closed true} [:slave-agent-id Id]]]
   :out [:map {:closed true} [:slave-agent-id Id]]
   :rel (fn [[overrides] out] (= overrides out))
   :golden-path "test/golden/hive-mcp/events/handlers/saa-resolve.edn"
   :num-tests 50})

(deftest a-missing-agent-id-falls-back-to-the-resolved-one
  (let [fallback (saa-config/slave-agent-id)]
    (is (string? fallback))
    (is (pos? (count fallback)))
    (is (= fallback (saa-config/effective-agent-id nil)))
    (is (= fallback (:slave-agent-id (saa-config/resolve!))))))
