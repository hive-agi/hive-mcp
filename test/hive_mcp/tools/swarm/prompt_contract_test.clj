(ns hive-mcp.tools.swarm.prompt-contract-test
  "Contract facets for swarm prompts: the lazy preset header names every
   preset and is nil for none; with no swarm host the prompt handlers
   answer the host-unavailable error."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.tools.swarm.prompt :as pr]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema preset-header-names-every-preset
  #'pr/build-lazy-preset-header
  {:in [:cat [:vector {:max 3} [:enum "tdd" "clarity" "hivemind"]]]
   :out [:maybe :string]
   :rel (fn [[names] out]
          (if (seq names)
            (every? #(str/includes? out %) names)
            (nil? out)))
   :classify (fn [[names] _] (if (seq names) :some :none))
   :classify-domain #{:some :none}
   :num-tests 100})

(golden/deftest-golden prompt-hostless-golden
  "test/golden/hive-mcp/tools/swarm/prompt-hostless.edn"
  {:header  (pr/build-lazy-preset-header ["tdd"])
   :pending (pr/handle-swarm-pending-prompts {})
   :respond (pr/handle-swarm-respond-prompt {:slave_id "s" :response "y"})})

(deftest without-a-host-the-handlers-refuse
  (when-not (core/swarm-addon-available?)
    (is (true? (:isError (pr/handle-swarm-pending-prompts {}))))))
