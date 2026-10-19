(ns hive-mcp.embedder.config-test
  "EmbedderConfig resolves the whole `:embedder` block as opaque data. An
   override wins over the config file, so resolution is checked through
   overrides — no file is read for those cases."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.embedder.config :as config]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Block
  "An `:embedder` block shaped like the shipped one: a default provider key,
   routes and providers."
  [:map
   [:default {:optional true} [:enum :ollama-qwen3-4b :venice-qwen3]]
   [:routes {:optional true} [:map-of [:enum :type/note :type/axiom] :keyword]]
   [:providers {:optional true}
    [:map-of :keyword [:map [:impl [:enum :ollama :venice]] [:model :string]]]]])

(deftrifecta-from-schema resolve-override-trifecta config/resolve!
  {:in [:cat [:map [:embedder-block Block]]]
   :out [:map [:embedder-block :map]]
   :rel (fn [[overrides] resolved]
          (= (:embedder-block overrides) (:embedder-block resolved)))
   :num-tests 100})

(deftest block-is-the-resolved-map
  (is (map? (config/block)))
  (is (= (:embedder-block (config/resolve!)) (config/block))))

(deftest-contract-coverage embedder-contracts
  {:paths ["src/hive_mcp/embedder"]})
