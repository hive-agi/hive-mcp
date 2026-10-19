(ns hive-mcp.embeddings.config-trifecta-test
  "Schema-derived property, golden and mutation facets for the contracted
   pure parts of the embedding config value object."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.embeddings.config :as config]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- expected-route
  "Independent oracle, read off the docstring of `collection-route`."
  [{:keys [primary fallback success-message fallback-warning]} ok?]
  (cond
    (= :ollama primary) {:provider :ollama}
    ok?                 {:provider :openrouter :message success-message :level :info}
    :else               {:provider fallback :message fallback-warning
                         :level (when fallback-warning :warn)}))

(deftrifecta-from-schema collection-route-trifecta
  hive-mcp.embeddings.config/collection-route
  {:in [:cat config/CollectionSpec :boolean]
   :out config/CollectionRoute
   :rel (fn [[spec ok?] out] (= out (expected-route spec ok?)))
   :num-tests 300
   :seed 53
   :n-cases 12
   :golden-path "test/golden/embeddings/config-collection-route.edn"})

(deftest shipped-collection-specs-conform
  (doseq [spec config/collection-provider-specs
          ok? [true false]]
    (is (= (expected-route spec ok?) (config/collection-route spec ok?)))))
