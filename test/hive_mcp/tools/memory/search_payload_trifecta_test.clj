(ns hive-mcp.tools.memory.search-payload-trifecta-test
  "Golden + property + mutation pinning for search-payload.

   `memory search` excluded carto and ingestion-chunk entries by default and
   said nothing: 191 entries were hidden in one session and the caller could
   not tell an empty corpus from a filtered one (kanban
   20260728020157-1407f421). The payload now echoes the exclusion set and
   where it came from.

   Mutants are self-contained and never call the subject var."
  (:require [clojure.test :refer [is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.tools.memory.search :as search]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private defaults ["carto" "ingestion-chunk"])

(def ^:private tag-pool ["carto" "ingestion-chunk" "kanban" "note"])

(def ^:private gen-input
  (gen/let [n     (gen/choose 0 5)
            excl  (gen/one-of [(gen/return defaults)
                               (gen/return [])
                               (gen/vector-distinct (gen/elements tag-pool) {:max-elements 3})])]
    {:results          (vec (range n))
     :query            "q"
     :scope            "p"
     :exclude-tags     excl
     :default-excludes defaults}))

(def ^:private input-of (atom nil))

(defn run-payload
  "Subject wrapper: remembers its input so :pred can check the output."
  [input]
  (reset! input-of input)
  (search/search-payload input))

(defn- loud? [out]
  (let [{:keys [results exclude-tags default-excludes]} @input-of]
    (and (= (count results) (:count out))
         (= (vec exclude-tags) (:excluded-tags out))
         (= (:excludes-source out)
            (cond (empty? exclude-tags) :none
                  (= (set exclude-tags) (set default-excludes)) :default
                  :else :caller)))))

(deftrifecta search-payload-contract
  hive-mcp.tools.memory.search-payload-trifecta-test/run-payload
  {:golden-path "test/golden/tools/memory/search-payload.edn"
   :cases       {:default-excludes {:results [1 2] :query "q" :scope "p"
                                    :exclude-tags defaults :default-excludes defaults}
                 :caller-empty     {:results [] :query "q" :scope "p"
                                    :exclude-tags [] :default-excludes defaults}
                 :caller-custom    {:results [1] :query "q" :scope "p"
                                    :exclude-tags ["kanban"] :default-excludes defaults}
                 :nil-excludes     {:results [] :query "q" :scope nil
                                    :exclude-tags nil :default-excludes defaults}}
   :gen         gen-input
   :pred        loud?
   :num-tests   100
   :mutations   [["silent — the original payload, no exclusion echo"
                  (fn [{:keys [results query scope]}]
                    {:results results :count (count results) :query query :scope scope})]
                 ["always-default — blames the default for a caller set"
                  (fn [{:keys [results query scope exclude-tags]}]
                    {:results results :count (count results) :query query :scope scope
                     :excluded-tags (vec exclude-tags) :excludes-source :default})]]
   :assert      (fn []
                  (is (= {:excluded-tags ["carto" "ingestion-chunk"] :excludes-source :default}
                         (select-keys (run-payload {:results [] :exclude-tags defaults
                                                    :default-excludes defaults})
                                      [:excluded-tags :excludes-source]))
                      "an empty answer under the default filter says it was filtered")
                  (is (= :caller
                         (:excludes-source (run-payload {:results [] :exclude-tags ["kanban"]
                                                         :default-excludes defaults})))
                      "a caller-supplied exclusion set is attributed to the caller")
                  (is (= :none
                         (:excludes-source (run-payload {:results [] :exclude-tags []
                                                         :default-excludes defaults})))
                      "exclude_tags=[] reports no filter"))})
