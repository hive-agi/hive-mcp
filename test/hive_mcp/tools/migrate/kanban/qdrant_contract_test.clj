(ns hive-mcp.tools.migrate.kanban.qdrant-contract-test
  "Contract facets for the qdrant adapter's pure decode: only points with a
   payload whose uuid maps back to a requested id are kept, keyed by id."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.migrate.kanban.ports :as ports]
            [hive-mcp.tools.migrate.kanban.qdrant :as q]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private uuid-of #(str (#'q/->qdrant-uuid %)))

(defn decode
  "Subject: decode points for IDS where POINTS name [id payload?] pairs."
  [[points ids]]
  (q/decode-results-by-id
   (mapv (fn [[id payload?]] {:id (uuid-of id)
                              :payload (when payload? {:type "note" :content "{}"})})
         points)
   ids
   identity))

(deftrifecta-from-schema decode-keeps-requested-points-with-payload
  #'decode
  {:in [:tuple [:vector {:max 4} [:tuple [:enum "a" "b" "c"] :boolean]]
        [:vector {:max 3} [:enum "a" "b" "z"]]]
   :out [:map-of :string :map]
   :rel (fn [[points ids] out]
          (= (set (keys out))
             (set (for [[id p?] points :when (and p? (some #{id} ids))] id))))
   :num-tests 100})

(golden/deftest-golden qdrant-decode-golden
  "test/golden/hive-mcp/tools/migrate/kanban-qdrant-decode.edn"
  (into (sorted-map)
        (decode [[["a" true] ["b" false] ["c" true]] ["a" "b"]])))

(deftest the-reader-is-an-entry-reader
  (is (satisfies? ports/IEntryReader (q/make-entry-reader nil))))
