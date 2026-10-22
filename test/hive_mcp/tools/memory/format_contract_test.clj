(ns hive-mcp.tools.memory.format-contract-test
  "Contract facets for hive-mcp.tools.memory.format."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.memory.schema :as ms]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private SimpleEntry
  [:map {:closed true}
   [:id [:enum "e1" "e2"]]
   [:type [:enum "note" "decision"]]
   [:tags [:maybe [:vector {:max 2} [:enum "a" "kanban"]]]]
   [:content [:maybe [:enum "short" "a much longer piece of content that will be truncated"]]]])

(deftrifecta-from-schema entry-metadata-keeps-identity-and-caps-preview
  #'fmt/entry->metadata
  {:in [:cat SimpleEntry [:int {:min 6 :max 40}]]
   :out ms/EntryMetadata
   :rel (fn [[e n] out]
          (and (= (:id e) (:id out)) (= (:type e) (:type out))
               (= (or (:tags e) []) (:tags out))
               (<= (count (:preview out)) n)))
   :golden-path "test/golden/hive-mcp/tools/memory/entry-metadata.edn"
   :num-tests 200})

(deftrifecta-from-schema json-alist-defaults-tags
  #'fmt/entry->json-alist
  {:in [:cat [:maybe SimpleEntry]]
   :out [:maybe [:map [:tags any?]]]
   :rel (fn [[e] out]
          (if (nil? e) (nil? out) (= (or (:tags e) []) (:tags out))))
   :golden-path "test/golden/hive-mcp/tools/memory/entry-json-alist.edn"
   :num-tests 100})

(deftrifecta-from-schema entry-receipt-never-echoes-content
  #'fmt/entry->receipt
  {:in [:cat [:maybe SimpleEntry]]
   :out [:maybe ms/EntryReceipt]
   :rel (fn [[e] out]
          (if (nil? e)
            (nil? out)
            (and (= (:id e) (:id out)) (= "ok" (:status out))
                 (not (contains? out :content))
                 (= (count (or (:content e) "")) (:chars out))
                 (<= (count (:preview out)) 61))))
   :golden-path "test/golden/hive-mcp/tools/memory/entry-receipt.edn"
   :num-tests 100})

(deftest receipt-of-a-3kb-entry-fits-in-300-bytes
  (let [body    (apply str (repeat 3000 "x"))
        entry   {:id "20260712154114-0e04470a" :type "note" :tags ["a" "b"]
                 :content body :content-hash (apply str (repeat 64 "d"))
                 :duration "long" :expires ""}
        receipt (fmt/entry->receipt entry)]
    (is (= 3000 (:chars receipt)))
    (is (= (str (subs body 0 60) "…") (:preview receipt)))
    (is (not (contains? receipt :content)))
    (is (< (count (.getBytes ^String (json/write-str receipt) "UTF-8")) 300))))

(deftest json-renderings-round-trip-the-ids
  (let [es [{:id "e1" :tags ["a"]} {:id "e2"}]]
    (is (= ["e1" "e2"] (map #(get % "id") (json/read-str (fmt/entries->json es)))))
    (is (= ["e1" "e2"] (map #(get % "id") (json/read-str (fmt/entries->metadata-json es)))))))
