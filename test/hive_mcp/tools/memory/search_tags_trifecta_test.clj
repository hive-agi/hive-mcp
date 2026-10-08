(ns hive-mcp.tools.memory.search-tags-trifecta-test
  "Golden + property + mutation pinning for filter-required-tags.

   `memory search` accepted a `tags` param and dropped it, so a search for
   tags=[session-wrap] answered five hits none of which carried the tag
   (kanban 20260713154842-68993ce0). The filter keeps a hit only when it
   carries EVERY required tag, the same AND semantics `memory query` uses.

   Mutants are self-contained and never call the subject var."
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.tools.memory.search :as search]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private tag-pool ["session-wrap" "decision" "kanban" "carto" "note"])

(defn- entry [id tags]
  {:id id :document id :distance 0.1
   :metadata {:tags (str/join "," tags) :type "note" :content id}})

(def ^:private gen-input
  (gen/let [rows (gen/vector (gen/tuple gen/string-alphanumeric
                                        (gen/vector (gen/elements tag-pool) 0 3))
                             0 8)
            req  (gen/vector (gen/elements tag-pool) 0 2)]
    {:entries (mapv (fn [[id tags]] (entry id tags)) rows)
     :tags    req}))

(defn- tag-set [e]
  (into #{} (remove str/blank?) (str/split (get-in e [:metadata :tags]) #",")))

(def ^:private input-of (atom nil))

(defn- run-filter
  "Subject wrapper: remembers its input so :pred can check the output
   against it."
  [input]
  (reset! input-of input)
  (search/filter-required-tags input))

(defn- sound-and-complete? [out]
  (let [{:keys [entries tags]} @input-of
        required (set tags)
        keep?    #(every? (tag-set %) required)]
    (and (vector? out)
         (= out (filterv keep? entries)))))

(deftrifecta filter-required-tags-contract
  hive-mcp.tools.memory.search-tags-trifecta-test/run-filter
  {:golden-path "test/golden/tools/memory/filter-required-tags.edn"
   :cases       {:no-tags-keeps-all
                 {:entries [(entry "a" ["note"]) (entry "b" [])] :tags []}
                 :nil-tags-keeps-all
                 {:entries [(entry "a" ["note"])] :tags nil}
                 :single-required
                 {:entries [(entry "a" ["session-wrap" "note"])
                            (entry "b" ["decision"])
                            (entry "c" ["session-wrap"])]
                  :tags ["session-wrap"]}
                 :all-required-and
                 {:entries [(entry "a" ["session-wrap" "decision"])
                            (entry "b" ["session-wrap"])]
                  :tags ["session-wrap" "decision"]}
                 :none-match
                 {:entries [(entry "a" ["note"])] :tags ["kanban"]}}
   :gen         gen-input
   :pred        sound-and-complete?
   :num-tests   200
   :mutations   [["ignore-tags — the original bug, every hit passes"
                  (fn [{:keys [entries]}] (vec entries))]
                 ["any-tag — OR instead of AND"
                  (fn [{:keys [entries tags]}]
                    (if (empty? tags)
                      (vec entries)
                      (filterv #(some (tag-set %) tags) entries)))]
                 ["substring — 'wrap' matches 'session-wrap'"
                  (fn [{:keys [entries tags]}]
                    (filterv (fn [e] (every? #(str/includes? (get-in e [:metadata :tags]) %)
                                             (map #(subs % 0 (max 0 (- (count %) 2))) tags)))
                             entries))]]
   :assert      (fn []
                  (is (= ["a" "c"]
                         (mapv :id (search/filter-required-tags
                                    {:entries [(entry "a" ["session-wrap"])
                                               (entry "b" ["decision"])
                                               (entry "c" ["note" "session-wrap"])]
                                     :tags ["session-wrap"]})))
                      "only hits carrying the required tag survive")
                  (is (= ["x"]
                         (mapv :id (search/filter-required-tags
                                    {:entries [{:id "x" :metadata {:tags ["kanban" "todo"]}}]
                                     :tags ["kanban"]})))
                      "a sequential :tags value is read as tags too"))})
