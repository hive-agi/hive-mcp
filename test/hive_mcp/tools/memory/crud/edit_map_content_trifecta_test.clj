(ns hive-mcp.tools.memory.crud.edit-map-content-trifecta-test
  "Trifecta contract for memory edit find/replace over non-string content
   (kanban 20260921210947-5c988302).

   Subject: `edit/resolve-content` projected to a stable verdict. A kanban
   card's :content is a map; find/replace on it used to throw a bare
   ClassCastException. It must now be an :invalid-edit naming the reason,
   while string content keeps its exact unique-match swap.

   Facets:
   - golden:   verdicts for string, nil, map and vector content
   - property: total over generated content, and never a ClassCastException
   - mutation: a string-casting resolver and an always-refusing resolver are caught"
  (:require [clojure.string :as str]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.tools.memory.crud.edit :as edit]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private resolve-content @#'edit/resolve-content)

(defn run-find-replace
  "Apply find \"beta\" / replace \"B\" to an entry whose :content is CONTENT.
   Verdict: {:ok new-content}, {:invalid-edit kanban-hint?} or {:thrown class}."
  [content]
  (try
    {:ok (resolve-content {:content content} {:find "beta" :replace "B"})}
    (catch clojure.lang.ExceptionInfo e
      (if (= :invalid-edit (:type (ex-data e)))
        {:invalid-edit (str/includes? (ex-message e) "kanban update")}
        {:thrown (str (class e))}))
    (catch Throwable t
      {:thrown (str (class t))})))

(def ^:private cases
  {:string-unique  "alpha beta gamma"
   :nil-content    nil
   :kanban-map     {:task-type "kanban" :title "t" :description "alpha beta"}
   :vector-content ["alpha" "beta"]})

(def ^:private gen-content
  (gen/one-of
   [(gen/fmap #(str % " beta " %) gen/string-alphanumeric)
    (gen/fmap (fn [d] {:task-type "kanban" :description d}) gen/string-alphanumeric)
    (gen/vector gen/string-alphanumeric)
    (gen/return nil)]))

(defn- never-thrown?
  "Property predicate (receives OUTPUT): an ok value or a named refusal, never a raw throw."
  [out]
  (and (map? out) (not (contains? out :thrown))))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(deftrifecta find-replace-map-content-contract
  hive-mcp.tools.memory.crud.edit-map-content-trifecta-test/run-find-replace
  {:golden-path   "test/golden/tools/memory/edit-find-replace-map-content.edn"
   :cases         cases
   :gen           gen-content
   :pred          never-thrown?
   :property-type :pred
   :num-tests     100
   :mutations
   [["string-cast — the original bug"
     (fn [content] (if (string? content)
                     {:ok (str/replace-first content "beta" "B")}
                     {:thrown "java.lang.ClassCastException"}))]
    ["always-refuse"
     (fn [_content] {:invalid-edit false})]]})
