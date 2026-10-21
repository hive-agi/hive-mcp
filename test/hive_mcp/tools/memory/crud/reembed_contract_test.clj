(ns hive-mcp.tools.memory.crud.reembed-contract-test
  "Contract facets for reembed: the blank-id refusal of handle-reembed, and
   the drain tally over an outbox with no parked ids (an isolated temp queue
   dir, so no store is touched)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.memory.crud.deferred :as deferred]
            [hive-mcp.tools.memory.crud.reembed :as reembed]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private BlankId
  "Every id spelling handle-reembed refuses before reaching a store."
  [:map [:id [:enum nil "" " " "\t\n"]]])

(deftrifecta-from-schema handle-reembed-refuses-a-blank-id
  #'reembed/handle-reembed
  {:in BlankId
   :out [:map [:type [:= "text"]] [:text :string] [:isError [:= true]]]
   :rel (fn [_ out] (str/includes? (:text out) "id is required"))
   :num-tests 50})

(defn- empty-queue-dir []
  (.getPath (.toFile (java.nio.file.Files/createTempDirectory
                      "reembed-contract" (make-array java.nio.file.attribute.FileAttribute 0)))))

(golden/deftest-golden empty-outbox-golden
  "test/golden/hive-mcp/tools/memory/reembed-empty-outbox.edn"
  (binding [deferred/*queue-dir* (empty-queue-dir)]
    {:drain (reembed/drain-pending!)
     :batch (reembed/handle-batch-reembed {})
     :blank (reembed/handle-reembed {:id "  "})}))

(deftest drain-tally-sums-to-total
  (binding [deferred/*queue-dir* (empty-queue-dir)]
    (let [{:keys [total reembedded not-found errors]} (reembed/drain-pending!)]
      (is (= total (+ reembedded not-found errors))))))
