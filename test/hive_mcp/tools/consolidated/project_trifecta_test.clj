(ns hive-mcp.tools.consolidated.project-trifecta-test
  "Trifecta over project/handle-project-staleness: a directory that holds no
   .hive-project.edn scan is stale. Directories are generated under a path
   that does not exist, so the handler never touches a real project tree."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.tools.consolidated.project :as pj]
            [hive-mcp.tools.core :as tc]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn staleness-of
  "Unary adapter: the staleness verdict for DIRECTORY, as data."
  [directory]
  (let [resp (pj/handle-project-staleness {:directory directory})]
    (if (:isError resp)
      {:error true}
      (json/read-str (:text resp) :key-fn keyword))))

(deftrifecta staleness-of-absent-directories
  hive-mcp.tools.consolidated.project-trifecta-test/staleness-of
  {:golden-path "test/golden/tools/consolidated/project-staleness.edn"
   :cases       {:absent "/m27/nonexistent" :nested "/m27/a/b/c"}
   :gen         (gen/fmap #(str "/m27/nonexistent/" %) gen/string-alphanumeric)
   :pred        #(true? (:stale %))
   :num-tests   30
   :mutations   [["always-fresh" (fn [d] {:stale false :directory d})]
                 ["drops-directory" (fn [d] (dissoc (staleness-of d) :directory))]]})

(deftest project-handlers-answer-mcp-responses
  (is (m/validate tc/McpResponse (pj/handle-project-tree {:project_id "m27-no-such-project"})))
  (is (m/validate tc/McpResponse (pj/handle-project-staleness {:directory "/m27/nope"}))))
