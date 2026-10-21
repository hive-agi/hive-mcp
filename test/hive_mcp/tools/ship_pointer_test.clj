(ns hive-mcp.tools.ship-pointer-test
  "The git/magit tool text points at `git ship`, not at the retired /ship
   skills."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.consolidated.git :as git]
            [hive-mcp.tools.consolidated.magit :as cmagit]
            [hive-mcp.tools.magit :as magit]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest git-tool-text-points-at-git-ship
  (doseq [[label text] {"git tool"            (:description git/tool-def)
                        "magit tool"          (:description cmagit/tool-def)
                        "magit ns doc"        (:doc (meta (find-ns 'hive-mcp.tools.magit)))
                        "feature-branches doc" (:doc (meta #'magit/handle-magit-feature-branches))}]
    (testing label
      (is (not (str/includes? text "/ship")) text)
      (is (str/includes? text "git ship land|status|reconcile") text))))
