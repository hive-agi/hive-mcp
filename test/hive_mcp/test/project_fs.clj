(ns hive-mcp.test.project-fs
  "Test support: lay out `.hive-project.edn` files on disk."
  (:require [clojure.java.io :as io]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn write-project!
  "Create DIR (and parents) and write CFG as its `.hive-project.edn`.
   Returns nil."
  [dir cfg]
  (.mkdirs (io/file dir))
  (spit (io/file dir ".hive-project.edn") (pr-str cfg)))

(m/=> write-project! [:=> [:cat [:or :string [:fn #(instance? java.io.File %)]] :map] :nil])
