(ns hive-mcp.engine.hprof.disk-test
  "Golden over the disk-pressure probe. Free space is host-dependent, so the
   snapshot records only answers that hold on every host: a path with no
   existing ancestor reads 0 bytes, and the floor comparison at the two
   extremes (0 GB never pressures, an absurd floor always does)."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.engine.hprof.disk :as disk]
            [hive-test.golden :refer [deftest-golden]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private tmp (System/getProperty "java.io.tmpdir"))

(deftest-golden disk-probe-golden
  "test/golden/engine/hprof-disk-probe.edn"
  {:no-ancestor-bytes      (disk/free-bytes "no-such-root-f08/a/b")
   :no-ancestor-gb         (disk/free-gb "no-such-root-f08/a/b")
   :no-ancestor-pressured? (disk/under-pressure? "no-such-root-f08/a/b" 1)
   :tmp-floor-zero         (disk/under-pressure? tmp 0)
   :tmp-floor-absurd       (disk/under-pressure? tmp 1000000000)
   :missing-child-of-tmp   (pos? (disk/free-bytes (str tmp "/f08-missing/deeper")))
   :file-arg               (pos? (disk/free-bytes (java.io.File. tmp)))})

(deftest gb-is-bytes-over-gibibyte
  (let [b (disk/free-bytes tmp)
        gb (disk/free-gb tmp)]
    (is (< (Math/abs (- gb (/ (double b) 1024.0 1024.0 1024.0))) 1.0e-3))))
