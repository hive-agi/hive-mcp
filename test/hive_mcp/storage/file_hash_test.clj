(ns hive-mcp.storage.file-hash-test
  "SHA-256 content hashing: compute-hash as a trifecta from its contract
   (64 lowercase hex, deterministic, str-ed input), and file-content-hash over
   temp files: present, absent, nil, and the mtime-keyed cache."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [hive-mcp.storage.file-hash :as fh]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema compute-hash-is-sha256-of-str
  #'fh/compute-hash
  {:in [:cat [:or :string :int :keyword :nil]]
   :out fh/Sha256Hex
   :rel (fn [[x] out]
          (and (= out (fh/compute-hash (str x)))
               (m/validate fh/Sha256Hex out)))
   :golden-path "test/golden/hive-mcp/storage/file-hash-compute.edn"
   :num-tests 200})

(defn- temp-file [content]
  (doto (java.io.File/createTempFile "f04-hash" ".txt")
    (.deleteOnExit)
    (spit content)))

(deftest-golden file-content-hash-golden
  "test/golden/hive-mcp/storage/file-hash-files.edn"
  (let [_       (fh/clear-hash-cache!)
        a       (temp-file "hello")
        b       (temp-file "")
        path-a  (.getAbsolutePath a)
        first-a (fh/file-content-hash path-a)
        ;; same mtime: the cache answers, even though the bytes changed
        _       (let [mt (.lastModified a)] (spit a "changed") (.setLastModified a mt))
        cached  (fh/file-content-hash path-a)
        ;; new mtime: recomputed
        _       (.setLastModified a (+ (.lastModified a) 5000))
        fresh   (fh/file-content-hash path-a)]
    {:hello   first-a
     :cached  (= first-a cached)
     :changed fresh
     :empty   (fh/file-content-hash (.getAbsolutePath b))
     :absent  (fh/file-content-hash (str (io/file (System/getProperty "java.io.tmpdir")
                                                  "f04-no-such-file.txt")))
     :nil     (fh/file-content-hash nil)
     :clear   (fh/clear-hash-cache!)}))

(deftest file-hash-results-validate
  (is (m/validate fh/FileHash (fh/file-content-hash nil))))
