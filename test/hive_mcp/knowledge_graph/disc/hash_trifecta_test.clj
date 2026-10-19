(ns hive-mcp.knowledge-graph.disc.hash-trifecta-test
  "Trifecta for the disc hash facade's compute-hash, and a golden over
   file-content-hash on a file the test owns."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [hive-mcp.knowledge-graph.disc.hash :as hash]
            [hive-schemas.test :as hst]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- out-schema [nsym fsym]
  (-> (m/function-schemas) (get-in [nsym fsym :schema]) m/children second))

(defn- sha256 [^String s]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest md (.getBytes s "UTF-8"))))))

(hst/deftrifecta-from-schema compute-hash-contract
  hive-mcp.knowledge-graph.disc.hash/compute-hash
  {:in [:cat :string]
   :out (out-schema 'hive-mcp.knowledge-graph.disc.hash 'compute-hash)
   :rel (fn [[s] out] (= out (sha256 s)))
   :golden-path "test/golden/hive-mcp/m14-compute-hash.edn"})

(defn- with-probe-file
  "Call F with a file holding CONTENT, deleted afterwards."
  [content f]
  (let [file (java.io.File/createTempFile "m14-hash" ".txt")]
    (try (spit file content) (f (.getPath file))
         (finally (io/delete-file file true)))))

(golden/deftest-golden file-content-hash-golden
  "test/golden/hive-mcp/m14-file-content-hash.edn"
  {:cleared (hash/clear-hash-cache!)
   :existing (with-probe-file "m14 probe" hash/file-content-hash)
   :other (with-probe-file "another probe" hash/file-content-hash)
   :missing (hash/file-content-hash "/nonexistent/m14/probe.txt")
   :nil-path (hash/file-content-hash nil)})

(deftest file-content-hash-agrees-with-compute-hash
  (with-probe-file "same content"
    (fn [path]
      (is (= {:hash (hash/compute-hash "same content") :exists? true}
             (hash/file-content-hash path)))
      (is (= {:hash (hash/compute-hash "same content") :exists? true}
             (hash/file-content-hash (io/file path)))
          "a java.io.File is a path too"))))
