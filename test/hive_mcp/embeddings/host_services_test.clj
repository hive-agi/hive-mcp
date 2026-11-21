(ns hive-mcp.embeddings.host-services-test
  "This host publishes the whole backend host-services catalog of hive-spi, so
   a vector-store backend (hive-chroma) that depends on hive-spi alone finds
   every service it calls through."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.embeddings.host-services :as host-services]
            [hive-mcp.embeddings.space :as space]
            [hive-mcp.protocols.vector :as vector]
            [hive-spi.embeddings.space :as spi-space]
            [hive-spi.vector.host-services :as hs]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest publishes-the-whole-catalog
  (let [caps (host-services/capabilities)]
    (is (= (set (keys hs/catalog)) (set (keys caps)))
        "every catalog key is published, and nothing outside it")
    (is (every? ifn? (vals caps)))))

(deftest register-installs-through-hive-spi
  (let [prior (hs/registered)]
    (try
      (hs/unregister-services!)
      (is (= (set (keys hs/catalog)) (host-services/register!)))
      (testing "a published capability is the host's own"
        (is (identical? @#'vector/IVectorCollectionStore (hs/invoke :vector/port)))
        (is (= (vector/store-set?) (hs/invoke :vector/store-set?))))
      (finally
        (hs/unregister-services!)
        (hs/register-services! prior)))))

(deftest space-is-the-hive-spi-layer
  (is (identical? spi-space/space->metadata space/space->metadata))
  (is (identical? spi-space/audit space/audit))
  (is (= spi-space/EmbeddingSpace space/EmbeddingSpace)))
