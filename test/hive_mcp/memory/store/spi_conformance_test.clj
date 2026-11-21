(ns hive-mcp.memory.store.spi-conformance-test
  "hive-spi.memory.conformance against the in-repo StubMemoryStore, which the
   rest of the suite injects as a stand-in for a real backend and so must
   honour the same contract.

   The Chroma half of this suite moved to the hive-chroma addon
   (hive-chroma.spi-conformance-test) with the provider it tests."
  (:require [hive-spi.memory.conformance :refer [defconformance]]
            [hive-mcp.test.stub.memory-store :as stub]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defconformance stub-spi
  #(stub/->stub)
  {:connect-config {}})
