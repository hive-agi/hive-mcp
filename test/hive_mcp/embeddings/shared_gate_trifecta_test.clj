(ns hive-mcp.embeddings.shared-gate-trifecta-test
  "Schema-derived property, golden and mutation facets for the contracted
   admission policy and the gate's pure accessors."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-mcp.embeddings.shared-gate :as shared]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- expected-lane
  "Independent oracle, read off the docstring: a waiting interactive call is
   always served first; nothing waiting admits nothing."
  [interactive batch]
  (cond (> interactive 0) :interactive
        (> batch 0) :batch))

(deftrifecta-from-schema next-lane-trifecta
  hive-mcp.embeddings.shared-gate/next-lane
  {:in [:cat [:int {:min -3 :max 5}] [:int {:min -3 :max 5}]]
   :out [:maybe shared/Lane]
   :rel (fn [[i b] out] (= out (expected-lane i b)))
   :num-tests 300
   :seed 29
   :n-cases 12
   :golden-path "test/golden/embeddings/shared-gate-next-lane.edn"})

(deftest new-gate-validates-its-inputs
  (testing "a valid gate conforms to Gate"
    (is (m/validate shared/Gate (shared/new-gate 2 10))))
  (testing "non-positive capacity or timeout throws"
    (doseq [[p t] [[0 10] [1 0] [-1 5] [nil 5]]]
      (is (thrown? clojure.lang.ExceptionInfo (shared/new-gate p t))))))

(deftest gate-accessors-see-through-the-decorator
  (let [raw (reify proto/EmbeddingProvider
              (embed-text [_ _] [1.0])
              (embed-batch [_ ts] (mapv (constantly [1.0]) ts))
              (embedding-dimension [_] 1))
        gate (shared/new-gate 1 10)
        gated (shared/gated-provider raw gate)]
    (is (identical? gated (shared/gated-provider gated gate)) "idempotent")
    (is (identical? gate (shared/gate-of gated)))
    (is (identical? shared/process-gate (shared/gate-of raw)))
    (is (identical? raw (shared/ungated gated)))
    (is (identical? raw (shared/ungated raw)))
    (is (thrown? clojure.lang.ExceptionInfo (shared/gated-provider nil gate)))))

(deftest try-acquire-and-release-balance-the-permit
  (let [gate (shared/new-gate 1 10)]
    (is (true? (shared/try-acquire! gate :batch 10)))
    (is (false? (shared/try-acquire! gate :batch 5)) "no capacity left")
    (is (nil? (shared/release! gate)))
    (is (zero? @(:active gate)))
    (is (= :ok (shared/with-permit gate :interactive (constantly :ok))))
    (is (false? (shared/await-waiters gate :batch 1 5)))))
