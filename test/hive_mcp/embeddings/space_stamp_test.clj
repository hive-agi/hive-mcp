(ns hive-mcp.embeddings.space-stamp-test
  "Collection provenance listing: `space/audit` is the non-throwing companion
   of `space/admit` — :stamped, :legacy or :diverged as data, with the differing
   keys and the remedy, so an operator can list every incompatible collection
   in one pass instead of dying on the first mismatch.

   Pure: no provider, no transport, no collection — metadata maps in, verdict
   rows out. Ported from the pre-extraction hive-mcp suite (3368d665); the
   transport-backed listing tests live with the Chroma addon
   (hive-chroma.space-stamp-test)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.embeddings.space :as space]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private dim 8)
(def ^:private coll-name "hive-mcp-memory-8d")

(defn- space-of [overrides]
  (space/->space (merge {:provider :ollama :model "stub-embed" :digest "sha256:aaaa"
                         :dimension dim :metric :l2 :query-template-rev "raw"
                         :doc-serializer-rev "memory-to-document/1"}
                        overrides)))

(deftest audit-reports-every-verdict-as-data
  (let [s (space-of {})]
    (testing "stamped with the expected space"
      (let [row (space/audit coll-name s (space/space->metadata s))]
        (is (= :stamped (:verdict row)))
        (is (= (space/space-id s) (:space-id row)))
        (is (= (space/space-id s) (:expected-id row)))
        (is (nil? (:remedy row)) "nothing to remedy")
        (is (m/validate space/AuditRow row))))
    (testing "no stamp at all is legacy, and its space id is the unknown one"
      (let [row (space/audit coll-name s {:dimension dim})]
        (is (= :legacy (:verdict row)))
        (is (= space/unknown-space-id (:space-id row)))
        (is (= #{:isolate :re-embed} (:remedy row)))
        (is (m/validate space/AuditRow row))))
    (testing "a recorded space that differs is diverged, never thrown"
      (let [other (space-of {:digest "sha256:bbbb"})
            row   (space/audit coll-name s (space/space->metadata other))]
        (is (= :diverged (:verdict row)))
        (is (= (space/space-id other) (:space-id row)))
        (is (= (space/space-id s) (:expected-id row)))
        (is (= [:digest] (:differing-keys row)))
        (is (= #{:isolate :re-embed} (:remedy row)))
        (is (m/validate space/AuditRow row))))
    (testing "with no expected space a recorded collection is still stamped"
      (is (= :stamped (:verdict (space/audit coll-name (space/space->metadata s)))))
      (is (nil? (:expected-id (space/audit coll-name (space/space->metadata s))))))))

(deftest audit-without-an-expected-space-never-names-one
  (let [s (space-of {})]
    (testing "legacy metadata with no expected space"
      (let [row (space/audit coll-name {:dimension dim})]
        (is (= :legacy (:verdict row)))
        (is (not (contains? row :expected-id)))
        (is (= #{:isolate :re-embed} (:remedy row)))))
    (testing "nil metadata is legacy too"
      (let [row (space/audit coll-name nil)]
        (is (= :legacy (:verdict row)))
        (is (not (contains? row :expected-id)))))
    (testing "a stamp read back through string keys audits the same"
      (let [row (space/audit coll-name s (update-keys (space/space->metadata s) name))]
        (is (= :stamped (:verdict row)))
        (is (= (space/space-id s) (:expected-id row)))))))

(deftest audit-diverges-on-every-defining-key
  (testing "each space key that moves makes the collection diverged, in canonical order"
    (let [s     (space-of {})
          moves {:provider :openai :model "other-embed" :digest "sha256:bbbb"
                 :dimension 16 :metric :cosine :normalized? true
                 :query-template-rev "q2" :doc-serializer-rev "memory-to-document/2"}]
      (doseq [[k v] moves]
        (let [row (space/audit coll-name s (space/space->metadata (assoc s k v)))]
          (is (= :diverged (:verdict row)) (str k))
          (is (= [k] (:differing-keys row)) (str k))))
      (let [row (space/audit coll-name s (space/space->metadata
                                          (assoc s :digest "sha256:bbbb" :model "other-embed")))]
        (is (= [:model :digest] (:differing-keys row))
            "canonical space-key order, not map order")))))