(ns hive-mcp.scripts.contract-test
  "Contract facets for src/hive_mcp/scripts: the rebuild classifier as a
   schema-driven trifecta, and the export / import / verify round trip run
   against a fresh per-test DataScript KG store (fixtures/datascript-fixture),
   so no real datahike directory is touched and every test owns its store.
   rebuild! itself moves the open store's directory and exits the JVM's agents,
   so it is not driven here; aot-boot's compile-boot-closure! calls System/exit
   and is exempted in the gate."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-mcp.scripts.aot-boot :as aot]
            [hive-mcp.scripts.kg-datahike-rebuild :as rebuild]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.generator :as mg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each fixtures/datascript-fixture)

;; =============================================================================
;; Gate
;; =============================================================================

(coverage/deftest-contract-coverage scripts-contracts
  {:paths ["src/hive_mcp/scripts"]
   :exempt {'hive-mcp.scripts.aot-boot/compile-boot-closure!
            "An exec-fn that never returns: it ends in (System/exit 0|1) after
             AOT-compiling the boot closure into target/boot-classes. There is
             no return value to promise, and calling it under any test or
             instrumentation would exit the JVM."}})

(deftest the-gate-is-not-vacuous
  (is (= 9 (count (:universe (coverage/coverage {:paths ["src/hive_mcp/scripts"]}))))))

(deftest every-scripts-contract-generates-both-sides
  (doseq [ns-sym '[hive-mcp.scripts.aot-boot hive-mcp.scripts.kg-datahike-rebuild]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

(deftest ctor-namespaces-are-symbols-from-the-manifests
  (let [nss (aot/ctor-namespaces)]
    (is (vector? nss))
    (is (every? simple-symbol? nss))
    (is (= (count nss) (count (distinct nss))))))

;; =============================================================================
;; Classifier
;; =============================================================================

(def ^:private uuid-a "0f8fad5b-d9cb-469f-a165-70867728950e")
(def ^:private uuid-b "7c9e6679-7425-40de-944b-e07fc1f90ae7")
(def ^:private Endpoint
  ;; Half the endpoints are UUIDs, so both-UUID (structural) edges are drawn
  ;; about a quarter of the time.
  [:enum uuid-a uuid-b uuid-a uuid-b "20260101-abc" "node-1" "" nil])

(def ^:private GenEntity
  [:or
   [:map [:kg-edge/id [:enum "e1" "e2"]] [:kg-edge/from Endpoint] [:kg-edge/to Endpoint]]
   [:map [:mem-mutation/id [:enum "mut-1" "mut-2"]] [:mem-mutation/op [:enum :decay :feedback]]]
   [:map [:some/attr :string]]
   :nil :string])

(defn- uuid? [s] (contains? #{uuid-a uuid-b} s))

(deftrifecta-from-schema entity-kind-sorts-the-export
  #'rebuild/entity-kind
  {:in [:cat GenEntity]
   :out rebuild/EntityKind
   :rel (fn [[e] out]
          (= out (cond (not (map? e)) :unknown
                       (:mem-mutation/id e) :mutation
                       (not (:kg-edge/id e)) :unknown
                       (and (uuid? (:kg-edge/from e)) (uuid? (:kg-edge/to e))) :edge-structural
                       :else :edge-durable)))
   :classify (fn [_ out] out)
   :classify-domain rebuild/EntityKind
   ;; structural edges are about one draw in twenty; 5 of 300 is a firm floor
   :classify-floor 5
   :golden-path "test/golden/hive-mcp/scripts/entity-kind.edn"
   :num-tests 300})

(deftrifecta-from-schema keep-entity-keeps-mutations-and-durable-edges
  #'rebuild/keep-entity?
  {:in [:cat GenEntity]
   :out :boolean
   :rel (fn [[e] out] (= out (contains? #{:mutation :edge-durable} (rebuild/entity-kind e))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :golden-path "test/golden/hive-mcp/scripts/keep-entity.edn"
   :num-tests 200})

(deftrifecta-from-schema structural-edge-wants-two-uuid-endpoints
  #'rebuild/structural-edge?
  {:in [:cat [:maybe [:map [:kg-edge/from Endpoint] [:kg-edge/to Endpoint]]]]
   :out :boolean
   :rel (fn [[e] out] (= out (boolean (and (uuid? (:kg-edge/from e)) (uuid? (:kg-edge/to e))))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :classify-floor 10
   :golden-path "test/golden/hive-mcp/scripts/structural-edge.edn"
   :num-tests 200})

;; =============================================================================
;; Export / import / verify over a fresh DataScript store
;; =============================================================================

(def ^:private seed-tx
  [{:kg-edge/id "d1" :kg-edge/from "20260101-abc" :kg-edge/to uuid-a :kg-edge/relation :implements}
   {:kg-edge/id "d2" :kg-edge/from "note-1" :kg-edge/to "note-2" :kg-edge/relation :refines}
   {:kg-edge/id "s1" :kg-edge/from uuid-a :kg-edge/to uuid-b :kg-edge/relation :depends-on}
   {:mem-mutation/id "mut-1" :mem-mutation/entry-id "note-1" :mem-mutation/op :edit}])

(defn- temp-file []
  (doto (java.io.File/createTempFile "kg-keep" ".edn") (.deleteOnExit)))

(deftest-golden export-keeps-durable-edges-and-mutations
  "test/golden/hive-mcp/scripts/export-keep-edges.edn"
  (let [f (temp-file)]
    (conn/transact-sync! seed-tx)
    (let [report (rebuild/export-keep-edges! (.getPath f))
          rows (with-open [r (io/reader f)] (mapv read-string (line-seq r)))]
      {:report (assoc report :max-heap-pct :bounded)
       :ids (sort (keep #(or (:kg-edge/id %) (:mem-mutation/id %)) rows))
       :no-db-id (not-any? :db/id rows)})))

(deftest export-report-validates-and-bounds-the-heap-reading
  (conn/transact-sync! seed-tx)
  (let [report (rebuild/export-keep-edges! (temp-file))]
    (is (m/validate rebuild/ExportReport report))
    (is (= (:scanned report) (+ (:kept report) (:dropped report))))))

(deftest import-then-verify-round-trips-the-keep-set
  (let [f (temp-file)]
    (spit f (str (pr-str {:kg-edge/id "d1" :kg-edge/from "a" :kg-edge/to uuid-a}) "\n"
                 (pr-str {:kg-edge/id "d2" :kg-edge/from "b" :kg-edge/to "c"}) "\n"
                 (pr-str {:kg-edge/id "d3" :kg-edge/from "c" :kg-edge/to "d"}) "\n"))
    (is (= {:imported 3} (rebuild/import-keep-edges! f :chunk 2)))
    (is (= {:total-edges 3 :structural-leaked? false} (rebuild/verify)))))

(deftest verify-reports-a-structural-leak
  (conn/transact-sync! seed-tx)
  (is (= {:total-edges 3 :structural-leaked? true} (rebuild/verify))))
