(ns hive-mcp.batch.ref-contract-judges-test
  "Trifectas for the judges of `hive-mcp.batch.ref-contract`, driven by their
   own contracts, and the contract-coverage gate for src/hive_mcp/batch.

   The corpora are exercised against every provider by
   `hive-mcp.batch.ref-conformance-test`; here the judges themselves are
   checked against relations that do not call them."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.batch.ref-contract :as rc]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :as hst]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage batch-dir-contracts-gate
  {:paths ["src/hive_mcp/batch"]
   :exempt {}})

(def ^:private AnyCandidate
  "Ref-shaped strings, plain strings, nil and a non-string."
  [:or rc/RefCandidate :nil :int])

(defn- body-of [s] (subs s (count rc/ref-prefix)))

(hst/deftrifecta-from-schema ref-string-contract
  hive-mcp.batch.ref-contract/ref-string?
  {:in        AnyCandidate
   :out       :boolean
   :rel       (fn [in out]
                (= out (and (string? in) (= rc/ref-prefix (subs in 0 (min 5 (count in)))))))
   :num-tests 200
   :golden-path "test/golden/batch/ref-string.edn"})

(hst/deftrifecta-from-schema ref-op-id-contract
  hive-mcp.batch.ref-contract/ref-op-id
  {:in        AnyCandidate
   :out       [:maybe rc/OpId]
   ;; a ref answers its body's dot-free head; anything else answers nil
   :rel       (fn [in out]
                (if (and (string? in) (str/starts-with? in rc/ref-prefix))
                  (and (str/starts-with? (body-of in) out)
                       (not (str/includes? out "."))
                       (or (= out (body-of in))
                           (= \. (nth (body-of in) (count out)))))
                  (nil? out)))
   :num-tests 200
   :golden-path "test/golden/batch/ref-op-id.edn"})

(def ^:private Nested
  "Values a ref may hide in: strings, refs, and maps and vectors of them."
  [:or rc/RefCandidate
   [:vector {:max 3} rc/RefCandidate]
   [:map-of {:max 2} :keyword [:or rc/RefCandidate [:vector {:max 2} rc/RefCandidate]]]
   :int])

(defn- leaves [v]
  (cond (map? v) (mapcat leaves (vals v))
        (sequential? v) (mapcat leaves v)
        :else [v]))

(hst/deftrifecta-from-schema ref-strings-contract
  hive-mcp.batch.ref-contract/ref-strings
  {:in        Nested
   :out       [:vector :string]
   ;; exactly the leaves that carry the prefix, in walk order
   :rel       (fn [in out]
                (= out (filterv #(and (string? %) (str/starts-with? % rc/ref-prefix)) (leaves in))))
   :num-tests 200
   :golden-path "test/golden/batch/ref-strings.edn"})

(def ^:private RefOp
  "One op of `RefOps` (the vector's element schema)."
  (nth rc/RefOps 2))

(defn- head-of [ref] (first (str/split (body-of ref) #"\." 2)))

(hst/deftrifecta-from-schema op-ref-op-ids-contract
  hive-mcp.batch.ref-contract/op-ref-op-ids
  {:in        RefOp
   :out       rc/OpIds
   ;; :from is the op's only addressable ref param: :content is prose and
   ;; :id / :depends_on are plumbing
   :rel       (fn [{:keys [from]} out]
                (= out (if (str/starts-with? from rc/ref-prefix) #{(head-of from)} #{})))
   :num-tests 200
   :golden-path "test/golden/batch/op-ref-op-ids.edn"})

(defn- expected-pairs [ops]
  (set (for [{:keys [id from depends_on]} ops
             :when (str/starts-with? from rc/ref-prefix)
             :let [target (head-of from)]
             :when (not (contains? (set depends_on) target))]
         [id target])))

(hst/deftrifecta-from-schema undeclared-ref-pairs-contract
  hive-mcp.batch.ref-contract/undeclared-ref-pairs
  {:in        rc/RefOps
   :out       [:set [:tuple :any rc/OpId]]
   :rel       (fn [ops out] (= out (expected-pairs ops)))
   :num-tests 200
   :golden-path "test/golden/batch/undeclared-ref-pairs.edn"})

(hst/deftrifecta-from-schema expected-ref-dep-errors-contract
  hive-mcp.batch.ref-contract/expected-ref-dep-errors
  {:in        rc/RefOps
   :out       [:set :string]
   ;; one error per undeclared pair, each naming both ends
   :rel       (fn [ops out]
                (and (= (count out) (count (expected-pairs ops)))
                     (every? (fn [[id target]]
                               (some #(and (str/includes? % (str "'" id "'"))
                                           (str/includes? % (str "'" target "'")))
                                     out))
                             (expected-pairs ops))))
   :num-tests 200})

(hst/deftrifecta-from-schema undeclared-ref-error-contract
  hive-mcp.batch.ref-contract/undeclared-ref-error
  {:in        [:cat [:enum "a" "b" nil] [:enum "x" "ghost" ""]]
   :out       [:string {:min 40}]
   :rel       (fn [[id target] out]
                (and (str/includes? out (str "'" id "'"))
                     (str/includes? out (str "'" target "'"))
                     (str/includes? out "depends_on")))
   :golden-path "test/golden/batch/undeclared-ref-error.edn"})

(hst/deftrifecta-from-schema not-found-sentinel-contract
  hive-mcp.batch.ref-contract/not-found-sentinel?
  {:in        [:enum :hive-mcp.batch/ref-not-found :x/ref-not-found :ref-not-found
               :x/other "ref-not-found" 'x/ref-not-found nil]
   :out       :boolean
   :rel       (fn [in out]
                (= out (contains? #{:hive-mcp.batch/ref-not-found :x/ref-not-found} in)))
   :n-cases   10
   :golden-path "test/golden/batch/not-found-sentinel.edn"})

(def ^:private faithful-table
  "[candidate answer] -> faithful?, written out by hand over the whole cross
   product of the candidates and answers below."
  (let [a {:op-id "a" :path ["b"]}
        b {:op-id "a" :path []}
        c {:op-id "a" :path ["c"]}
        d {:op-id "a.b" :path []}]
    {["$ref:a.b" nil] false ["$ref:a.b" a] true  ["$ref:a.b" b] false
     ["$ref:a.b" c] false   ["$ref:a.b" d] false
     ["$ref:a." nil] false  ["$ref:a." a] false  ["$ref:a." b] true
     ["$ref:a." c] false    ["$ref:a." d] false
     ["plain" nil] true     ["plain" a] false     ["plain" b] false
     ["plain" c] false      ["plain" d] false
     [nil nil] true         [nil a] false         [nil b] false
     [nil c] false          [nil d] false}))

(hst/deftrifecta-from-schema parsed-ref-faithful-contract
  hive-mcp.batch.ref-contract/parsed-ref-faithful?
  {:in        [:cat (into [:enum] (distinct (map first (keys faithful-table))))
               (into [:enum] (distinct (map second (keys faithful-table))))]
   :out       :boolean
   :rel       (fn [in out] (= out (faithful-table in)))
   :num-tests 200
   :n-cases   16
   :golden-path "test/golden/batch/parsed-ref-faithful.edn"})

(deftest parse-corpus-rows-are-faithful-test
  (testing "every :bx/a corpus row is faithful under the judge"
    (doseq [[label in expected] rc/parse-ref-cases]
      (is (true? (rc/parsed-ref-faithful? in expected)) label))))

(deftest ref-contract-schemas-generate-test
  (doseq [s [rc/OpId rc/PathSegment rc/Path rc/ParsedRef rc/MaybeParsedRef rc/RefCandidate
             rc/Op rc/Ops rc/RefOps rc/ErrorStrings rc/OpIds]]
    (is (seq (mg/sample s {:size 4})) (pr-str s))))
