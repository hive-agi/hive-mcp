(ns hive-mcp.tools.catchup.bucket-types-test
  "Tests for the single-source-of-truth bucket-type defs.

   Every assertion guards against a future drift between the shared value
   object in bucket-types and the consumers that depend on it."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.catchup.bucket-types :as bt]
            [hive-mcp.tools.catchup.bundle-cache :as bc]
            [clojure.java.io :as io]))

(def ^:private type-names
  "The seven memory types that can land in a catchup bucket (documented
   contract of this namespace)."
  ["axiom" "axiom-candidate" "principle" "convention" "decision" "snippet" "note"])

(defn- string-type-vars
  "Return the vars whose values are the seven string type names."
  []
  [(var bt/axiom)
   (var bt/axiom-candidate)
   (var bt/principle)
   (var bt/convention)
   (var bt/decision)
   (var bt/snippet)
   (var bt/note)])

(deftest all-contains-exactly-the-seven-types-test
  (testing "`all` has exactly 7 members"
    (is (= 7 (count bt/all))))
  (testing "`all` equals the set of the 7 single-type vars"
    (let [singletons (into #{} (map deref) (string-type-vars))]
      (is (= singletons bt/all)))))

(deftest bundle-cache-identical-to-bt-all-test
  (testing "bundle-cache/bundle-types is IDENTICAL? to bt/all (not just equal)"
    (is (identical? bt/all bc/bundle-types)
        "bundle-cache/bundle-types must be identical? to bt/all. If someone
         re-types the literal set this will fail, which is by design — the
         single-source-of-truth rule means bt/all is THE definition.")))

(deftest no-type-string-literals-in-bundle-clj-test
  (testing "bundle.clj has no quoted memory-type string literals"
    (let [source (slurp (io/resource "hive_mcp/tools/catchup/bundle.clj"))]
      (doseq [t type-names]
        ;; Match the full quoted token, closing quote included, so "axiom"
        ;; does not false-positive on "axiom-candidate".
        (let [pattern (re-pattern (java.util.regex.Pattern/quote (str "\"" t "\"")))]
          (is (not (re-find pattern source))
              (str "bundle.clj must not contain the literal string \"" t "\". "
                   "Use bt/" t " instead.")))))))
