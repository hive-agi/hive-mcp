(ns hive-mcp.swarm.claim.span-test
  "The span algebra, pinned at the cases that made it necessary.

   `callers` is a plain map here rather than a carto lookup: the graph rule has
   to be provable without an index, or the test is measuring the index."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.swarm.claim.span :as span]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private f "/repo/src/app/core.clj")
(def ^:private g "/repo/src/app/other.clj")

(def ^:private callers
  "beta and gamma call alpha; nothing calls beta."
  {"app.core/alpha" ["app.core/beta" "app.core/gamma"]
   "app.core/beta"  []})

(defn- callers-fn [qn] (get callers qn []))

(defn- held [slave s] (assoc (span/span s) :claim/slave slave))

;; =============================================================================
;; The win: two forms, one file, no contention
;; =============================================================================

(deftest two-bodies-in-one-file-do-not-conflict
  (testing "the case whole-file claims serialized for no reason"
    (let [a (span/form-span f "app.core/alpha")
          b (span/form-span f "app.core/delta")]
      (is (nil? (span/overlap callers-fn a b)))
      (is (false? (span/conflicts? callers-fn a b)))
      (is (empty? (span/conflicts callers-fn [(held "ling-1" a)] b "ling-2"))
          "ling-2 may take delta while ling-1 holds alpha"))))

(deftest the-same-form-still-conflicts
  (let [a (span/form-span f "app.core/alpha")
        c (span/conflicts callers-fn [(held "ling-1" a)] a "ling-2")]
    (is (= 1 (count c)))
    (is (= :same-form (:reason (first c))))
    (is (= "ling-1" (:held-by (first c))))
    (is (re-find #"already editing app.core/alpha" (span/explain (first c))))))

(deftest a-claim-does-not-conflict-with-its-own-holder
  (testing "re-claiming is idempotent, so a retry is cheap"
    (let [a (span/form-span f "app.core/alpha")]
      (is (empty? (span/conflicts callers-fn [(held "ling-1" a)] a "ling-1"))))))

(deftest different-files-never-conflict
  (is (nil? (span/overlap callers-fn
                          (span/form-span f "app.core/alpha")
                          (span/form-span g "app.core/alpha")))))

;; =============================================================================
;; The graph rule: a signature change reaches its callers
;; =============================================================================

(deftest signature-conflicts-with-a-caller
  (testing "changing alpha's arity rewrites beta, so beta is not free"
    (let [sig  (span/form-span f "app.core/alpha" :signature)
          body (span/form-span f "app.core/beta")
          why  (span/overlap callers-fn sig body)]
      (is (= :signature-vs-caller (:reason why)))
      (is (= "app.core/alpha" (:qn why)))
      (is (= "app.core/beta" (:caller why))))))

(deftest signature-conflict-is-symmetric
  (testing "it must not matter which side is already held"
    (let [sig  (span/form-span f "app.core/alpha" :signature)
          body (span/form-span f "app.core/beta")]
      (is (some? (span/overlap callers-fn sig body)))
      (is (some? (span/overlap callers-fn body sig))))))

(deftest a-body-change-does-not-reach-callers
  (testing "only the SIGNATURE rule consults the graph"
    (let [body-a (span/form-span f "app.core/alpha" :body)
          body-b (span/form-span f "app.core/beta")]
      (is (nil? (span/overlap callers-fn body-a body-b))
          "rewriting alpha's interior leaves beta's text alone"))))

(deftest signature-does-not-conflict-with-a-non-caller
  (let [sig  (span/form-span f "app.core/beta" :signature)
        body (span/form-span f "app.core/alpha")]
    (is (nil? (span/overlap callers-fn sig body))
        "nothing calls beta, so its signature reaches nobody")))

(deftest a-cold-graph-degrades-to-fewer-conflicts-never-wrong-ones
  (testing "callers-fn nil drops the graph rule and keeps the local ones"
    (let [sig  (span/form-span f "app.core/alpha" :signature)
          body (span/form-span f "app.core/beta")]
      (is (nil? (span/overlap nil sig body)))
      (is (some? (span/overlap nil sig (span/form-span f "app.core/alpha")))
          "the same-form rule never needed the graph"))))

;; =============================================================================
;; The file mode, and back-compat with whole-file claims
;; =============================================================================

(deftest a-file-claim-blankets-every-span-inside-it
  (let [whole (span/file-span f)
        body  (span/form-span f "app.core/alpha")]
    (is (= :file-claim (:reason (span/overlap callers-fn whole body))))
    (is (= :file-claim (:reason (span/overlap callers-fn body whole))))
    (is (re-find #"holds the whole of" (span/explain
                                        (first (span/conflicts
                                                callers-fn
                                                [(held "ling-1" whole)]
                                                body "ling-2")))))))

(deftest a-bare-string-is-still-a-whole-file-claim
  (testing "every pre-span caller keeps working untouched"
    (is (= (span/file-span f) (span/span f)))
    (is (= :file (:span/mode (span/span f))))
    (is (= f (span/key-of f)))))

(deftest a-form-mode-with-no-qn-widens-to-the-file
  (testing "claiming less than asked is the one failure that loses an edit"
    (let [s (span/span {:span/file f :span/mode :body :span/qn nil})]
      (is (= :file (:span/mode s)))
      (is (= f (span/key-of s))))
    (let [s (span/span {:span/file f :span/mode :body :span/qn "  "})]
      (is (= :file (:span/mode s))))))

;; =============================================================================
;; Keys and widening
;; =============================================================================

(deftest key-separates-forms-and-collides-with-legacy-file-rows
  (is (= (str f "#app.core/alpha")
         (span/key-of (span/form-span f "app.core/alpha"))))
  (is (not= (span/key-of (span/form-span f "app.core/alpha"))
            (span/key-of (span/form-span f "app.core/beta"))))
  (is (= f (span/key-of (span/file-span f)))
      "a file claim must key exactly as the legacy :claim/file row did"))

(deftest widen-never-narrows
  (is (= :file (span/widen :body :file)))
  (is (= :file (span/widen :file :body)))
  (is (= :signature (span/widen :body :signature)))
  (is (= :body (span/widen :body :body))))
