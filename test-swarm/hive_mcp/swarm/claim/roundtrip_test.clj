(ns hive-mcp.swarm.claim.roundtrip-test
  "The claim path through the REAL store.

   `registry-test` builds spans by hand and asserts on them, which is why it
   stayed green while the feature was dead: `claim-span!` never persisted
   :claim/mode and `get-all-claims` did not project it, so every stored claim
   read back as :body and the signature-versus-caller rule never fired on real
   data. A test that constructs the value under test cannot see that. These
   go through DataScript."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.claim.registry :as registry]
            [hive-mcp.swarm.claim.span :as span]
            [hive-test.isolation :as iso]
            hive-mcp.isolation-methods))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (iso/with-isolations :swarm-ds))

(def ^:private f "/repo/src/app/core.clj")

(def ^:private callers
  {"app.core/alpha" ["app.core/beta"]})

(defn- ling! [id]
  (ds/add-slave! id {:name id :presets [] :cwd "/tmp"})
  id)

;; =============================================================================
;; The regression: a mode must survive the store
;; =============================================================================

(deftest a-signature-claim-reads-back-as-a-signature-claim
  (testing "mode is not recoverable from the key, so it must be persisted"
    (ling! "ling-1")
    (registry/claim-span! (span/form-span f "app.core/alpha" :signature) "ling-1")
    (let [held (registry/held-spans)
          mine (first (filter #(= "app.core/alpha" (:span/qn %)) held))]
      (is (some? mine) "the claim should come back at all")
      (is (= :signature (:span/mode mine))
          (str "read back as " (:span/mode mine)
               "; if this is :body the signature-versus-caller rule is dead"))
      (is (= f (:span/file mine)))
      (is (= "ling-1" (:claim/slave mine))))))

(deftest the-signature-rule-fires-on-a-stored-claim
  (testing "end to end: stored signature claim blocks a caller"
    (ling! "ling-1")
    (ling! "ling-2")
    (registry/claim-span! (span/form-span f "app.core/alpha" :signature) "ling-1")
    (let [found (span/conflicts (fn [qn] (get callers qn []))
                                (registry/held-spans)
                                (span/form-span f "app.core/beta")
                                "ling-2")]
      (is (= 1 (count found)))
      (is (= :signature-vs-caller (:reason (first found)))
          "ling-2 must be told that alpha's signature reaches beta"))))

(deftest a-body-claim-stores-body
  (ling! "ling-1")
  (registry/claim-span! (span/form-span f "app.core/alpha" :body) "ling-1")
  (is (= :body (:span/mode (first (registry/held-spans))))))

;; =============================================================================
;; The win, proven through the store
;; =============================================================================

(deftest two-lings-take-two-forms-in-one-file
  (ling! "ling-1")
  (ling! "ling-2")
  (let [a (registry/acquire! [(span/form-span f "app.core/alpha")] "ling-1")
        b (registry/acquire! [(span/form-span f "app.core/delta")] "ling-2")]
    (is (:acquired? a))
    (is (:acquired? b) (str "second ling refused: " (:conflicts b)))
    (is (= 2 (count (registry/held-spans))))))

(deftest the-same-form-is-refused-and-says-which
  (ling! "ling-1")
  (ling! "ling-2")
  (registry/acquire! [(span/form-span f "app.core/alpha")] "ling-1")
  (let [r (registry/acquire! [(span/form-span f "app.core/alpha")] "ling-2")
        c (first (:conflicts r))]
    (is (false? (:acquired? r)))
    (is (= :same-form (:reason c)))
    (is (= "ling-1" (:held-by c)))
    (is (= "app.core/alpha" (:qn c)))
    (is (re-find #"app.core/alpha" (:message c))
        "the refusal must name the form, not just the file")))

(deftest acquire-is-all-or-nothing
  (testing "a partial acquisition leaves an agent blocking with half a refactor"
    (ling! "ling-1")
    (ling! "ling-2")
    (registry/acquire! [(span/form-span f "app.core/alpha")] "ling-1")
    (let [r (registry/acquire! [(span/form-span f "app.core/delta")
                                (span/form-span f "app.core/alpha")]
                               "ling-2")]
      (is (false? (:acquired? r)))
      (is (zero? (:spans-claimed r)))
      (is (nil? (first (filter #(= "app.core/delta" (:span/qn %))
                               (registry/held-spans))))
          "delta was free but must NOT have been taken"))))

(deftest a-legacy-whole-file-claim-still-blocks-a-span
  (testing "old and new agents interoperate"
    (ling! "old-ling")
    (ling! "ling-2")
    (registry/claim-span! f "old-ling")
    (let [r (registry/acquire! [(span/form-span f "app.core/alpha")] "ling-2")]
      (is (false? (:acquired? r)))
      (is (= :file-claim (:reason (first (:conflicts r))))))))

(deftest releasing-a-span-frees-only-that-span
  (ling! "ling-1")
  (ling! "ling-2")
  (registry/claim-span! (span/form-span f "app.core/alpha") "ling-1")
  (registry/claim-span! (span/form-span f "app.core/delta") "ling-1")
  (registry/release-span! (span/form-span f "app.core/alpha"))
  (is (:acquired? (registry/acquire! [(span/form-span f "app.core/alpha")] "ling-2")))
  (is (false? (:acquired? (registry/acquire! [(span/form-span f "app.core/delta")]
                                             "ling-2")))))

;; =============================================================================
;; Liveness: a dead ling must not fence a form off forever
;; =============================================================================

(deftest a-stale-claim-stops-blocking
  (testing "an advisory claim from a dead ling buys no safety"
    (ling! "dead-ling")
    (registry/claim-span! (span/form-span f "app.core/alpha") "dead-ling")
    (is (= 1 (count (registry/held-spans))) "fresh claim is live")
    (let [future-ms (+ (System/currentTimeMillis) (* 11 60 1000))]
      (is (empty? (registry/held-spans {:now-ms future-ms}))
          "11 minutes on, the claim is stale and no longer fences the form")
      (is (= 1 (count (registry/held-spans {:now-ms future-ms
                                            :include-stale? true})))
          "the listing tools must still be able to see it"))))
