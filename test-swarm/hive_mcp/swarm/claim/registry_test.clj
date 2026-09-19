(ns hive-mcp.swarm.claim.registry-test
  "The parts of the registry that hold without a database: key identity and the
   row round-trip. The acquire path is covered where the swarm store is stood
   up; what is pinned here is the reasoning a stored row is decoded by."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.swarm.claim.registry :as registry]
            [hive-mcp.swarm.claim.span :as span]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private f "/repo/src/app/core.clj")

;; =============================================================================
;; The invariant the whole scheme rests on
;; =============================================================================

(deftest mode-is-not-part-of-the-key
  (testing "body and signature on one form MUST collide on the unique index"
    (let [body (span/form-span f "app.core/alpha" :body)
          sig  (span/form-span f "app.core/alpha" :signature)]
      (is (= (span/key-of body) (span/key-of sig))
          (str "if these differ, two agents can hold a body claim and a"
               " signature claim on the same form at once, which is the exact"
               " collision the registry exists to stop"))))
  (testing "different forms still get different keys"
    (is (not= (span/key-of (span/form-span f "app.core/alpha"))
              (span/key-of (span/form-span f "app.core/beta"))))))

;; =============================================================================
;; Row round-trip
;; =============================================================================

(deftest a-span-row-decodes-back-to-its-span
  (let [s   (span/form-span f "app.core/alpha")
        row {:claim/file (span/key-of s) :claim/slave "ling-1"}
        got (registry/row->span row)]
    (is (= f (:span/file got)))
    (is (= "app.core/alpha" (:span/qn got)))
    (is (= "ling-1" (:claim/slave got)))))

(deftest stored-fields-outrank-the-parsed-key
  (testing "a row carrying its own qn and mode is believed over key shape"
    (let [got (registry/row->span {:claim/file "/r/a.clj#app/alpha"
                                   :claim/qn   "app/explicit"
                                   :claim/mode :signature
                                   :claim/slave "l1"})]
      (is (= "app/explicit" (:span/qn got)))
      (is (= :signature (:span/mode got))))))

(deftest a-legacy-row-decodes-to-a-blanket-file-claim
  (testing "a claim written before spans must still block every span inside"
    (let [got (registry/row->span {:claim/file f :claim/slave "old-ling"})]
      (is (= :file (:span/mode got)))
      (is (nil? (:span/qn got)))
      (is (some? (span/overlap nil got (span/form-span f "app.core/alpha")))
          "the old row still blankets the file"))))

(deftest a-windows-style-path-is-not-mistaken-for-a-span-key
  (testing "only # splits a key, so a path with no # stays a file claim"
    (let [got (registry/row->span {:claim/file "C:/repo/a.clj" :claim/slave "l"})]
      (is (= :file (:span/mode got)))
      (is (= "C:/repo/a.clj" (:span/file got))))))

(deftest the-last-hash-splits-so-a-path-containing-one-survives
  (testing "a directory with # in its name keeps its qn recoverable"
    (let [got (registry/row->span {:claim/file "/re#po/a.clj#app/alpha"
                                   :claim/slave "l"})]
      (is (= "/re#po/a.clj" (:span/file got)))
      (is (= "app/alpha" (:span/qn got))))))

(deftest the-claim-lock-is-one-shared-object
  (testing "two mutexes guarding one invariant is not mutual exclusion"
    (let [get-atom (requiring-resolve 'hive-mcp.swarm.logic/get-logic-db-atom)]
      (is (identical? (get-atom) (get-atom))
          (str "acquire! locks this atom to serialize against the older"
               " file-granular claim path; if it were rebuilt per call the two"
               " paths would each hold their own lock and neither would"
               " exclude the other")))))
