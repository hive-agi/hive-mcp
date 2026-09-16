(ns hive-mcp.channel.drain-projection-test
  "Contract tests for the drain wire policy.

   The load-bearing assertion is not that :index is smaller. It is that :index
   is smaller WITHOUT dropping an id: every entry still reaches the caller, as
   a pointer instead of a body. A policy that saved tokens by silently
   discarding entries would pass a size check and be a data-loss bug."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.channel.drain-projection :as proj]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def gen-entry
  (gen/let [t (gen/elements [:axiom :convention :principle :note :knowledge])
            id (gen/not-empty gen/string-alphanumeric)
            c (gen/not-empty gen/string-ascii)
            tags (gen/vector (gen/not-empty gen/string-alphanumeric) 0 9)]
    {:id id :T t :C c :tags tags}))

(deftest an-axiom-is-never-projected
  (testing "a floor entry is never reduced to a POINTER under any pool policy"
    (let [ax {:id "a1" :T :axiom :C "# AXIOM\n\nbody that must survive verbatim" :tags ["x"]}]
      (doseq [policy [:full :index]]
        (let [out (proj/project-entry ax {:policy policy :axiom-policy :compact})]
          (is (nil? (:ref out))
              (str "axiom turned into a pointer under " policy))
          (is (str/includes? (str (:C out)) "body that must survive verbatim")
              "the normative body must survive byte-identical")))))
  (testing "with the floor policy :full the entry is untouched"
    (let [ax {:id "a1" :T :axiom :C "# AXIOM\n\n## Why\n\nstory" :tags ["x"]}]
      (is (= ax (proj/project-entry ax {:policy :index :axiom-policy :full})))))
  (testing "a pinned pool entry is promoted to the floor and so is not pointerised"
    (let [e {:id "p1" :T :convention :C "# Title\n\nbody" :tags []}]
      (is (nil? (:ref (proj/project-entry e {:policy :index :pins #{"p1"}
                                             :axiom-policy :full}))))
      (is (:ref (proj/project-entry e {:policy :index :pins #{"other"}}))
          "unpinned pool entry should have been projected"))))

(deftest the-string-spelling-of-a-type-is-the-one-production-sends
  (testing "format-entry emits :T as a STRING, so the floor test must survive it"
    (let [ax {:id "a" :T "axiom"
              :C (str "# AXIOM\n\n" (apply str (repeat 400 "x")))
              :tags ["t"]}]
      (is (= ax (proj/project-entry ax {:policy :index}))
          "a string-typed axiom was projected — the inviolable entry would be summarised"))
    (let [conv {:id "c" :T "convention"
                :C (str "# Title\n\n" (apply str (repeat 400 "x")))
                :tags ["t"]}]
      (is (:ref (proj/project-entry conv {:policy :index}))
          "a string-typed pool entry should still project"))))

(deftest full-policy-is-identity
  (let [es [{:id "1" :T :convention :C "# a\nbody" :tags ["t"]}
            {:id "2" :T :axiom :C "# b\nbody" :tags []}]]
    (is (= es (proj/project es {:policy :full})))))

(deftest index-rows-carry-what-a-decision-needs
  (let [e {:id "i1" :T :convention
           :C "# hot inject makes an addon live NOW\n\nlong body follows here"
           :tags ["a" "b" "c" "d" "e" "f" "g"]}
        r (proj/project-entry e {:policy :index})]
    (is (= "i1" (:id r)) "id is the pull handle and must survive")
    (is (= :convention (:T r)))
    (is (true? (:ref r)) "a pointer must be marked as one")
    (is (= "hot inject makes an addon live NOW" (:C r))
        "markdown heading marks are stripped from the title")
    (is (<= (count (:tags r)) proj/index-tags))
    (is (< (count (pr-str r)) (count (pr-str e))))))

(deftest title-of-handles-degenerate-content
  (is (= "" (proj/title-of nil)))
  (is (= "" (proj/title-of "")))
  (is (= "" (proj/title-of "   \n\n  ")))
  (is (= "first real line" (proj/title-of "\n\n   # first real line\nsecond")))
  (testing "a long title is truncated and marked"
    (let [t (proj/title-of (apply str (repeat 500 "x")))]
      (is (= (+ proj/title-chars 3) (count t)))
      (is (str/ends-with? t "...")))))

(deftest resolve-policy-never-throws-and-falls-back
  (is (contains? proj/policies (proj/resolve-policy)))
  (is (= :full (proj/resolve-policy :full)))
  (is (= :index (proj/resolve-policy "index")) "string spelling is accepted")
  (is (= :index (proj/resolve-policy ":index")) "leading colon is tolerated")
  (is (= proj/default-policy (proj/resolve-policy :nonsense))
      "an unknown policy falls back rather than failing the drain"))

(deftest pull-hint-only-when-actionable
  (is (nil? (proj/pull-hint [{:id "1" :T :axiom :C "body"}]))
      "a full batch carries no instruction it cannot act on")
  (let [h (proj/pull-hint [{:id "1" :T :convention :C "t" :ref true}])]
    (is (string? h))
    (is (str/includes? h "memory get"))
    (is (str/includes? h "kg traverse") "the KG route is the point of the index")))

(defspec projection-never-drops-an-id 200
  (prop/for-all [es (gen/vector gen-entry 0 40)]
    (= (mapv :id es)
       (mapv :id (proj/project es {:policy :index})))))

(defspec projection-never-grows-the-payload 200
  (prop/for-all [es (gen/vector gen-entry 0 40)]
    (<= (count (pr-str (proj/project es {:policy :index})))
        (count (pr-str es)))))

(defspec projection-is-idempotent 200
  (prop/for-all [es (gen/vector gen-entry 0 40)]
    (let [once (proj/project es {:policy :index})]
      (= once (proj/project once {:policy :index})))))

(defspec axioms-are-never-turned-into-pointers 200
  (prop/for-all [es (gen/vector gen-entry 0 40)]
    (let [out (proj/project es {:policy :index})]
      (every? #(nil? (:ref %)) (filter #(= :axiom (:T %)) out)))))

(defspec axioms-are-untouched-under-the-full-floor-policy 200
  (prop/for-all [es (gen/vector gen-entry 0 40)]
    (let [axioms (filterv #(= :axiom (:T %)) es)
          out (filterv #(= :axiom (:T %))
                       (proj/project es {:policy :index :axiom-policy :full}))]
      (= axioms out))))
