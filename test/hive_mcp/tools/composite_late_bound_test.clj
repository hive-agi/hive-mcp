(ns hive-mcp.tools.composite-late-bound-test
  "A root's lazily resolved handlers follow a reload of the subdomain they name."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.composite :as composite]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private sub 'hive-mcp.tools.composite-late-bound-test.sub)

(def ^:private sym (symbol (str sub) "handlers"))

(defn- load-sub!
  "Recreate the subdomain namespace, as a reload does: a new namespace object
   holding a new `handlers` var."
  [handlers]
  (remove-ns sub)
  (create-ns sub)
  (intern sub 'handlers handlers))

(defn- answers [v]
  {:list   (fn [_] v)
   :two    (fn [a b] [v a b])
   :nested {:go (fn [_] v)}})

(deftest leaves-follow-a-reload
  (try
    (load-sub! (answers :v1))
    (let [tree (composite/lazy-resolve-handlers sym)]
      (testing "before the reload"
        (is (= :v1 ((:list tree) {})))
        (is (= :v1 ((get-in tree [:nested :go]) {}))))
      (load-sub! (answers :v2))
      (testing "after the reload the same tree reaches the new code"
        (is (= :v2 ((:list tree) {})))
        (is (= :v2 ((get-in tree [:nested :go]) {})))
        (is (= [:v2 1 2] ((:two tree) 1 2))))
      (testing "a verb the reload removed answers from the handler read at load time"
        (load-sub! {})
        (is (= :v1 ((:list tree) {}))))
      (testing "a namespace that cannot be resolved does the same"
        (remove-ns sub)
        (is (= :v1 ((:list tree) {})))))
    (finally (remove-ns sub))))

(deftest the-tree-keeps-its-shape
  (try
    (load-sub! (answers :v1))
    (let [tree (composite/lazy-resolve-handlers sym)]
      (is (= #{:list :two :nested} (set (keys tree))))
      (is (= #{:go} (set (keys (:nested tree)))))
      (is (fn? (:list tree))))
    (finally (remove-ns sub))))

(deftest a-leaf-keeps-the-metadata-of-its-var
  (try
    (load-sub! {:list #'answers})
    (is (= 'answers (:name (meta (:list (composite/lazy-resolve-handlers sym))))))
    (finally (remove-ns sub))))

(deftest an-unresolvable-symbol-is-an-empty-tree
  (is (= {} (composite/lazy-resolve-handlers 'no.such.namespace/handlers))))
