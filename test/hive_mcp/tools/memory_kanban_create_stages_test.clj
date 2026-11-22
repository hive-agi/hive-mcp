(ns hive-mcp.tools.memory-kanban-create-stages-test
  "Golden + property tests for the pure stages `memory-kanban/create*` is
   decomposed into (validate, priority, extra tags, card content, card tags)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.tools.memory-kanban :as mk]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- validation-error? [params]
  (try (mk/validate-create-params! params) false
       (catch clojure.lang.ExceptionInfo e
         (= :validation-error (:type (ex-data e))))))

(deftest validate-create-params-golden
  (testing "valid params return nil"
    (is (nil? (mk/validate-create-params! {:title "t"})))
    (is (nil? (mk/validate-create-params! {:title "t" :status "inprogress"}))))
  (testing "blank / missing title is a validation error"
    (is (validation-error? {}))
    (is (validation-error? {:title nil}))
    (is (validation-error? {:title "   "})))
  (testing "unknown status is a validation error"
    (is (validation-error? {:title "t" :status "nonsense"}))))

(deftest resolve-create-priority-golden
  (is (= "high" (mk/resolve-create-priority "high" ["priority-low"]))
      "explicit priority wins over a tag")
  (is (= "low" (mk/resolve-create-priority nil ["x" "priority-low"]))
      "priority-* tag is honoured")
  (is (= "medium" (mk/resolve-create-priority nil ["priority-urgent" "x"]))
      "unknown priority tag falls through to medium")
  (is (= "medium" (mk/resolve-create-priority nil nil))))

(deftest create-extra-tags-golden
  (is (= ["wave:1" "epic:foo"]
         (mk/create-extra-tags ["wave:1" "priority-high" "scope:project:x" "epic:foo" 42 nil])))
  (is (= [] (mk/create-extra-tags nil)))
  (is (= [] (mk/create-extra-tags "not-a-coll"))))

(deftest create-card-content-golden
  (is (= {:task-type "kanban" :title "t" :status "todo" :priority "medium"
          :created "now" :started nil :context nil}
         (mk/create-card-content {:title "t" :status "todo" :priority "medium" :now "now"})))
  (is (= "body"
         (:description (mk/create-card-content {:title "t" :description "body" :now "n"})))))

(deftest create-card-tags-golden
  (let [base (mk/create-card-tags "todo" "medium" "proj" nil nil)]
    (is (vector? base))
    (is (= base (vec (distinct base))))
    (is (= (conj base "idempotency:k")
           (mk/create-card-tags "todo" "medium" "proj" nil "idempotency:k")))))

(defspec extra-tags-never-leak-reserved-prefixes 100
  (prop/for-all [tags (gen/vector (gen/one-of [gen/string-alphanumeric
                                              (gen/fmap #(str "priority-" %) gen/string-alphanumeric)
                                              (gen/fmap #(str "scope:project:" %) gen/string-alphanumeric)
                                              gen/small-integer]))]
    (let [out (mk/create-extra-tags tags)]
      (and (every? string? out)
           (not-any? #(or (str/starts-with? % "priority-")
                          (str/starts-with? % "scope:project:")) out)))))

(defspec card-tags-are-distinct 100
  (prop/for-all [extras (gen/vector gen/string-alphanumeric)
                 idem   (gen/one-of [(gen/return nil) gen/string-alphanumeric])]
    (let [tags (mk/create-card-tags "todo" "medium" "proj" extras idem)]
      (= tags (vec (distinct tags))))))
