(ns hive-mcp.dispatch.handler-test
  "The predicate that decides what may be dispatched.

   Its whole reason to exist is the VAR case, so the tests that matter are the
   discriminating ones: a var is accepted (that is the rebind seam), and the
   things `ifn?` would have let through are still refused."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.dispatch.handler :as dispatch]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- a-handler [_args] :answered)

(defmulti a-multi-handler :kind)
(defmethod a-multi-handler :default [_] :answered)

(def ^:private not-callable
  "A var whose value is data, to prove the var arm checks the VALUE."
  42)

(deftest a-plain-function-is-a-handler
  (is (dispatch/handler? a-handler))
  (is (dispatch/handler? (fn [_] nil)))
  (is (dispatch/handler? a-multi-handler)
      "a multimethod is invocable, and some handlers are multimethods"))

(deftest a-var-is-a-handler-and-that-is-the-point
  (testing "the rebind seam: registering #'f is what lets a reload reach dispatch"
    (is (dispatch/handler? #'a-handler))
    (is (dispatch/handler? #'a-multi-handler)
        "a var holding a multimethod: fn? is false for BOTH halves of this")))

(deftest the-predicate-is-narrower-than-ifn
  (testing "everything ifn? would admit that is not a handler stays refused"
    (doseq [x [:a-keyword {:a 1} #{:a} [:a] "a string" 42 nil]]
      (is (not (dispatch/handler? x))
          (str (pr-str x) " is ifn? or truthy, and is not a handler"))
      (is (or (not (ifn? x)) (not (dispatch/handler? x)))
          "and where ifn? disagrees, this predicate is the stricter one"))))

(deftest a-var-holding-a-non-callable-is-not-a-handler
  (testing "var? alone is not enough; the current VALUE has to be invocable"
    (is (not (dispatch/handler? #'not-callable)))))

(deftest current-reads-through-at-call-time
  (testing "a var resolves to whatever it holds NOW, which is the whole seam"
    (is (identical? a-handler (dispatch/current #'a-handler)))
    (is (identical? a-handler (dispatch/current a-handler))
        "and a plain fn is its own current value"))
  (testing "rebinding the var changes what current reads, without re-registering"
    (let [held #'a-handler]
      (is (= :answered ((dispatch/current held) {})))
      (with-redefs [a-handler (fn [_] :rebound)]
        (is (= :rebound ((dispatch/current held) {}))
            "the registry kept the same var and still saw the new value")
        (is (= :rebound (held {}))
            "and invoking the var directly agrees, which is the seam working")))))
