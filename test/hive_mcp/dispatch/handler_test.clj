(ns hive-mcp.dispatch.handler-test
  "The predicate that decides what may be dispatched.

   Its whole reason to exist is the VAR case, so the tests that matter are the
   discriminating ones: a var is accepted (that is the rebind seam), and the
   things `ifn?` would have let through are still refused."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.dispatch.handler :as dispatch]
            [hive-mcp.tools.consolidated.config :as config-tool]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- a-handler [_args] :answered)

(defmulti a-multi-handler :kind)
(defmethod a-multi-handler :default [_] :answered)

(def ^:private not-callable
  "A var whose value is data, to prove the var arm checks the VALUE."
  42)

(def ^:private an-alias
  "A var whose value is ANOTHER VAR — the shape an alias re-export takes once
   both the alias and the table that names it are var-quoted:

     (def has-plan? #'pred/has-plan?)     ; the alias
     {:has-plan? #'has-plan?}             ; the table entry

   Two seams stacked, not an abuse."
  #'a-handler)

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

(deftest current-follows-a-chain-of-vars-to-a-fixed-point
  (testing "one deref is not enough once an alias is also a var"
    (is (identical? a-handler (dispatch/current #'an-alias))
        "table entry -> alias -> function, resolved in one call")
    (is (identical? a-handler (dispatch/current an-alias))
        "and entering the chain one link down lands in the same place"))
  (testing "a chain still reads through at CALL time, at every link"
    (let [held #'an-alias]
      (is (= :answered ((dispatch/current held) {})))
      (with-redefs [a-handler (fn [_] :rebound)]
        (is (= :rebound ((dispatch/current held) {}))
            "a reload at the END of the chain reaches a reader holding its HEAD")))))

(deftest handler?-accepts-a-var-chain
  (testing "the predicate agrees with current, because it is stated through it"
    (is (dispatch/handler? #'an-alias))
    (is (dispatch/handler? an-alias))))

(deftest a-cyclic-var-chain-is-refused-rather-than-hanging
  (testing "the hop bound turns a pathological cycle into a refusal"
    (let [ns' (the-ns 'hive-mcp.dispatch.handler-test)
          v   (intern ns' 'a-cyclic-var)]
      (try
        (alter-var-root v (constantly v))
        (is (var? (dispatch/current v))
            "current gives up and hands back a var, still un-dereferenced")
        (is (not (dispatch/handler? v))
            "so the caller's own gate reports an uninvocable handler")
        (finally
          (ns-unmap ns' 'a-cyclic-var))))))

(defn- compose
  "A stand-in for `build-middleware-chain`: wrap `h` in a closure the way the
   real chain does. The point under test is the CAPTURE, which is identical in
   both, so this exercises it without dragging the guard, nats and piggyback
   middleware into a unit test."
  [h]
  (fn [args] {:wrapped (h args)}))

(deftest a-chain-built-over-a-value-cannot-see-a-reload
  (testing "the defect, stated as a passing test so it cannot be argued with"
    (let [chain (compose a-handler)]
      (is (= {:wrapped :answered} (chain {})))
      (with-redefs [a-handler (fn [_] :rebound)]
        (is (= {:wrapped :answered} (chain {}))
            "the closure froze the VALUE at build time, so the rebind is invisible: this is exactly what a reloaded handler namespace looks like from dispatch")))))

(deftest a-chain-built-over-a-var-does-see-a-reload
  (testing "the fix, measured against the same chain shape"
    (let [chain (compose #'a-handler)]
      (is (= {:wrapped :answered} (chain {})))
      (with-redefs [a-handler (fn [_] :rebound)]
        (is (= {:wrapped :rebound} (chain {}))
            "the closure holds the VAR, which dereferences on invoke, so the reload reaches dispatch with no table rebuild")))))

(deftest the-config-tool-registers-its-handler-by-var
  (testing "pilot for the fleet-wide conversion: one tool, converted and checked"
    (let [h (:handler config-tool/tool-def)]
      (is (var? h)
          "a tool that registers by value cannot pick up a reload of its own handler namespace")
      (is (dispatch/handler? h)
          "and the six gates that guard dispatch must accept it, or the conversion breaks the tool instead of freeing it"))))
