(ns hive-mcp.test.shim-facets
  "Facet support for the swarm compat shims (hive-mcp.swarm.*, hive-mcp.hivemind.*).

   A shim's public fn delegates to a var of hive-agent or hive-datascript,
   resolved through hive-mcp.swarm.delegate. The test/ tree runs without
   those libraries, so a call raises the delegate's :swarm/addon-missing
   ex-info, whose data NAMES the addon var the shim reached for. That is the
   observable a shim test can pin with no addon and no with-redefs:

     delegation-table  {fn-name outcome} over each fn's contract-generated
                       arguments, plus the load-time values: the golden facet
     call-gen          [fn-name args] drawn from the fns' own m/=> input
                       schemas: the property facet (the var reached does not
                       depend on the arguments)
     delegation-check  + delegation-mutants: the mutation facet
     outcome           what one call does: {:ns :var} of the addon var it
                       reached, {:returned type}, or {:threw class}

   With either swarm library present (the swarm suite, test-swarm/), these
   facets step aside; that suite drives the real behaviour."
  (:require [clojure.test :as t]
            [clojure.test.check.generators :as gen]
            [hive-mcp.swarm.delegate :as delegate]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def swarm-libs
  "One namespace from each library the swarm shims resolve into."
  ["hive-agent.swarm.claim.span" "hive-datascript.swarm.connection"])

(defn offline?
  "True when no swarm library is on the classpath: the mode in which a shim
   call is observable as the addon var it reached for."
  []
  (not-any? delegate/available? swarm-libs))

(defn contracted
  "Sorted fn names of SHIM-NS that carry an m/=> contract."
  [shim-ns]
  (vec (sort (keys (get (m/function-schemas) shim-ns)))))

(defn input-schemas
  "The :cat input schema of every arity of SHIM-NS/SYM's contract."
  [shim-ns sym]
  (let [s (m/schema (get-in (m/function-schemas) [shim-ns sym :schema]))
        arities (if (= :function (m/type s)) (m/children s) [s])]
    (mapv (comp first m/children) arities)))

(defn outcome
  "What calling F with ARGS does, as data: the addon var a shim reached
   ({:ns :var} of its addon-missing ex-info), {:returned <type>}, an
   unrelated {:exinfo msg}, or {:threw <class>}."
  [f args]
  (try
    {:returned (some-> (apply f args) type .getName)}
    (catch clojure.lang.ExceptionInfo e
      (let [d (ex-data e)]
        (if (:swarm/addon-missing d)
          (select-keys d [:ns :var])
          {:exinfo (ex-message e)})))
    (catch Throwable e
      {:threw (.getName (class e))})))

(defn call
  "Call SHIM-NS/SYM through its var, so a mutated root is what runs."
  [shim-ns sym args]
  (outcome @(ns-resolve shim-ns sym) args))

(defn sample-args
  "Seeded argument lists, a few per arity of SHIM-NS/SYM's contract."
  [shim-ns sym]
  (vec (mapcat #(mg/sample % {:size 4 :seed 11}) (input-schemas shim-ns sym))))

(defn load-time-values
  "{name value-type} of SHIM-NS's public non-fn vars: a delegated value is
   nil without its library."
  [shim-ns]
  (into (sorted-map)
        (for [[sym v] (ns-publics shim-ns)
              :when (not (or (:arglists (meta v)) (:macro (meta v))))]
          [(str sym) (some-> @v type .getName)])))

(defn delegation-table
  "{:calls {fn-name outcome} :values {name type}} for SHIM-NS. Each fn of
   SYMS is called with the first seeded sample of every arity's input
   schema; an fn whose arities disagree maps to the set of outcomes."
  [shim-ns syms]
  {:calls (into (sorted-map)
                (for [sym syms]
                  (let [outs (set (for [in (input-schemas shim-ns sym)]
                                    (call shim-ns sym (first (mg/sample in {:size 3 :seed 42})))))]
                    [(str sym) (if (= 1 (count outs)) (first outs) outs)])))
   :values (load-time-values shim-ns)})

(defn call-gen
  "Generator of [sym args]: a contracted fn of SHIM-NS from SYMS and an
   argument list drawn from one of its arities' input schemas."
  [shim-ns syms]
  (gen/bind (gen/elements syms)
            (fn [sym]
              (gen/tuple (gen/return sym)
                         (gen/one-of (mapv mg/generator (input-schemas shim-ns sym)))))))

(defn expected
  "The outcome of calling SYM offline: TARGET-NS/SYM unless OVERRIDES (keyed
   by symbol) names another."
  [target-ns overrides sym]
  (get overrides sym {:ns target-ns :var (name sym)}))

(defn reaches-expected?
  "Property body: offline, SHIM-NS/SYM called with ARGS has the expected
   outcome. Vacuously true with a swarm library present."
  [shim-ns target-ns overrides sym args]
  (or (not (offline?))
      (= (expected target-ns overrides sym) (call shim-ns sym args))))

(defn delegation-check
  "A zero-arg assertion fn for deftest-mutations: SHIM-NS/SYM, called with
   its contract-generated argument lists, reaches EXPECTED-OUTCOME."
  [shim-ns sym expected-outcome]
  (fn []
    (doseq [args (take 4 (sample-args shim-ns sym))]
      (t/is (= expected-outcome (call shim-ns sym args))
            (str shim-ns "/" sym " " (pr-str args))))))

(defn delegation-mutants
  "Mutants of a delegating shim fn: one that answers a value without
   delegating, one that reaches the same var in the WRONG library, one that
   swallows the call, and (when the shim has one) one that calls SIBLING
   instead."
  [shim-ns sym sibling]
  (cond-> [["answers a value without delegating" (constantly 0)]
           ["reaches the wrong library"
            (fn [& _] (throw (ex-info "x" {:swarm/addon-missing true :ns "hive-elsewhere.swarm" :var (name sym)})))]
           ["swallows the call" (fn [& _] nil)]]
    sibling (conj ["delegates to a sibling" (fn [& args] (apply @(ns-resolve shim-ns sibling) args))])))

(defn offline-golden
  "The golden value for a shim test: (F) offline. With a swarm library
   present the stored snapshot stands in (the swarm suite owns that mode)."
  [test-ns path f]
  (if (offline?)
    (f)
    (do (t/is true "a swarm library is on the classpath; the swarm suite drives the shim")
        (golden/read-golden (golden/anchor test-ns path)))))
