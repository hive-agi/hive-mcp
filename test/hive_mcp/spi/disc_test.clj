;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.spi.disc-test
  "The disc port answers the same three questions in three states: an
   installed implementation, a late-bound host namespace, and nothing at all.
   The third state must be HONEST: no staleness weight, no propagation tally,
   and an empty stale-file list, never a pretend one.

   `soft/*resolve*` is bound rather than redefining a host var, so the
   absent-namespace case describes the classpath hive-memory leaves behind
   (HIVE-KERNEL K1/E6c)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.spi.disc :as port]
            [hive-mcp.swarm.adapters.soft :as soft]))

(defn- clean-registry [f]
  (port/uninstall!)
  (try (f) (finally (port/uninstall!))))

(use-fixtures :each clean-registry)

(def stub-staleness-values
  "Stands in for the host's `base-staleness-values`, which is DATA held in a
   var: the port must deref it rather than call it."
  {:hash-mismatch 5.0 :git-commit 2.0 :time-decay 0.5})

(defn- stub-resolver
  "Stands in for `requiring-resolve`. The data name resolves to a VAR, the two
   function names to fns that record their call."
  [seen]
  (fn [sym]
    (when (= "hive-mcp.knowledge-graph.disc" (namespace sym))
      (case (name sym)
        "base-staleness-values" #'stub-staleness-values
        "propagate-staleness!"  (fn [& args]
                                  (swap! seen conj (vec (cons 'propagate args)))
                                  {:propagated 2 :skipped 0 :errors 0 :grounded 1})
        "top-stale-files"       (fn [& args]
                                  (swap! seen conj (vec (cons 'top-stale args)))
                                  [{:path "a.clj" :score 0.9}])
        nil))))

(deftest an-absent-disc-namespace-answers-honestly
  (binding [soft/*resolve* (constantly nil)]
    (port/reset-cache!)
    (is (nil? (port/staleness-value :hash-mismatch))
        "no weight is known, and nil says so")
    (is (nil? (port/propagate-staleness! "f.clj" 5.0 :hash-mismatch))
        "nothing was propagated, and no tally is invented")
    (is (= [] (port/top-stale-files {:n 5 :project-id "p"}))
        "an empty list, never a pretend one")))

(deftest the-port-late-binds-to-the-host-namespace-by-symbol
  (let [seen (atom [])]
    (binding [soft/*resolve* (stub-resolver seen)]
      (port/reset-cache!)
      (testing "the data var is dereferenced and read by key"
        (is (= 5.0 (port/staleness-value :hash-mismatch)))
        (is (nil? (port/staleness-value :no-such-reason))))
      (testing "propagate-staleness! passes its three arguments and returns the host tally"
        (is (= {:propagated 2 :skipped 0 :errors 0 :grounded 1}
               (port/propagate-staleness! "f.clj" 5.0 :hash-mismatch)))
        (is (= '[propagate "f.clj" 5.0 :hash-mismatch] (first @seen))))
      (testing "top-stale-files turns the opts map into the host's kwargs"
        (is (= [{:path "a.clj" :score 0.9}]
               (port/top-stale-files {:n 5 :project-id "hive-mcp"})))
        (is (= '[top-stale :n 5 :project-id "hive-mcp"] (second @seen))))
      (testing "an opt the caller did not set is not passed, so the host keeps owning its default"
        (port/top-stale-files {:n 3})
        (is (= '[top-stale :n 3] (nth @seen 2)))))))

(deftest an-installed-implementation-wins-over-the-host-namespace
  (let [host (atom [])
        calls (atom [])]
    (binding [soft/*resolve* (stub-resolver host)]
      (port/reset-cache!)
      (port/install! (reify port/IDiscKnowledge
                       (-staleness-value [_ reason] (swap! calls conj [:value reason]) 1.0)
                       (-propagate-staleness! [_ p v r] (swap! calls conj [:propagate p v r]) {:propagated 0})
                       (-top-stale-files [_ opts] (swap! calls conj [:top opts]) [])))
      (is (= 1.0 (port/staleness-value :hash-mismatch)))
      (is (= {:propagated 0} (port/propagate-staleness! "f.clj" 1.0 :hash-mismatch)))
      (is (= [] (port/top-stale-files {:n 5})))
      (is (= [[:value :hash-mismatch]
              [:propagate "f.clj" 1.0 :hash-mismatch]
              [:top {:n 5}]]
             @calls))
      (is (= [] @host) "nothing reached the host namespace while an impl was installed")
      (port/uninstall!)
      (is (= 5.0 (port/staleness-value :hash-mismatch))
          "uninstall! falls back to the late-bound host var"))))
