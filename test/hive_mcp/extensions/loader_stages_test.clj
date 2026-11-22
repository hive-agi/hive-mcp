(ns hive-mcp.extensions.loader-stages-test
  "Golden + property tests for the pure stages `loader/load-extensions!` is
   decomposed into (init-namespace selection, init-result summary, result map)."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.extensions.loader :as loader]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private manifests
  [{:addon/id "a" :addon/init-ns "x.a"}
   {:addon/id "b" :addon/init-ns "x.b"}])

(deftest select-init-namespaces-golden
  (is (= '[x.a x.b y.c]
         (loader/select-init-namespaces manifests false '[x.b y.c]))
      "legacy path: manifests first, hardcoded appended, deduplicated")
  (is (= '[y.c]
         (loader/select-init-namespaces manifests true '[x.b y.c]))
      "composer ran: only hardcoded leftovers not covered by manifests")
  (is (= [] (loader/select-init-namespaces nil true [])))
  (is (= '[z] (loader/select-init-namespaces nil false '[z z]))))

(deftest summarize-init-results-golden
  (is (= {:successful-ns '#{x.a x.c x.d} :init-total 5}
         (loader/summarize-init-results
          {'x.a {:total 2} 'x.b nil 'x.c {:total 3} 'x.d {}})))
  (is (= {:successful-ns #{} :init-total 0}
         (loader/summarize-init-results {}))))

(deftest load-result-golden
  (is (= {:registered [:a :b]
          :total 7
          :sources {:initializers 3 :classpath-manifests 2
                    :mount-compose 1 :manifest-fallback 0}}
         (loader/load-result [:a :b] 7 3 2 1 0)))
  (is (vector? (:registered (loader/load-result nil 0 0 0 0 0)))))

(defspec select-init-namespaces-is-duplicate-free 100
  (prop/for-all [ns-names (gen/vector (gen/elements '[a b c d e]))
                 hard     (gen/fmap vec (gen/set (gen/elements '[a b c d e])))
                 mounted? gen/boolean]
    (let [ordered (mapv (fn [s] {:addon/id (str s) :addon/init-ns (str s)}) ns-names)
          out     (loader/select-init-namespaces ordered mounted? hard)]
      (and (= out (vec (distinct out)))
           (or (not mounted?)
               (empty? (filter (set ns-names) out)))))))

(defspec summarize-total-is-sum-of-totals 100
  (prop/for-all [totals (gen/vector (gen/one-of [(gen/return nil) gen/nat]))]
    (let [results (into {} (map-indexed (fn [i t] [(symbol (str "n" i)) (when t {:total t})]) totals))
          {:keys [init-total successful-ns]} (loader/summarize-init-results results)]
      (and (= init-total (reduce + 0 (keep identity totals)))
           (= (count successful-ns) (count (filter some? totals)))))))
