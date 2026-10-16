(ns hive-mcp.diag.profiler-test
  "A fresh clj-async-profiler IProfiler is idle: it reports itself inactive,
   and stopping it without a session is `err :diag/profiler-not-running`,
   never a throw. Starting is not driven here (it would attach an agent)."
  (:require [clojure.test :refer [deftest is]]
            [hive-dsl.result :as r]
            [hive-mcp.diag.profiler :as profiler]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.diag.ports :as p]))

;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(deftrifecta-from-schema make-profiler-trifecta profiler/make-profiler
  {:in  [:cat]
   :out [:and [:fn #(satisfies? p/IProfiler %)] [:map]]
   :rel (fn [_ prof]
          (and (false? (p/profiler-active? prof))
               (= :diag/profiler-not-running (:error (p/stop-profiling! prof)))))
   :num-tests 5})

(deftest each-profiler-owns-its-session
  (let [a (profiler/make-profiler)
        b (profiler/make-profiler)]
    (is (not (identical? a b)))
    (is (r/err? (p/stop-profiling! a)))
    (is (false? (p/profiler-active? b)))))
