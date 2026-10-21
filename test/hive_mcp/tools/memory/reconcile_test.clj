(ns hive-mcp.tools.memory.reconcile-test
  "Facets for the read-only `memory reconcile` verb.

   reconcile-response is pure over its IReconcileSources port: a trifecta
   from its contract (the ReconcileSources generator in
   hive-mcp.tools.memory.schema yields a stub listing store [a b] and vector
   [b c]) plus a golden over the limit arms. handle-reconcile gets a golden
   with sources installed through set-sources! (the verb's own seam)."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.memory.reconcile :as core]
            [hive-mcp.tools.memory.reconcile :as rec]
            [hive-mcp.tools.memory.schema :as ms]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [t] (rec/clear-sources!) (try (t) (finally (rec/clear-sources!)))))

(deftrifecta-from-schema reconcile-response-is-not-wired-without-sources
  hive-mcp.tools.memory.reconcile/reconcile-response
  {:in        [:cat [:maybe ms/ReconcileSources] [:maybe [:or [:int {:min -2 :max 5}] [:enum "3" "x" "0"]]]]
   :out       ms/ReconcileResponse
   :rel       (fn [[sources _] out]
                (= (:status out) (if (nil? sources) "not-wired" "ok")))
   :num-tests 200})

(defn- sources [store vector]
  (reify core/IReconcileSources
    (store-ids [_] store)
    (vector-ids [_] vector)))

(def ^:private drift (sources ["a" "b" "c" "d"] ["b" "x" "y" nil]))

(deftest-golden reconcile-response-arms
  "test/golden/tools/memory/reconcile-response.edn"
  {:not-wired    (rec/reconcile-response nil 5)
   :default      (rec/reconcile-response drift nil)
   :limit-1      (rec/reconcile-response drift 1)
   :limit-str    (rec/reconcile-response drift "2")
   :limit-bad    (rec/reconcile-response drift "x")
   :limit-zero   (rec/reconcile-response drift 0)
   :clean        (rec/reconcile-response (sources ["a"] ["a"]) 10)})

(deftest-golden handle-reconcile-through-installed-sources
  "test/golden/tools/memory/reconcile-handler.edn"
  (let [before (json/read-str (:text (rec/handle-reconcile {})) :key-fn keyword)]
    (rec/set-sources! drift)
    {:before    before
     :installed (json/read-str (:text (rec/handle-reconcile {:limit 2})) :key-fn keyword)}))

(deftest set-and-clear-return-their-documented-values
  (is (identical? drift (rec/set-sources! drift)))
  (is (nil? (rec/clear-sources!))))
