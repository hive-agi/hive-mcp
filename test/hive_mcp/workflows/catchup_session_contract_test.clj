(ns hive-mcp.workflows.catchup-session-contract-test
  "Contracts and hive-test facets for the catchup FSM handlers.

   Every handler is driven through stub resources (the territory) under
   instrumentation: query-memory's two [:or] arms are each produced and checked
   to fail the other, delivery's context-store and piggyback legs are observed
   through recording stubs, and the full FSM gets a golden over its default
   (no build-response-fn) summary."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.workflows.catchup-session :as cs]
            [hive-mcp.workflows.schema :as wfs]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  (set (for [s '[handle-start handle-scope-resolve handle-query-memory handle-transform
                 handle-addon-pass handle-maintenance handle-deliver handle-end handle-error
                 compile-catchup run-catchup run-catchup-session]]
         (ns-resolve 'hive-mcp.workflows.catchup-session s))))

(use-fixtures :each
  (fn [f]
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try (f) (finally (mi/unstrument!)))))

(defn- boom [& _] (throw (ex-info "boom" {})))

(defn- out-schema [v]
  (-> (m/function-schemas) (get-in ['hive-mcp.workflows.catchup-session v :schema])
      m/schema m/children second))

(defn- entry [id] {:id id :content (str "c-" id)})

(defn- resources []
  {:chroma-check-fn      (constantly true)
   :scope-fn             (constantly "proj")
   :project-name-fn      (constantly "Proj")
   :build-scopes-fn      (fn [n p] [n p])
   :query-fn             (fn [type _tags _pid _limit] [(entry type)])
   :query-axioms-fn      (constantly [(entry "ax")])
   :query-conventions-fn (fn [_ ax pc] [(entry (str "conv:" (count ax) ":" (count pc)))])
   :query-expiring-fn    (constantly [])
   :git-fn               (constantly {:branch "main"})
   :entry->meta-fns      {:axiom :id :catchup :id :priority :id :axiom-candidate :id}
   :permeate-fn          (constantly {:permeated 1})
   :tree-scan-fn         (constantly {:scanned true})
   :disc-decay-fn        (constantly {:updated 0 :skipped 0 :errors 0})})

(deftest start-and-scope
  (is (= {:directory "/d" :chroma-configured? true :error nil}
         (cs/handle-start {:chroma-check-fn (constantly 1) :directory "/d"} {})))
  (is (false? (:chroma-configured? (cs/handle-start {} {}))) "no check fn: not configured")
  (is (= {:directory "/d" :project-id "proj" :project-name "Proj" :scopes ["Proj" "proj"]}
         (cs/handle-scope-resolve (resources) {:directory "/d"})))
  (is (= {:project-id nil :project-name nil :scopes nil}
         (cs/handle-scope-resolve {:tree-scan-fn boom} {}))
      "a throwing tree scan is recovered; no directory resolves nothing"))

(deftest query-memory-arms
  (let [[ok-arm err-arm] (m/children (out-schema 'handle-query-memory))
        ok  (cs/handle-query-memory (resources) {:project-id "proj"})
        bad (cs/handle-query-memory (assoc (resources) :query-conventions-fn boom) {})]
    (is (= [(entry "ax")] (:axioms ok)))
    (is (= [(entry "conv:1:1")] (:conventions ok)) "conventions see the axiom and priority ids")
    (is (= [(entry "decision")] (:decisions ok)))
    (is (false? (:query-failed? ok)))
    (is (= [true "Query failed: boom"] ((juxt :query-failed? :error) bad)))
    (is (and (m/validate ok-arm ok) (not (m/validate err-arm ok))))
    (is (and (m/validate err-arm bad) (not (m/validate ok-arm bad))))
    (testing "no query fns: every category is an empty vector"
      (is (every? #(= [] %) (vals (select-keys (cs/handle-query-memory {} {})
                                               [:axioms :sessions :decisions :snippets :conventions])))))))

(deftest transform-addon-maintenance
  (let [out (cs/handle-transform (resources) {:directory "/d" :axioms [(entry "a")]
                                              :decisions [(entry "d")]})]
    (is (= ["a"] (:axioms-meta out)))
    (is (= ["d"] (:decisions-base out)))
    (is (= {:branch "main"} (:git-info out)))
    (is (= [] (:snippets-meta out))))
  (is (= {:x 1} (cs/handle-addon-pass {} {:x 1})) "no addon: pass-through")
  (is (= {:x 1 :addon-data 2} (cs/handle-addon-pass {:addon-fn (constantly {:addon-data 2})} {:x 1})))
  (is (= {:x 1} (cs/handle-addon-pass {:addon-fn (constantly nil)} {:x 1})))
  (let [out (cs/handle-maintenance {:permeate-fn boom} {})]
    (is (= {:permeated 0 :error "permeation failed"} (:permeation out)))
    (is (= {:updated 0 :skipped 0 :errors 1} (:disc-decay out)))))

(deftest deliver-legs
  (let [stored (atom []) piggy (atom nil) adopted (atom nil)
        res {:context-store-fn (fn [entries tags _ttl] (swap! stored conj tags) (str "ctx-" (count entries)))
             :piggyback-fn     (fn [caller entries refs] (reset! piggy [caller (count entries) refs]))
             :cursor-adopt-fn  (fn [pid proj] (reset! adopted [pid proj]))}
        out (cs/handle-deliver res {:project-id "proj" :axioms [(entry "a")]
                                    :priority-conventions [(entry "p")] :sessions []})]
    (is (= {:axioms "ctx-1" :priority-conventions "ctx-1"} (:context-refs out)))
    (is (true? (:piggyback-enqueued? out)))
    (is (= ["coordinator" 2 {:axioms "ctx-1" :priority-conventions "ctx-1"}] @piggy))
    (is (= "proj" (second @adopted))))
  (is (= {:context-refs nil :piggyback-enqueued? false} (cs/handle-deliver {} {})))
  (is (nil? (:context-refs (cs/handle-deliver {:context-store-fn boom} {:axioms [(entry "a")]})))
      "a failing context store degrades to no refs"))

(deftest terminals
  (is (= :resp (cs/handle-end {:build-response-fn (constantly :resp)} {:data {}})))
  (is (= {:decisions-meta ["d"] :conventions-meta nil :project-id "p"}
         (cs/handle-end {} {:data {:project-id "p" :decisions-base ["d"] :junk 1}})))
  (is (= "e" (cs/handle-error {:error-response-fn identity} {:error "e"})))
  (is (= "d" (cs/handle-error {:error-response-fn identity} {:data {:error "d"}})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Catchup workflow error"
                        (cs/handle-error {} {:error "e" :data {}}))))

(deftest-golden catchup-default-summary
  "test/golden/workflows/catchup-summary.edn"
  (cs/run-catchup-session (resources) {:directory "/d"}))

(deftest runners
  (is (m/validate wfs/CompiledFsm (cs/compile-catchup)))
  (is (= "proj" (:project-id (cs/run-catchup (cs/compile-catchup) (assoc (resources) :directory "/d")))))
  (testing "chroma not configured routes to the error handler"
    (is (= :no-chroma (cs/run-catchup-session {:error-response-fn (constantly :no-chroma)}))))
  (testing "the contract rejects a non-map opts"
    (is (thrown? Exception (cs/run-catchup-session (resources) "opts")))))
