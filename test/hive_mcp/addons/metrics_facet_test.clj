(ns hive-mcp.addons.metrics-facet-test
  "Facets for hive-mcp.addons.metrics: the pure outcome / label rules
   (property) and timed-call / wrap-handler through the *recorder* port bound
   to a recording stub (golden)."
  (:require [clojure.string :as str]
            [hive-mcp.addons.metrics :as am]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema outcome hive-mcp.addons.metrics/outcome
  {:in  [:or :nil :string
         [:map [:isError [:enum true false "true"]]]
         [:map [:success? [:enum true false nil]]]
         [:map [:isError [:enum true false]] [:success? [:enum true false]]]]
   :out [:enum :ok :error]
   :rel (fn [in out]
          (= out (if (and (map? in) (or (true? (:isError in)) (false? (:success? in))))
                   :error :ok)))
   :classify (fn [_ out] out)
   :classify-domain #{:ok :error}
   :num-tests 200})

(deftrifecta-from-schema label hive-mcp.addons.metrics/label
  {:in  [:or :nil :keyword :qualified-keyword :symbol :string]
   :out :string
   :rel (fn [in out]
          (cond (nil? in) (= "-" out)
                (keyword? in) (and (= (str in) (str ":" out)) (not (str/starts-with? out ":")))
                :else (= (str in) out)))
   :num-tests 200})

(defn- with-stub
  "Run f with *recorder* answering a recorder that logs [k outcome]."
  [f]
  (let [seen (atom [])]
    (binding [am/*recorder* (constantly (fn [k _ns oc] (swap! seen conj [k oc])))]
      {:value (f) :recorded @seen})))

(defn- trace []
  {:ok (with-stub #(am/timed-call [:addon :start] (constantly {:success? true})))
   :error-answer (with-stub #(am/timed-call [:addon :tool "x"] (constantly {:isError true})))
   :thrown (with-stub #(try (am/timed-call [:addon :stop] (fn [] (throw (ex-info "down" {}))))
                            (catch clojure.lang.ExceptionInfo e (ex-message e))))
   :wrapped (with-stub #(let [h (am/wrap-handler [:addon :handler] (fn [a b] (+ a b)))]
                          [(h 1 2) (h 3 4)]))
   :no-recorder (binding [am/*recorder* (constantly nil)]
                  (let [h (fn [x] x)]
                    {:value (am/timed-call [:addon :start] (constantly :plain))
                     :wrap-is-identity? (identical? h (am/wrap-handler [:addon :h] h))}))})

(golden/deftest-golden timed-call-golden
  "test/golden/hive-mcp/addons/metrics-timed-call.edn"
  (trace))
