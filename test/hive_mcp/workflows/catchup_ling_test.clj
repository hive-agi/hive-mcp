(ns hive-mcp.workflows.catchup-ling-test
  "Facets for ling-catchup. Its seam is the extension var it resolves at call
   time, hive-mcp.extensions.context/ling-catchup, which no hive-mcp jar
   ships. The tests supply that extension: a namespace created for the test
   whose var answers a string, a structured map, or nothing, chosen per call
   through the opts' :task. The namespace is removed afterwards (it was absent
   before), so no other test sees an extension. Nothing existing is redefined."
  (:require [clojure.string :as str]
            [clojure.test :refer [use-fixtures]]
            [hive-mcp.workflows.catchup-ling :as catchup]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private ext-ns 'hive-mcp.extensions.context)

(defn- extension-answer
  "The stub extension: what ling-catchup's extension returns, keyed by the
   task so one generated opts map picks one of the documented shapes."
  [opts]
  (case (:task opts)
    "string" "AXIOM: never push.\nContext line."
    "long" (apply str (repeat 3000 "word "))
    "structured" {:preset "PRESET" :axioms "AXIOMS" :context "CTX" :history "HIST"}
    "partial" {:axioms "only axioms"}
    "empty" ""
    "map-without-layers" {:other 1}
    nil))

(defn- extension-fixture [f]
  (let [existed? (find-ns ext-ns)]
    (when-not existed?
      (create-ns ext-ns)
      (intern ext-ns 'ling-catchup extension-answer))
    (try (f) (finally (when-not existed? (remove-ns ext-ns))))))

(use-fixtures :each extension-fixture)

(def ^:private Task
  [:enum "string" "long" "structured" "partial" "empty" "map-without-layers" "none"])

;; Generated with a :task on every non-nil draw (an optional :task leaves half
;; the draws answering nothing); nil and a task-less map still occur.
(def ^:private Opts
  [:maybe {:gen/schema [:tuple [:map
                                [:directory {:optional true} [:maybe [:enum "/repo"]]]
                                [:task Task]
                                [:token-budget {:optional true} [:maybe [:enum 5 100 8000]]]]
                        [:enum 0 1 2 3 4 5 6 7]]
           :gen/fmap (fn [[m arm]] (case arm 0 nil 1 (dissoc m :task) m))}
   [:map
    [:directory {:optional true} [:maybe [:enum "/repo"]]]
    [:task {:optional true} [:maybe Task]]
    [:token-budget {:optional true} [:maybe [:enum 5 100 8000]]]]])

(def ^:private answers-nothing
  "Tasks for which the stub extension answers nil, \"\" or a map without
   any context layer, all of which ling-catchup turns into nil."
  #{nil "empty" "map-without-layers" "none"})

(deftrifecta-from-schema ling-catchup-contract
  hive-mcp.workflows.catchup-ling/ling-catchup
  {:in [:cat Opts]
   :out (-> (m/function-schemas) (get-in ['hive-mcp.workflows.catchup-ling 'ling-catchup :schema])
            m/children second)
   :rel (fn [[opts] out]
          (let [task (:task opts)
                answer (extension-answer opts)]
            (cond
              ;; contains?, not set-as-fn: nil is a member and would read falsy
              (contains? answers-nothing task) (nil? out)
              (= "structured" task) (and (str/includes? out "PRESET")
                                         (<= (count out) catchup/max-context-chars))
              (= "partial" task) (= "only axioms\n\n" out)
              ;; string answers: a prefix of the extension's text, within the cap
              :else (and (string? out) (pos? (count out))
                         (<= (count out) catchup/max-context-chars)
                         (str/starts-with? answer (str/trimr out))))))
   :classify (fn [_ out] (if out :context :nothing))
   :classify-domain #{:context :nothing}
   :golden-path "test/golden/hive-mcp/workflows/catchup-ling.edn"})

(golden/deftest-golden ling-catchup-shapes-golden
  "test/golden/hive-mcp/workflows/catchup-ling-shapes.edn"
  (let [run (fn [opts] (let [out (catchup/ling-catchup opts)]
                         (if (and out (> (count out) 120))
                           {:chars (count out) :head (subs out 0 40)}
                           out)))]
    {:string (run {:task "string"})
     :long-default-budget (run {:task "long"})
     :long-small-budget (run {:task "long" :token-budget 5})
     :structured (run {:task "structured" :token-budget 8000})
     :structured-tight (run {:task "structured" :token-budget 5})
     :empty (run {:task "empty"})
     :no-extension-answer (run nil)}))
