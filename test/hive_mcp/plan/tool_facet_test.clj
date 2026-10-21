(ns hive-mcp.plan.tool-facet-test
  "Facets for the plan-to-kanban tool: a compute-waves trifecta over generated
   ACYCLIC step graphs (the wave of a step is one more than its deepest
   dependency), and goldens over plan-to-kanban / handle-plan-to-kanban /
   handle-plan-schema with the IKanbanWrite port satisfied by a recording stub
   registered through hive-spi's kanban registry. Plans are read from a temp
   file (the :plan-path route), so no memory store is needed. KG edge ids are
   random, so the goldens keep only their count."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [use-fixtures]]
            [hive-mcp.plan.field-registry :as fr]
            [hive-mcp.plan.tool :as tool]
            [hive-schemas.test :as hst]
            [hive-spi.kanban :as kanban]
            [hive-spi.kanban.registry :as kanban-registry]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord RecordingKanbanWrite [created]
  kanban/IKanbanWrite
  (transition! [_ _request] {:ok {}})
  ;; Faithful to the port: success is {:ok {:id ..}}, refusal an {:err ..} value.
  (create-task! [_ request]
    (if (= "REJECT" (:title request))
      {:err {:error :kanban/rejected :message "title refused"}}
      {:ok {:id (str "task-" (count (swap! created conj request)))}})))

(def ^:private created (atom []))

(defn- kanban-fixture [f]
  (let [prior (when (kanban-registry/registered? :IKanbanWrite)
                (kanban-registry/provider :IKanbanWrite))
        saved-fields @@#'fr/registry]
    (reset! created [])
    (fr/reset-registry!)
    (kanban-registry/register! :IKanbanWrite (->RecordingKanbanWrite created))
    (try (f)
         (finally
           (reset! @#'fr/registry saved-fields)
           (if prior
             (kanban-registry/register! :IKanbanWrite prior)
             (kanban-registry/unregister! :IKanbanWrite))))))

(use-fixtures :each kanban-fixture)

;; -----------------------------------------------------------------------------
;; compute-waves over acyclic graphs
;; -----------------------------------------------------------------------------

(def ^:private ids ["s1" "s2" "s3" "s4" "s5"])

;; Step i may depend only on steps before it, so the graph is a DAG; picks are
;; indices reduced modulo i. Some draws give independent roots, some chains.
(def ^:private DagSteps
  [:sequential {:gen/schema [:tuple [:int {:min 0 :max 5}]
                             [:vector {:min 5 :max 5} [:vector {:max 2} [:int {:min 0 :max 4}]]]]
                :gen/fmap (fn [[n picks]]
                            (vec (for [i (range n)]
                                   {:id (ids i)
                                    :depends-on (if (zero? i)
                                                  []
                                                  (vec (distinct (map #(ids (mod % i)) (picks i)))))})))}
   [:map [:id :string] [:depends-on [:vector :string]]]])

(defn- expected-waves [steps]
  (reduce (fn [acc {:keys [id depends-on]}]
            (assoc acc id (if (empty? depends-on) 0 (inc (apply max (map acc depends-on))))))
          {} steps))

(hst/deftrifecta-from-schema compute-waves-dag-contract
  hive-mcp.plan.tool/compute-waves
  {:in [:cat DagSteps]
   :out (-> (m/function-schemas) (get-in ['hive-mcp.plan.tool 'compute-waves :schema]) m/children second)
   :rel (fn [[steps] out] (= out (expected-waves steps)))
   :classify (fn [_ out] (if (some pos? (vals out)) :layered :flat))
   :classify-domain #{:layered :flat}
   :golden-path "test/golden/hive-mcp/plan/tool-compute-waves-dag.edn"})

;; -----------------------------------------------------------------------------
;; plan-to-kanban with the kanban write port stubbed
;; -----------------------------------------------------------------------------

(defn- plan-file [content]
  (let [f (doto (java.io.File/createTempFile "plan-tool-facet" ".md") (.deleteOnExit))]
    (clojure.core/spit f content)
    (str f)))

(defn- view
  "Decode an MCP response and drop what varies per run (file path, edge ids)."
  [resp]
  (let [body (json/read-str (:text resp) :key-fn keyword)]
    (if (map? body)
      (-> body
          (dissoc :plan-id :kg-edges)
          (update :kg-warnings #(some-> % vec)))
      {:error-text body})))

(defn- error-view
  "An MCP error reduced to its flag, first line and Errors line: the
   Validation line in between echoes the plan with its temp-file memory id."
  [resp]
  (let [lines (str/split-lines (:text resp))]
    {:isError (boolean (:isError resp))
     :head (first lines)
     :errors (first (filter #(str/starts-with? % "Errors:") lines))}))

(golden/deftest-golden plan-to-kanban-golden
  "test/golden/hive-mcp/plan/tool-plan-to-kanban.edn"
  (let [ok (tool/plan-to-kanban
            nil
            :directory "/tmp/plan-tool-facet"
            :plan-path (plan-file (str "{:id \"p\" :title \"P\" :decision-id \"d1\""
                                       " :steps [{:id \"a\" :title \"A\" :priority :high :files [\"x.clj\"]}"
                                       " {:id \"b\" :title \"B\" :depends-on [\"a\"]}"
                                       " {:id \"c\" :title \"C\" :depends-on [\"a\" \"b\"]}]}")))
        requests @created
        user-waves (tool/plan-to-kanban
                    nil
                    :directory "/tmp/plan-tool-facet"
                    :plan-path (plan-file (str "{:id \"p2\" :title \"Pinned\" :waves {:A [\"b\"] :B {:parallel [\"a\"]}}"
                                               " :steps [{:id \"a\" :title \"A\"} {:id \"b\" :title \"B\"}]}")))
        rejected (tool/plan-to-kanban
                  nil
                  :directory "/tmp/plan-tool-facet"
                  :plan-path (plan-file "{:id \"p3\" :title \"R\" :steps [{:id \"a\" :title \"REJECT\"}]}"))
        cyclic (tool/plan-to-kanban
                nil
                :plan-path (plan-file (str "{:id \"p4\" :title \"C\" :steps [{:id \"a\" :title \"A\" :depends-on [\"b\"]}"
                                           " {:id \"b\" :title \"B\" :depends-on [\"a\"]}]}")))]
    {:ok (update (view ok) :edge-count pos?)
     :requests requests
     :user-waves (select-keys (view user-waves) [:waves :max-wave :task-count])
     :rejected (error-view rejected)
     :cyclic (:isError cyclic)
     :missing-file (:isError (tool/plan-to-kanban nil :plan-path "/nonexistent/plan.md"))
     :handler-blank (error-view (tool/handle-plan-to-kanban {}))}))

(golden/deftest-golden handle-plan-schema-golden
  "test/golden/hive-mcp/plan/tool-handle-plan-schema.edn"
  (let [body (json/read-str (:text (tool/handle-plan-schema {})) :key-fn keyword)]
    (select-keys body [:success :required :step-required :step-enums])))

(golden/deftest-golden plan-contract-golden
  "test/golden/hive-mcp/plan/tool-plan-contract.edn"
  {:refs (tool/plan-contract
          {:type "object" :required ["id" "steps"]
           :properties {:steps {:type "array" :items {:$ref "#/definitions/Step"}}}
           :definitions {:Step {:type "object" :required ["id"]
                                :properties {:priority {:anyOf [{:type "null"} {:enum ["high" "low"]}]}
                                             :estimate {:enum ["s"]}}}}})
   :string-keys (tool/plan-contract {"required" ["a"] "properties" {"steps" {"items" {"required" ["x" "y"]}}}})
   :empty (tool/plan-contract {})})
