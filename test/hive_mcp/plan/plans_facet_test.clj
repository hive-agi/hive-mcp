(ns hive-mcp.plan.plans-facet-test
  "Facets for the plans collection. The pure helpers (count-plan-steps,
   high-abstraction-type?) get trifectas from their contracts; the
   collection-backed functions get a golden with both ports satisfied by
   injected stubs: the in-memory IVectorCollectionStore and a deterministic
   EmbeddingProvider. The fixture restores whatever store and provider were
   installed before."
  (:require [clojure.string :as str]
            [clojure.test :refer [use-fixtures]]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.plan.plans :as plans]
            [hive-mcp.protocols.vector :as vp]
            [hive-mcp.vectordb.memory-store :as memory-store]
            [hive-schemas.test :as hst]
            [hive-spi.embeddings.ports :as ports]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord StubEmbedder [dim]
  ports/EmbeddingProvider
  ;; Deterministic: the vector is a function of the text's hash, and always
  ;; `dim` long, so the store's dimension check holds.
  (embed-text [_ text]
    (let [h (hash text)] (mapv #(double (mod (bit-shift-right h %) 7)) (range dim))))
  (embed-batch [this texts] (mapv #(ports/embed-text this %) texts))
  (embedding-dimension [_] dim))

(defn- ports-fixture [f]
  (let [prior-store (vp/get-store)
        prior-provider (active/get-embedding-provider)]
    (vp/set-store! (memory-store/in-memory-store))
    (active/set-embedding-provider! (->StubEmbedder 4))
    (plans/reset-collection-cache!)
    (try (f)
         (finally
           (plans/reset-collection-cache!)
           (if prior-store (vp/set-store! prior-store) (vp/clear-store!))
           (if prior-provider
             (active/set-embedding-provider! prior-provider)
             (active/reset-embedding-provider!))))))

(use-fixtures :each ports-fixture)

(defn- out-schema [fsym]
  (-> (m/function-schemas) (get-in ['hive-mcp.plan.plans fsym :schema]) m/children second))

;; Content assembled from numbered-list lines, "## Step N" headers and prose,
;; so both counting heuristics and their max are exercised.
(def ^:private Line [:enum :numbered :paren :header :prose])

(defn- render-line [i kind]
  (case kind
    :numbered (str (inc i) ". item")
    :paren (str "  " (inc i) ") item")
    :header (str "## Step " (inc i))
    :prose "Some prose 3. not at line start"))

(def ^:private Content
  ;; one third each: nil, prose only (counts 0), and mixed lines
  [:or {:gen/schema [:tuple [:vector {:min 1 :max 6} Line] [:enum :absent :prose :mixed]]
        :gen/fmap (fn [[lines kind]]
                    (case kind
                      :absent nil
                      :prose (str/join "\n" (map-indexed render-line (map (constantly :prose) lines)))
                      :mixed (str/join "\n" (map-indexed render-line lines))))}
   :string :nil])

(defn- expected-steps [content]
  (when content
    (let [lines (str/split-lines content)
          numbered (count (filter #(or (str/includes? % ". item") (str/includes? % ") item")) lines))
          headers (count (filter #(str/starts-with? % "## Step") lines))]
      (max numbered headers))))

(hst/deftrifecta-from-schema count-plan-steps-contract
  hive-mcp.plan.plans/count-plan-steps
  {:in [:cat Content]
   :out (out-schema 'count-plan-steps)
   :rel (fn [[content] out] (= out (expected-steps content)))
   :classify (fn [_ out] (cond (nil? out) :absent (zero? out) :none :else :counted))
   :classify-domain #{:absent :none :counted}
   :golden-path "test/golden/hive-mcp/plan/plans-count-plan-steps.edn"})

(hst/deftrifecta-from-schema high-abstraction-type-contract
  hive-mcp.plan.plans/high-abstraction-type?
  ;; "plan" is drawn in about a third of cases; the rest are near-misses
  ;; (case, other types), keywords and nil.
  {:in [:cat [:or {:gen/schema [:tuple [:enum 0 1 2] [:enum "note" "decision" "Plan" :plan nil]]
                   :gen/fmap (fn [[arm other]] (if (zero? arm) "plan" other))}
              :string :keyword :nil]]
   :out (out-schema 'high-abstraction-type?)
   :rel (fn [[t] out] (= out (= "plan" t)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :golden-path "test/golden/hive-mcp/plan/plans-high-abstraction-type.edn"})

(golden/deftest-golden plans-collection-lifecycle-golden
  "test/golden/hive-mcp/plan/plans-collection-lifecycle.edn"
  ;; Every step is a let binding: a map literal of more than eight entries
  ;; evaluates in hash order, which would delete before it queried.
  (let [id1 (plans/index-plan! {:id "p1" :content "1. a\n2. b" :tags ["x" "y"] :project-id "proj"})
        id2 (plans/index-plan! {:id "p2" :content "## Step 1\n## Step 2\n## Step 3"
                                :tags ["y"] :project-id "other" :decision-id "d-1"
                                :plan-status "active" :wave-count 2})
        got (plans/get-plan "p2")
        get-missing (plans/get-plan "nope")
        query-proj (mapv :id (plans/query-plans :project-id "proj"))
        query-tags (mapv :id (plans/query-plans :tags ["y"]))
        query-status (mapv :id (plans/query-plans :plan-status "active"))
        search (mapv #(dissoc % :distance) (plans/search-plans "a" :limit 1 :project-id "proj"))
        updated (plans/update-plan-status! "p1" "completed")
        after-update (:plan-status (plans/get-plan "p1"))
        update-invalid (try (plans/update-plan-status! "p1" "bogus")
                            (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))
        status (plans/status)
        deleted (plans/delete-plan! "p1")
        after-delete (mapv :id (plans/query-plans))]
    {:ids [id1 id2] :get got :get-missing get-missing
     :query-proj query-proj :query-tags (vec (sort query-tags)) :query-status query-status
     :search search :update updated :after-update after-update
     :update-invalid update-invalid :status status
     :delete deleted :after-delete after-delete}))
