(ns hive-mcp.tools.kanban.children-facet-test
  "Facets for hive-mcp.tools.kanban.children: open-children over generated
   stub KGs and stores (property against an independent oracle) and over a
   fixed board (golden). The reads are injected through the deps map."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.tools.kanban.children :as children]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:enum "card" "k1" "k2" "k3" "note" "ghost"])

(def ^:private Edge
  [:tuple Id [:enum :contains :derived-from :depends-on :implements :relates "depends-on"] Id])

(def ^:private Status [:enum "todo" "doing" "review" "done"])

(def ^:private World
  "Edges among a fixed id set, and the status of each kanban card k1..k3."
  [:tuple [:vector {:gen/max 10} Edge] [:map-of [:enum "k1" "k2" "k3"] Status]])

(defn- entry [id status]
  {:id id :tags ["kanban" status]
   :content {:task-type "kanban" :title (str "t-" id) :status status :priority "low"}})

(defn- deps-of [[edges statuses]]
  (let [es (mapv (fn [[f r t]] {:kg-edge/from f :kg-edge/relation r :kg-edge/to t}) edges)
        store (merge {"note" {:id "note" :content "plain"}}
                     (into {} (map (fn [[id s]] [id (entry id s)])) statuses))]
    {:edges-from (fn [id] (filter #(= id (:kg-edge/from %)) es))
     :edges-to   (fn [id] (filter #(= id (:kg-edge/to %)) es))
     :get-entry  store}))

(def ^:private child-links
  #{[:contains :out] [:derived-from :in] [:depends-on :in] [:implements :in]})

(defn- oracle
  "Independent reading of the docstring: the open kanban cards linked to
   \"card\" as children, each once by its first link, outgoing links before
   incoming ones."
  [[edges statuses]]
  (let [rel #(keyword (name %))
        links (concat (for [[f r t] edges :when (and (= f "card") (not= t "card")
                                                     (child-links [(rel r) :out]))]
                        [t (rel r) "outgoing"])
                      (for [[f r t] edges :when (and (= t "card") (not= f "card")
                                                     (child-links [(rel r) :in]))]
                        [f (rel r) "incoming"]))
        firsts (:acc (reduce (fn [{:keys [seen] :as st} [id :as l]]
                               (if (seen id) st (-> st (update :seen conj id) (update :acc conj l))))
                             {:seen #{} :acc []} links))]
    (vec (for [[id r d] firsts
               :let [s (get statuses id)]
               :when (and s (not= "done" s))]
           {:id id :title (str "t-" id) :status s :priority "low"
            :relation (name r) :direction d}))))

(def ^:private out-schema
  (-> (m/function-schemas) (get-in ['hive-mcp.tools.kanban.children 'open-children :schema])
      m/children second))

(defspec open-children-matches-oracle 200
  (prop/for-all [world (mg/generator World)]
    (let [out (children/open-children "card" (deps-of world))]
      (and (m/validate out-schema out)
           (= (oracle world) out)))))

(golden/deftest-golden open-children-golden
  "test/golden/hive-mcp/tools/kanban/children-open.edn"
  (let [world [[["k1" :depends-on "card"] ["card" :contains "k2"] ["k3" "depends-on" "card"]
                ["note" :derived-from "card"] ["ghost" :implements "card"]
                ["card" :depends-on "k1"] ["k1" :implements "card"] ["k2" :relates "card"]]
               {"k1" "doing" "k2" "todo" "k3" "done"}]]
    (children/open-children "card" (deps-of world))))
