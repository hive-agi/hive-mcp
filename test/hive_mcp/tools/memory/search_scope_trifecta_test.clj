(ns hive-mcp.tools.memory.search-scope-trifecta-test
  "Golden + property + mutation pinning for the two pure pieces that decide
   WHAT a semantic memory search may return:

   - `domain/scope->search-project-ids`: an explicit `scope` used to behave as
     a soft hint (ancestors and global leaked in), so scope=\"topic:x\"
     answered off-scope hits. It is now a strict project filter.
   - `search/resolve-exclude-tags`: the default `ingestion-chunk` exclusion no
     longer contradicts a search that asks for that tag by name.
   (kanban 20260519145016-4561f6bc)

   Mutants are self-contained and never call the subject var."
  (:require [clojure.test :refer [is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.memory.domain :as domain]
            [hive-mcp.tools.memory.search :as search]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; scope->search-project-ids
;; =============================================================================

(def ^:private hierarchy
  "pid -> [pid ancestors... global], the shape visible-scopes returns."
  {"topic:data" ["topic:data" "topic" "global"]
   "proj"       ["proj" "global"]})

(def ^:private children
  {"topic:data" ["topic:data:ch1"]
   "proj"       []})

(defn- visible [pid] (get hierarchy pid ["global"]))
(defn- child-scopes [pid] (get children pid []))

(def ^:private scope-gen
  (gen/one-of
   [(gen/return {:scope :all})
    (gen/return {:scope :global})
    (gen/fmap (fn [p] {:scope :project :pid p}) (gen/elements ["topic:data" "proj"]))
    (gen/fmap (fn [p] {:scope :auto :pid p}) (gen/elements ["topic:data" "proj" nil "global"]))]))

(def ^:private gen-scope-input
  (gen/let [s     scope-gen
            desc? gen/boolean]
    (assoc s :desc? desc?)))

(defn- ->sf [{:keys [scope pid]}]
  (case scope
    :all     (domain/parse-scope "all" nil)
    :global  (domain/parse-scope "global" nil)
    :project (domain/parse-scope pid nil)
    :auto    (domain/parse-scope nil pid)))

(def ^:private scope-input-of (atom nil))

(defn run-scope
  "Subject wrapper: remembers its input so :pred can check the output."
  [input]
  (reset! scope-input-of input)
  (domain/scope->search-project-ids
   (->sf input)
   {:include-descendants? (:desc? input)
    :strict?              true
    :visible-scopes       visible
    :descendant-scopes    child-scopes}))

(defn- explicit-scope-is-strict? [out]
  (let [{:keys [scope pid desc?]} @scope-input-of]
    (case scope
      :all     (nil? out)
      :global  (= ["global"] out)
      :project (= (vec (distinct (cons pid (when desc? (child-scopes pid))))) out)
      :auto    (if (and pid (not= pid "global"))
                 (= (vec (distinct (concat (visible pid) (when desc? (child-scopes pid))))) out)
                 (nil? out)))))

(deftrifecta scope-search-project-ids-contract
  hive-mcp.tools.memory.search-scope-trifecta-test/run-scope
  {:golden-path "test/golden/tools/memory/scope-search-project-ids.edn"
   :cases       {:all               {:scope :all :desc? true}
                 :global            {:scope :global :desc? true}
                 :explicit-strict   {:scope :project :pid "topic:data" :desc? false}
                 :explicit-with-kids {:scope :project :pid "topic:data" :desc? true}
                 :auto-keeps-ladder {:scope :auto :pid "topic:data" :desc? false}
                 :auto-no-project   {:scope :auto :pid nil :desc? true}}
   :gen         gen-scope-input
   :pred        explicit-scope-is-strict?
   :num-tests   200
   :mutations   [["soft-hint — explicit scope leaks ancestors and global (the original bug)"
                  (fn [{:keys [scope pid desc?]}]
                    (case scope
                      :all nil
                      :global ["global"]
                      :project (vec (distinct (concat (get hierarchy pid ["global"])
                                                      (when desc? (get children pid [])))))
                      :auto (when (and pid (not= pid "global"))
                              (vec (distinct (concat (get hierarchy pid ["global"])
                                                     (when desc? (get children pid []))))))))]
                 ["drops-descendants — child projects never admitted"
                  (fn [{:keys [scope pid]}]
                    (case scope
                      :all nil
                      :global ["global"]
                      :project [pid]
                      :auto (when (and pid (not= pid "global"))
                              (vec (get hierarchy pid ["global"])))))]
                 ["all-is-a-filter — scope=all still narrows"
                  (fn [{:keys [pid]}] [(or pid "global")])]]
   :assert      (fn []
                  (is (= ["topic:data"]
                         (run-scope {:scope :project :pid "topic:data" :desc? false}))
                      "an explicit scope admits exactly that project")
                  (is (= ["topic:data" "topic:data:ch1"]
                         (run-scope {:scope :project :pid "topic:data" :desc? true}))
                      "descendants are admitted, ancestors and global are not")
                  (is (= ["topic:data" "topic" "global"]
                         (run-scope {:scope :auto :pid "topic:data" :desc? false}))
                      "an auto-derived scope keeps the hierarchy")
                  (is (nil? (run-scope {:scope :all :desc? true}))
                      "scope=all lifts the project filter"))})

;; =============================================================================
;; resolve-exclude-tags
;; =============================================================================

(def ^:private defaults ["carto" "ingestion-chunk"])

(def ^:private tag-pool ["carto" "ingestion-chunk" "kanban" "kind:axiom"])

(def ^:private gen-exclude-input
  (gen/let [excl (gen/one-of [(gen/return nil)
                              (gen/return [])
                              (gen/vector-distinct (gen/elements tag-pool) {:max-elements 3})])
            req  (gen/vector-distinct (gen/elements tag-pool) {:max-elements 3})]
    {:exclude-tags excl :default-excludes defaults :required-tags req}))

(def ^:private exclude-input-of (atom nil))

(defn run-excludes
  "Subject wrapper: remembers its input so :pred can check the output."
  [input]
  (reset! exclude-input-of input)
  (search/resolve-exclude-tags input))

(defn- excludes-sound? [out]
  (let [{:keys [exclude-tags required-tags]} @exclude-input-of]
    (and (vector? out)
         (if (some? exclude-tags)
           (= (vec exclude-tags) out)
           (and (= out (vec (remove (set required-tags) defaults)))
                (empty? (filter (set required-tags) out)))))))

(deftrifecta resolve-exclude-tags-contract
  hive-mcp.tools.memory.search-scope-trifecta-test/run-excludes
  {:golden-path "test/golden/tools/memory/resolve-exclude-tags.edn"
   :cases       {:default-applies   {:exclude-tags nil :default-excludes defaults :required-tags []}
                 :caller-wins       {:exclude-tags ["kanban"] :default-excludes defaults :required-tags []}
                 :caller-empty      {:exclude-tags [] :default-excludes defaults :required-tags []}
                 :required-lifts    {:exclude-tags nil :default-excludes defaults
                                     :required-tags ["ingestion-chunk" "kind:axiom"]}
                 :caller-keeps-own  {:exclude-tags ["ingestion-chunk"] :default-excludes defaults
                                     :required-tags ["ingestion-chunk"]}}
   :gen         gen-exclude-input
   :pred        excludes-sound?
   :num-tests   200
   :mutations   [["ignores-required — default excludes a tag the caller asked for"
                  (fn [{:keys [exclude-tags default-excludes]}]
                    (if (some? exclude-tags) (vec exclude-tags) (vec default-excludes)))]
                 ["drops-caller-set — default always wins"
                  (fn [{:keys [default-excludes required-tags]}]
                    (vec (remove (set required-tags) default-excludes)))]
                 ["lifts-everything — any required tag clears the whole default"
                  (fn [{:keys [exclude-tags default-excludes required-tags]}]
                    (cond (some? exclude-tags) (vec exclude-tags)
                          (seq required-tags) []
                          :else (vec default-excludes)))]]
   :assert      (fn []
                  (is (= ["carto" "ingestion-chunk"]
                         (run-excludes {:exclude-tags nil :default-excludes defaults
                                        :required-tags ["kind:axiom"]}))
                      "an unrelated required tag leaves the default alone")
                  (is (= ["carto"]
                         (run-excludes {:exclude-tags nil :default-excludes defaults
                                        :required-tags ["ingestion-chunk"]}))
                      "asking for ingestion-chunk by name lifts only that exclusion")
                  (is (= []
                         (run-excludes {:exclude-tags [] :default-excludes defaults
                                        :required-tags []}))
                      "exclude_tags=[] stays empty"))})
