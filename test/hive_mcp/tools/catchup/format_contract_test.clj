(ns hive-mcp.tools.catchup.format-contract-test
  "Schema-derived trifectas for the catchup.format functions that
   format_trifecta_test.clj does not already cover: the meta projections,
   the spawn markdown formatters and the error blocks."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.catchup.format :as fmt]
            [hive-mcp.tools.catchup.shapes :as shapes]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Content
  "Content of every length class around the 240/600 caps."
  [:string {:gen/schema [:tuple [:enum "a" "word " "é"] [:enum 0 1 239 240 241 599 600 601]]
            :gen/fmap   (fn [[s n]] (apply str (take n (cycle s))))}])

(def ^:private EntryIn
  [:map
   [:id [:enum "e1" "e2"]]
   [:type {:optional true} [:enum :axiom "axiom" "decision" :note]]
   [:tags {:optional true} [:maybe [:vector {:gen/max 3}
                                    [:enum "scope:project:p" "requested-type:axiom" "x" "catchup-priority"]]]]
   [:content {:optional true} Content]
   [:created {:optional true} [:enum "2026-01-01T00:00:00Z"]]])

(deftrifecta-from-schema entry->review-meta-queue
  hive-mcp.tools.catchup.format/entry->review-meta
  {:in          [:cat EntryIn]
   :out         fmt/ReviewMeta
   :rel         (fn [[e] out]
                  (let [c (str (:content e))]
                    (and (not-any? #(str/starts-with? (str %) "scope:") (:tags out))
                         (= (:preview out)
                            (cond-> (subs c 0 (min (count c) fmt/review-preview-cap))
                              (> (count c) fmt/review-preview-cap) (str " …")))
                         (str/includes? (:approve out) (str (:id e))))))
   :golden-path "test/golden/catchup/format-review-meta.edn"
   :num-tests   200})

(deftrifecta-from-schema entry->wrap-preview-bounded
  hive-mcp.tools.catchup.format/entry->wrap-preview
  {:in          [:cat EntryIn]
   :out         fmt/WrapPreview
   :rel         (fn [[e] out]
                  (and (<= (count (:preview out)) fmt/wrap-preview-cap)
                       (str/starts-with? (str (:content e)) (:preview out))
                       (= (:id e) (:id out))))
   :golden-path "test/golden/catchup/format-wrap-preview.edn"
   :num-tests   200})

(deftrifecta-from-schema entry->axiom-meta-inviolable
  hive-mcp.tools.catchup.format/entry->axiom-meta
  {:in  [:cat EntryIn]
   :out fmt/AxiomMeta
   :rel (fn [[e] out] (and (= (:content e) (:content out)) (= (vec (or (:tags e) [])) (:tags out))))
   :num-tests 100})

(deftrifecta-from-schema entry->priority-meta-convention
  hive-mcp.tools.catchup.format/entry->priority-meta
  {:in  [:cat EntryIn]
   :out fmt/PriorityMeta
   :rel (fn [[e] out] (and (= (:id e) (:id out)) (= (:content e) (:content out))))
   :num-tests 100})

(deftrifecta-from-schema cap-piggyback-entry-axioms-pass
  hive-mcp.tools.catchup.format/cap-piggyback-entry
  {:in              [:cat EntryIn]
   :out             EntryIn
   :rel             (fn [[e] out]
                      (if (= "axiom" (name (or (:type e) "")))
                        (= e out)
                        (= out (fmt/cap-axiom-content e))))
   :classify        (fn [[e] _] (if (= "axiom" (name (or (:type e) ""))) :axiom :capped))
   :classify-domain #{:axiom :capped}
   :num-tests       200})

(def ^:private Lines [:vector {:gen/max 3} [:map [:content [:enum "first" "second rule"]]]])

(deftrifecta-from-schema format-spawn-axioms-numbered
  hive-mcp.tools.catchup.format/format-spawn-axioms
  {:in          [:cat Lines]
   :out         [:maybe :string]
   :rel         (fn [[xs] out]
                  (if (seq xs)
                    (and (str/starts-with? out "### Axioms")
                         (every? (fn [[i x]] (str/includes? out (str (inc i) ". " (:content x))))
                                 (map-indexed vector xs)))
                    (nil? out)))
   :golden-path "test/golden/catchup/format-spawn-axioms.edn"
   :num-tests   100})

(deftrifecta-from-schema format-spawn-priorities-numbered
  hive-mcp.tools.catchup.format/format-spawn-priorities
  {:in  [:cat Lines]
   :out [:maybe :string]
   :rel (fn [[xs] out] (if (seq xs) (str/starts-with? out "### Priority Conventions") (nil? out)))
   :num-tests 100})

(deftrifecta-from-schema format-spawn-decisions-bullets
  hive-mcp.tools.catchup.format/format-spawn-decisions
  {:in  [:cat [:vector {:gen/max 3} [:map [:P {:optional true} [:enum "p1" "p2"]]
                                     [:preview {:optional true} [:enum "pv"]]]]]
   :out [:maybe :string]
   :rel (fn [[xs] out]
          (if (seq xs)
            (= (count xs) (count (filter #(str/starts-with? % "- ") (str/split-lines out))))
            (nil? out)))
   :num-tests 100})

(deftrifecta-from-schema format-spawn-git-section
  hive-mcp.tools.catchup.format/format-spawn-git
  {:in          [:cat [:maybe [:map [:branch {:optional true} [:enum "main"]]
                               [:uncommitted {:optional true} :boolean]
                               [:last-commit {:optional true} [:enum "abc - x"]]]]]
   :out         [:maybe :string]
   :rel         (fn [[g] out]
                  (if g
                    (and (str/includes? out (or (:branch g) "unknown"))
                         (= (boolean (:uncommitted g)) (str/includes? out "Uncommitted")))
                    (nil? out)))
   :golden-path "test/golden/catchup/format-spawn-git.edn"
   :num-tests   100})

(deftrifecta-from-schema format-spawn-stale-files-rows
  hive-mcp.tools.catchup.format/format-spawn-stale-files
  {:in  [:cat [:vector {:gen/max 3} [:map [:path [:enum "a.clj" "b.clj"]]
                                     [:score [:double {:min 0.0 :max 1.0}]]
                                     [:days-since-read {:optional true} [:maybe [:int {:min 0 :max 90}]]]
                                     [:hash-mismatch? {:optional true} :boolean]]]]
   :out [:maybe :string]
   :rel (fn [[xs] out]
          (if (seq xs)
            (every? #(str/includes? out (str "`" (:path %) "`")) xs)
            (nil? out)))
   :num-tests 100})

(deftrifecta-from-schema build-scopes-membership
  hive-mcp.tools.catchup.format/build-scopes
  {:in          [:cat [:maybe [:enum "hive-mcp" "x"]] [:maybe [:enum "global" "hive-mcp" "nonexistent-m26"]]]
   :out         [:vector :string]
   :rel         (fn [[pname pid] out]
                  (and (= (not (and pid (not= pid "global"))) (contains? (set out) "scope:global"))
                       (or (nil? pname) (contains? (set out) (str "scope:project:" pname)))))
   :golden-path "test/golden/catchup/format-build-scopes.edn"
   :num-tests   100})

(deftest serialize-spawn-context-sections
  (let [s (fmt/serialize-spawn-context {:axioms [{:content "a1"}] :project-name "p"
                                        :git-info {:branch "main" :last-commit "x"}})]
    (is (str/includes? s "**Project**: p"))
    (is (str/includes? s "1. a1"))
    (is (str/includes? s "### Git Status")))
  (is (str/includes? (fmt/serialize-spawn-context {}) "**Project**: unknown")))

(deftest error-blocks
  (let [e1 (fmt/store-not-configured-error)
        e2 (fmt/catchup-error (ex-info "boom" {}))]
    (is (m/validate fmt/ErrorBlock e1))
    (is (m/validate fmt/ErrorBlock e2))
    (is (= "Memory store not configured" (:error (json/read-str (:text e1) :key-fn keyword))))
    (is (= "boom" (:error (json/read-str (:text e2) :key-fn keyword))))))

(deftest block-name-qualified
  (is (= "kanban" (fmt/block-name :kanban)))
  (is (= "x/y" (fmt/block-name :x/y)))
  (is (= "s" (fmt/block-name "s"))))

(deftest contributed-sections-and-response
  (let [secs (fmt/contributed-sections [[:a {:n 1}] [:b nil] [:c 7]])]
    (is (= 2 (count secs)))
    (is (= {:_block "c" :value 7} (json/read-str (:text (second secs)) :key-fn keyword))))
  (let [resp (fmt/build-catchup-response {:project-id "p" :scopes ["scope:project:p"]})]
    (is (m/validate [:vector fmt/TextBlock] resp))
    (is (= "header" (get (json/read-str (:text (first resp))) "_block")))))

(deftest entry-schema-is-shared
  (is (m/validate shapes/Entry {:id "e1" :type :axiom})))
