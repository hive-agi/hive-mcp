(ns hive-mcp.tools.registry-surface-test
  "The advertised surface is ONE pure function of a SurfaceInputs value:
   precedence addon > dynamic > base, absorbed and excluded names dropped once,
   every name exactly once, the visibility gate applied last."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]
            [hive-mcp.tools.registry :as registry]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- t
  "A tool def tagged with the layer it came from, so a snapshot shows which
   layer won."
  [layer tool-name]
  {:name tool-name :src layer})

(defn- inputs
  [& {:keys [base dynamic addon absorbed excluded visible]}]
  {:base     (mapv (partial t :base) base)
   :dynamic  (mapv (partial t :dynamic) dynamic)
   :addon    (mapv (partial t :addon) addon)
   :absorbed (set absorbed)
   :excluded (set excluded)
   :visible  visible})

(def ^:private golden-cases
  {:precedence-addon-over-dynamic-over-base
   (inputs :base ["code" "memory" "git"]
           :dynamic ["memory" "kanban"]
           :addon ["memory" "git"])

   :addon-tool-appears-three-times-once-advertised
   (inputs :base ["code"]
           :dynamic ["carto" "carto"]
           :addon ["carto"])

   :absorbed-composite-is-dropped-from-every-layer
   (inputs :base ["code" "analysis"]
           :dynamic ["analysis" "overarch"]
           :addon ["analysis"]
           :absorbed ["analysis"])

   :child-excluded-names-dropped-whichever-layer-supplies-them
   (inputs :base ["code" "swarm" "multi"]
           :dynamic ["emacs"]
           :addon ["swarm"]
           :excluded ["swarm" "multi" "emacs"])

   :visibility-gate-marks-not-drops
   (inputs :base ["code" "fs"]
           :dynamic ["zeta" "alpha"]
           :visible #{"code" "zeta"})

   :empty-surface
   (inputs)})

(defn- snapshot
  "Name, winning layer and gate flag of every advertised tool, in order."
  [tools]
  (mapv (juxt :name :src :deprecated) tools))

;; -----------------------------------------------------------------------------
;; Generators and the invariant
;; -----------------------------------------------------------------------------

(def ^:private gen-name
  (gen/elements ["code" "memory" "git" "analysis" "carto" "swarm" "kanban" "hot"]))

(def ^:private gen-inputs
  (gen/let [base     (gen/vector gen-name 0 6)
            dynamic  (gen/vector gen-name 0 6)
            addon    (gen/vector gen-name 0 6)
            absorbed (gen/set gen-name {:max-elements 2})
            excluded (gen/set gen-name {:max-elements 2})
            visible  (gen/one-of [(gen/return nil) (gen/set gen-name {:max-elements 4})])]
    (inputs :base base :dynamic dynamic :addon addon
            :absorbed absorbed :excluded excluded :visible visible)))

(def ^:private layer-rank
  (zipmap registry/surface-precedence (range)))

(defn- surface-invariant?
  "No name twice; every absorbed/excluded name absent; every other offered name
   present, from the highest-precedence layer that offered it."
  [in out]
  (let [names   (map :name out)
        dropped (into (:absorbed in) (:excluded in))
        best    (reduce (fn [acc {:keys [name src]}]
                          (if (<= (get layer-rank (get acc name) -1) (layer-rank src))
                            (assoc acc name src)
                            acc))
                        {}
                        (mapcat in registry/surface-precedence))]
    (and (= (count names) (count (distinct names)))
         (not-any? dropped names)
         (= (set names) (set (remove dropped (keys best))))
         (every? #(= (:src %) (best (:name %))) out))))

;; -----------------------------------------------------------------------------
;; Mutants: the bugs this function exists to rule out
;; -----------------------------------------------------------------------------

(defn- mutant-no-dedup
  "The old refresh: base ++ dynamic ++ addon, gated, never deduped."
  [{:keys [base dynamic addon visible]}]
  (registry/apply-visibility-gate (concat base dynamic addon) visible))

(defn- mutant-base-wins
  "Wrong precedence: the FIRST occurrence of a name wins."
  [{:keys [absorbed excluded visible] :as in}]
  (let [dropped (into (set absorbed) excluded)]
    (registry/apply-visibility-gate
     (->> (mapcat in registry/surface-precedence)
          (reduce (fn [[seen acc] tool]
                    (if (seen (:name tool)) [seen acc] [(conj seen (:name tool)) (conj acc tool)]))
                  [#{} []])
          second
          (remove (comp dropped :name)))
     visible)))

(defn- mutant-absorption-ignored
  "The reload path that re-admitted absorbed composites."
  [in]
  (registry/advertised-tools (assoc in :absorbed #{})))

(def ^:private fixed-absorbed #{"analysis"})
(def ^:private fixed-excluded #{"swarm"})

(def ^:private gen-inputs-fixed-drops
  "Random layers against a FIXED absorbed/excluded pair, so the invariant can
   be read off the output alone."
  (gen/fmap #(assoc % :absorbed fixed-absorbed :excluded fixed-excluded) gen-inputs))

(defn- output-invariant?
  "Every name once; the absorbed composite and the excluded root never appear."
  [out]
  (let [names (map :name out)]
    (and (= (count names) (count (distinct names)))
         (not-any? (into fixed-absorbed fixed-excluded) names))))

(deftrifecta advertised-tools
  registry/advertised-tools
  {:golden-path "test/golden/tools/advertised_surface.edn"
   :cases       golden-cases
   :xf          snapshot
   :gen         gen-inputs-fixed-drops
   :pred        output-invariant?
   :num-tests   300
   :mutations   [["drop-dedup" mutant-no-dedup]
                 ["wrong-precedence" mutant-base-wins]
                 ["absorption-ignored" mutant-absorption-ignored]]})

(defspec highest-layer-wins-and-dropped-names-vanish 300
  (prop/for-all [in gen-inputs]
    (surface-invariant? in (registry/advertised-tools in))))

(deftest golden-cases-are-schema-valid-surface-inputs
  (testing "every golden input is a SurfaceInputs value object"
    (doseq [[label in] golden-cases]
      (is (m/validate registry/SurfaceInputs in) (str label)))))

(deftest analysis-absorption-is-the-same-on-every-path
  (testing "absorbed at boot == absorbed after a reload re-registered it dynamically"
    (let [boot   (inputs :base ["code"] :absorbed ["analysis"])
          reload (inputs :base ["code"] :dynamic ["analysis"] :addon ["analysis"]
                         :absorbed ["analysis"])]
      (is (= (snapshot (registry/advertised-tools boot))
             (snapshot (registry/advertised-tools reload)))))))
