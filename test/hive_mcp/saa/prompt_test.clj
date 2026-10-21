(ns hive-mcp.saa.prompt-test
  "Facets for SAA phase-prompt construction: build-phase-prompt over a
   generated phase model (with and without a fragment for the phase) and
   build-phase-prompt* against the default model, both pure and contracted."
  (:require [clojure.string :as str]
            [hive-mcp.saa.model :as model]
            [hive-mcp.saa.prompt :as prompt]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- out-schema [fsym]
  (-> (m/function-schemas) (get-in ['hive-mcp.saa.prompt fsym :schema]) m/children second))

(def ^:private body-marker
  {:silence "TASK: " :abstract "Based on these observations" :act "Execute the following plan:"})

(def ^:private PhaseModel
  [:vector {:max 3}
   [:map [:phase model/SaaPhase] [:goal-prompt-fragment {:optional true} [:maybe [:enum "FRAG-A" "FRAG-B"]]]]])

;; The arglist, generated correlated: an independent phase and model meet in
;; about one draw in ten, so on a coin flip the model is led by a descriptor
;; for the drawn phase carrying a fragment.
;; The phase and the model are drawn independently, and an independent model
;; holds a fragment for the drawn phase in about one draw in ten. So on a coin
;; flip the generated model is led by fragment-bearing descriptors for every
;; phase; otherwise it is the plain draw (often no fragment at all).
(def ^:private PromptArgs
  [:cat model/SaaPhase [:maybe [:enum "fix auth" "plan X"]] [:maybe [:enum "ctx"]]
   (into [:vector {:gen/schema [:tuple PhaseModel [:enum "FRAG-A" "FRAG-B"] :boolean]
                   :gen/fmap (fn [[pm frag lead?]]
                               (if lead?
                                 (into (mapv #(hash-map :phase % :goal-prompt-fragment frag)
                                             [:silence :abstract :act])
                                       pm)
                                 pm))}]
         (m/children PhaseModel))])

(defn- body-ok? [phase content extra out]
  (and (str/includes? out (body-marker phase))
       (case phase
         :silence (str/includes? out (str "TASK: " content))
         :abstract (str/includes? out (pr-str content))
         :act (str/includes? out (or content "Use best judgment.")))
       (= (some? extra)
          (or (str/includes? out "Prior knowledge context:") (str/includes? out "Original task: ")))))

(deftrifecta-from-schema build-phase-prompt-contract
  hive-mcp.saa.prompt/build-phase-prompt
  {:in PromptArgs
   :out (out-schema 'build-phase-prompt)
   :rel (fn [[phase content extra pm] out]
          (let [frag (:goal-prompt-fragment (some #(when (= phase (:phase %)) %) pm))]
            (and (if frag (str/starts-with? out (str frag "\n\n")) (str/starts-with? out (body-marker phase)))
                 (body-ok? phase content extra out))))
   :classify (fn [[phase _ _ pm] _]
               (if (:goal-prompt-fragment (some #(when (= phase (:phase %)) %) pm)) :fragment :bare))
   :classify-domain #{:fragment :bare}
   :golden-path "test/golden/hive-mcp/saa/prompt-build-phase-prompt.edn"})

(deftrifecta-from-schema build-phase-prompt-default-model-contract
  hive-mcp.saa.prompt/build-phase-prompt*
  {:in [:cat model/SaaPhase [:maybe [:enum "fix auth" "plan X"]] [:maybe [:enum "ctx"]]]
   :out (out-schema 'build-phase-prompt*)
   :rel (fn [[phase content extra] out]
          (and (str/starts-with? out (:goal-prompt-fragment (model/phase-descriptor phase)))
               (body-ok? phase content extra out)))
   :golden-path "test/golden/hive-mcp/saa/prompt-build-phase-prompt-default.edn"})
