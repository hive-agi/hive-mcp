(ns hive-mcp.tools.memory-kanban.visible-project-ids-trifecta-test
  "Golden + property + mutation pinning for visible-project-ids.

   `kanban list` from a leaf scope always merged every ancestor board
   (820 cards from hive-vessel, kanban 20260913181359-7756fa87). The pure
   core takes the scope chain and descendants as data; include-ancestors?
   false collapses the chain to self.

   Mutants are self-contained and never call the subject var."
  (:require [clojure.test :refer [is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.tools.memory-kanban.query :as q]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private chain ["leaf" "hive" "global"])

(def ^:private gen-input
  (gen/let [anc?  (gen/elements [true false nil])
            desc? gen/boolean
            kids  (gen/vector (gen/elements ["kid-a" "kid-b" "leaf"]) 0 3)]
    {:project-id "leaf" :chain chain :descendants kids
     :include-ancestors? anc? :include-descendants? desc?}))

(def ^:private input-of (atom nil))

(defn run-visible
  "Subject wrapper: remembers its input so :pred can check the output."
  [input]
  (reset! input-of input)
  (q/visible-project-ids input))

(defn- sound? [out]
  (let [{:keys [project-id descendants include-ancestors? include-descendants?]} @input-of
        expected (cond-> (if (false? include-ancestors?) #{project-id} (set chain))
                   include-descendants? (into descendants))]
    (and (vector? out)
         (= project-id (first out))
         (= (count out) (count (distinct out)))
         (= expected (set out)))))

(deftrifecta visible-project-ids-contract
  hive-mcp.tools.memory-kanban.visible-project-ids-trifecta-test/run-visible
  {:golden-path "test/golden/tools/memory_kanban/visible-project-ids.edn"
   :cases       {:default-up      {:project-id "leaf" :chain chain :descendants ["kid-a"]
                                   :include-ancestors? true :include-descendants? false}
                 :up-and-down     {:project-id "leaf" :chain chain :descendants ["kid-a"]
                                   :include-ancestors? true :include-descendants? true}
                 :self-only       {:project-id "leaf" :chain chain :descendants ["kid-a"]
                                   :include-ancestors? false :include-descendants? false}
                 :self-and-down   {:project-id "leaf" :chain chain :descendants ["kid-a"]
                                   :include-ancestors? false :include-descendants? true}
                 :nil-means-true  {:project-id "leaf" :chain chain :descendants []
                                   :include-ancestors? nil :include-descendants? false}}
   :gen         gen-input
   :pred        sound?
   :num-tests   200
   :mutations   [["ancestors-always — the original behaviour"
                  (fn [{:keys [chain descendants include-descendants?]}]
                    (vec (distinct (concat chain (when include-descendants? descendants)))))]
                 ["drops-descendants"
                  (fn [{:keys [project-id chain include-ancestors?]}]
                    (if (false? include-ancestors?) [project-id] (vec chain)))]]
   :assert      (fn []
                  (is (nil? (run-visible {:project-id "global" :chain ["global"]}))
                      "global has no visible-id filter")
                  (is (= ["leaf"] (run-visible {:project-id "leaf" :chain chain
                                                :include-ancestors? false}))
                      "include-ancestors? false keeps only self"))})
