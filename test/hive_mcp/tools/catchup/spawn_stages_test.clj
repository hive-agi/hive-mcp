(ns hive-mcp.tools.catchup.spawn-stages-test
  "Characterization of `spawn-context` across its mode collectors, through
   ports only (stub memory store, disc port, context-store, extension
   registry), plus trifectas over the pure promote/pipeline stages."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.tools.catchup.spawn :as spawn]
            [hive-mcp.channel.context-store :as ctx-store]
            [hive-mcp.test.stub.extensions :as ext-stub]
            [hive-mcp.test.stub.memory-store :as stub]
            [hive-mcp.spi.disc :as disc-port]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.project.scope]
            [clojure.string]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord NoStaleDisc []
  disc-port/IDiscKnowledge
  (-staleness-value [_ _] nil)
  (-propagate-staleness! [_ _ _ _] nil)
  (-top-stale-files [_ _] []))

(use-fixtures :each
  stub/with-stub-store
  (fn [f]
    (disc-port/install! (->NoStaleDisc))
    (ctx-store/reset-all!)
    (try (f) (finally (disc-port/uninstall!) (ctx-store/reset-all!)))))

(def ^:private dir "/nonexistent-m26/x")

(defn- project-of-dir []
  (hive-mcp.project.scope/get-current-project-id dir))

(defn- seed-refs! []
  (let [p (or (project-of-dir) "global")]
    (doseq [cat ["axioms" "decisions"]]
      (ctx-store/context-put! [{:id (str cat "-1")}]
                              :tags #{"catchup" cat p} :ttl-ms 60000))))

(defn- shape
  "Strip the volatile ctx-ids from a rendered context so the golden is stable."
  [s]
  (some-> s (clojure.string/replace #"ctx-[0-9a-f-]+" "ctx-ID")))

(deftest-golden spawn-context-modes
  "test/golden/tools/catchup/spawn-context-modes.edn"
  (let [full (spawn/spawn-context dir)
        explicit-full (spawn/spawn-context dir {:mode :full})
        ref-cold (spawn/spawn-context dir {:mode :ref})
        _ (seed-refs!)
        ref-table (ext-stub/without-extensions
                   [:cr/i] #(spawn/spawn-context dir {:mode :ref}))
        ref-recon (ext-stub/with-extensions
                   {:cr/i (fn [refs seeds _] (str "## Reconstructed\n" (sort (keys refs)) seeds))}
                   #(spawn/spawn-context dir {:mode :ref}))]
    {:full (shape full)
     :explicit-full= (= full explicit-full)
     :ref-cold-falls-back-to-full (= full ref-cold)
     :ref-table (shape ref-table)
     :ref-recon (shape ref-recon)}))

(deftest no-store-is-nil-in-every-mode
  (stub/with-no-store
    (fn []
      (doseq [mode [:full :hints :ref]]
        (is (nil? (spawn/spawn-context dir {:mode mode})))))))

;; --- pure stages ------------------------------------------------------------

(defn- git-status-md* [g] (#'spawn/git-status-md g))
(defn- enrich-hints* [h t] (#'spawn/enrich-hints h t))

(deftrifecta git-status-md
  hive-mcp.tools.catchup.spawn-stages-test/git-status-md*
  {:golden-path "test/golden/tools/catchup/spawn-git-status-md.edn"
   :cases {:none nil
           :clean {:branch "main" :uncommitted false :last-commit "abc"}
           :dirty {:branch "dev" :uncommitted true}
           :blank {}}
   :gen (gen/one-of [(gen/return nil)
                     (gen/hash-map :branch gen/string-alphanumeric
                                   :uncommitted gen/boolean)])
   :pred (fn [s] (or (nil? s) (clojure.string/includes? s "### Git Status")))
   :num-tests 50
   :mutations [["always-nil" (constantly nil)]
               ["drops-branch" (fn [g] (when g "\n\n### Git Status\n"))]]})

(def ^:private gen-task-hints
  (gen/one-of
   [(gen/return nil)
    (gen/hash-map :l1-ids (gen/vector gen/string-alphanumeric 0 3)
                  :l2-queries (gen/vector gen/string-alphanumeric 0 3)
                  :l3-seeds (gen/vector (gen/hash-map :id gen/string-alphanumeric
                                                      :depth (gen/choose 1 3)) 0 3))]))

(deftrifecta enrich-hints
  hive-mcp.tools.catchup.spawn-stages-test/enrich-hints*
  {:golden-path "test/golden/tools/catchup/spawn-enrich-hints.edn"
   :apply? true
   :cases {:no-task [{:memory-hints {:read-ids ["a"]}} nil]
           :all-levels [{:memory-hints {:read-ids ["a"] :queries ["q"]}}
                        {:l1-ids ["b"] :l2-queries ["r"]
                         :l3-seeds [{:id "s1" :depth 3} {:id "s2"}]}]
           :empty-levels [{:memory-hints {}} {:l1-ids [] :l2-queries [] :l3-seeds []}]}
   :gen (gen/tuple (gen/return {:memory-hints {:read-ids ["base"]}}) gen-task-hints)
   :pred (fn [out] (= ["base"] (take 1 (get-in out [:memory-hints :read-ids]))))
   :num-tests 50
   :mutations [["ignores-task" (fn [h _] h)]
               ["replaces-read-ids" (fn [h t] (assoc-in h [:memory-hints :read-ids] (:l1-ids t)))]]})
