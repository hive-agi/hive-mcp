(ns hive-mcp.tools.memory.explicit-project-id-trifecta-test
  "Trifecta contract for `scope/explicit-project-id`, the rule that makes a
   caller-named `scope:project:<id>` tag win over the project inferred from the
   request directory or agent session on `memory add`
   (kanban 20260519173408-30f0a6a6, rule A).

   Facets:
   - golden:   verdicts for tag vectors with, without and with several scope tags
   - property: the answer is :none or a non-blank id
   - mutation: ignoring the tag, and letting scope:global override, are caught

   The do-add tests below drive the real do-add! through a recorder store to
   prove the canonical :project-id the store receives follows the tag."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.crystal.recall]
            [hive-mcp.knowledge-graph.connection]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.tools.memory.crud.write :as wr]
            [hive-mcp.tools.memory.scope :as scope]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn explicit-id
  "Subject: the explicit project-id named by TAGS, or :none."
  [tags]
  (or (scope/explicit-project-id tags) :none))

(def ^:private cases
  {:no-tags         []
   :unrelated-tags  ["kind:note" "agent:x"]
   :project-tag     ["a" "scope:project:funeraria" "b"]
   :first-wins      ["scope:project:one" "scope:project:two"]
   :global-ignored  ["scope:global"]
   :global-then-prj ["scope:global" "scope:project:funeraria"]
   :blank-id        ["scope:project:" "scope:project:   "]
   :hierarchical    ["scope:project:hive-mcp:agora"]
   :non-string      [nil 42 :scope:project:x]})

(def ^:private gen-tags
  (gen/vector
   (gen/one-of [(gen/fmap #(str "scope:project:p" %) gen/nat)
                (gen/return "scope:global")
                gen/string-alphanumeric])))

(defn- named-or-none?
  "Property predicate (receives OUTPUT): :none or a non-blank string."
  [out]
  (or (= :none out) (and (string? out) (not (str/blank? out)))))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(deftrifecta explicit-project-id-contract
  hive-mcp.tools.memory.explicit-project-id-trifecta-test/explicit-id
  {:golden-path   "test/golden/tools/memory/explicit-project-id.edn"
   :cases         cases
   :gen           gen-tags
   :pred          named-or-none?
   :property-type :pred
   :num-tests     100
   :mutations
   [["ignores the tag — the original bug"
     (fn [_tags] :none)]
    ["scope:global overrides the project"
     (fn [tags]
       (if (some #{"scope:global"} tags)
         "global"
         (or (scope/explicit-project-id tags) :none)))]]})

(defn- recorder-stub
  "IMemoryStore stub recording every added entry's :project-id."
  [seen]
  (reify proto/IMemoryStore
    (connect!       [_ _] {:success? true})
    (disconnect!    [_]   nil)
    (connected?     [_]   true)
    (health-check   [_]   {:healthy? true})
    (add-entry!     [_ e]
      (swap! seen conj (select-keys e [:project-id :tags]))
      (str "id-" (random-uuid)))
    (get-entry      [_ id] {:id id})
    (update-entry!  [_ id _] {:success? true :id id})
    (delete-entry!  [_ _] nil)
    (query-entries  [_ _] [])
    (search-similar [_ _ _] [])
    (supports-semantic-search? [_] false)
    (cleanup-expired! [_] {})
    (entries-expiring-soon [_ _ _] [])
    (find-duplicate [_ _ _ _] nil)
    (store-status   [_] {})
    (reset-store!   [_] nil)))

(use-fixtures :each
  (fn [f]
    (let [snapshot (proto/registered-stores)]
      (proto/unregister-store! :default)
      (try
        (f)
        (finally
          (proto/unregister-store! :default)
          (doseq [[k store] snapshot]
            (when (= :default k)
              (proto/register-store! k store))))))))

(defn- add-and-record
  "Run do-add! from DIRECTORY with TAGS; return the recorded store writes."
  [directory tags]
  (let [seen (atom [])]
    (proto/register-store! :default (recorder-stub seen))
    (with-redefs [hive-mcp.knowledge-graph.connection/with-tx-batch
                  (fn [body] (eval body))
                  hive-mcp.crystal.recall/register-created-id!
                  (constantly nil)]
      (#'wr/do-add! {:type "note" :content (str "c-" (random-uuid)) :tags tags
                     :directory directory}))
    @seen))

(deftest do-add-explicit-scope-tag-wins-over-directory
  (let [[w] (add-and-record "/tmp/hive-mcp" ["scope:project:funeraria"])]
    (is (= "funeraria" (:project-id w))
        "the scope:project tag, not the directory, names the project")
    (is (some #{"scope:project:funeraria"} (:tags w)))
    (is (not-any? #{"scope:project:hive-mcp"} (:tags w))
        "no second, conflicting scope tag is injected")))

(deftest do-add-without-scope-tag-uses-directory
  (let [[w] (add-and-record "/tmp/hive-mcp" ["kind:note"])]
    (is (= "hive-mcp" (:project-id w)))
    (is (some #{"scope:project:hive-mcp"} (:tags w)))))

(deftest do-add-scope-global-tag-does-not-override-directory
  (let [[w] (add-and-record "/tmp/hive-mcp" ["scope:global"])]
    (is (= "hive-mcp" (:project-id w))
        "scope:global is a visibility tag, never a project override")))
