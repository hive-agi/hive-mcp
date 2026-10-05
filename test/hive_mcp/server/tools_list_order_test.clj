(ns hive-mcp.server.tools-list-order-test
  "tools/list comes back sorted by name, so the listing is a function of the
   tool SET alone: two listings agree, and a register or refresh never
   reshuffles the tools that were already there. A hash-ordered listing busts
   every client's prompt cache whenever the table changes."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.trifecta :refer [deftrifecta]]
            [io.modelcontext.clojure-sdk.server :as sdk]
            [hive-mcp.server.registration :as reg]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.transport.mcp-http :as mcp-http]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private deprecated-only {:deprecated #'reg/deprecated-tool?})

(defn- entry [name & {:as extra}]
  {:tool (merge {:name name :description name} extra) :handler identity})

(defn- names [tools] (mapv :name tools))

(defn- sorted-names? [ns] (= ns (sort ns)))

;; =============================================================================
;; Trifecta over the pure projection
;; =============================================================================

(def ^:private gen-entries
  "Entries with distinct names, in arbitrary order, some deprecated."
  (gen/let [ns   (gen/set (gen/fmap #(str "t" %) (gen/choose 0 400)) {:max-elements 40})
            deps (gen/vector gen/boolean (count ns))
            perm (gen/shuffle (map vector ns deps))]
    (mapv (fn [[n d]] (if d (entry n :deprecated true) (entry n))) perm)))

(deftrifecta listed-tools-contract
  hive-mcp.server.registration/listed-tools
  {:golden-path "test/golden/server/tools-list-order.edn"
   :apply?      true
   :xf          names
   :cases       {:empty            [deprecated-only []]
                 :reverse-inserted [deprecated-only [(entry "zeta") (entry "mid") (entry "alpha")]]
                 :hidden-dropped   [deprecated-only [(entry "memory") (entry "agent" :deprecated true)
                                                     (entry "kg")]]
                 :no-rules         [{} [(entry "b" :deprecated true) (entry "a")]]
                 :case-and-dash    [{} [(entry "kg-b") (entry "Kg") (entry "kg") (entry "kg_a")]]
                 :desc-disagrees   [{} [(entry "b" :description "a") (entry "a" :description "b")]]}
   :gen         (gen/fmap (fn [es] [deprecated-only es]) gen-entries)
   :pred        (fn [tools]
                  (let [ns (names tools)]
                    (and (vector? tools)
                         (sorted-names? ns)
                         (= (count ns) (count (distinct ns)))
                         (not-any? :deprecated tools))))
   :num-tests   300
   :mutations   [["hash/insertion order — the bug"
                  (fn [rules entries] (reg/visible-tools rules entries))]
                 ["sorted descending"
                  (fn [rules entries]
                    (vec (reverse (sort-by :name (reg/visible-tools rules entries)))))]
                 ["sorted but the hide rules skipped"
                  (fn [_ entries] (vec (sort-by :name (map :tool entries))))]
                 ["sorted by description"
                  (fn [rules entries]
                    (vec (sort-by :description (reg/visible-tools rules entries))))]]})

(defspec listing-ignores-the-tables-iteration-order 300
  (prop/for-all [[es perm] (gen/let [es   gen-entries
                                     perm (gen/shuffle es)]
                             [es perm])]
    (let [as-table (fn [xs] (vals (into {} (map (juxt (comp :name :tool) identity)) xs)))]
      (= (reg/listed-tools deprecated-only es)
         (reg/listed-tools deprecated-only perm)
         (reg/listed-tools deprecated-only (as-table perm))))))

(defspec adding-a-tool-never-reorders-the-others 300
  (prop/for-all [es gen-entries
                 n  (gen/fmap #(str "new-" %) (gen/choose 0 1000))]
    (let [before (names (reg/listed-tools {} es))
          after  (names (reg/listed-tools {} (conj es (entry n))))]
      (and (sorted-names? after)
           (= before (vec (remove #{n} after)))))))

;; =============================================================================
;; Example: the installed tools/list method, end to end over MCP-HTTP
;; =============================================================================

(def ^:private tool-names
  ;; > 8 names, so the SDK table is a PersistentHashMap, not an array map
  ["swarm" "memory" "kg" "agent" "zeta" "carto" "bash" "magit" "olympus" "preset" "analysis" "hot"])

(defn- sdk-tool [n]
  {:name n :description n :inputSchema {:type "object"}
   :handler (fn [_] {:type "text" :text n})})

(defn- tools-list [context]
  (->> (mcp-http/answer (mcp-http/sdk-dispatch context)
                        {:jsonrpc "2.0" :id 1 :method "tools/list"})
       :body :result :tools names))

(deftest two-listings-agree-and-are-sorted
  (is (reg/tools-list-installed?))
  (let [ctx (sdk/create-context! {:name "order-test" :version "0"
                                  :tools (mapv sdk-tool tool-names)})
        a   (tools-list ctx)
        b   (tools-list ctx)]
    (is (= a b))
    (is (= (vec (sort tool-names)) a))))

(deftest a-registered-tool-lands-in-place
  (let [ctx    (sdk/create-context! {:name "order-test" :version "0"
                                     :tools (mapv sdk-tool tool-names)})
        before (tools-list ctx)]
    (testing "a tool written into the live table (what install-table! does)"
      (swap! (:tools ctx) assoc "dirge"
             {:tool (dissoc (sdk-tool "dirge") :handler) :handler identity})
      (let [after (tools-list ctx)]
        (is (sorted-names? after))
        (is (= before (vec (remove #{"dirge"} after))) "the others keep their order")))))

(deftest a-surface-refresh-keeps-the-listing-sorted-and-stable
  (testing "refresh-surfaces! replaces the whole table; the listing is still sorted and equal across refreshes"
    (let [ctx (sdk/create-context! {:name "order-test" :version "0"
                                    :tools (mapv sdk-tool tool-names)})
          id  ::refresh-surface]
      (routes/register-surface! id {:surface/kind :tools-atom
                                    :surface/tools-atom (:tools ctx)})
      (try
        (routes/refresh-surfaces!)
        (let [first-list (tools-list ctx)
              _          (routes/refresh-surfaces!)
              again      (tools-list ctx)]
          (is (seq first-list))
          (is (sorted-names? first-list))
          (is (= first-list again)))
        (finally (routes/unregister-surface! id))))))
