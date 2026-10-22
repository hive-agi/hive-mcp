(ns hive-mcp.tools.advertised-surface-claims-test
  "An addon that CLAIMS a core tool name holds it on every advertised surface:
   the server spec (`routes/build-server-spec`), the external-loader surface
   (`registry/get-advertised-tools`) and the surface the multi dispatch
   coherence check reads (`multi.registry/advertised-surface`) are all derived
   from `registry/advertised-tool-defs`, so they agree on the claimed name."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.protocol :as proto]
            [hive-mcp.addons.core :as addons]
            [hive-mcp.multi.registry :as multi-reg]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.test.stub.isolated-registry :refer [with-fresh-registries]]
            [hive-mcp.tools.registry :as reg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private claimed "web")

(def ^:private addon-description "web, as the claiming addon serves it")

(defn- addon-handler [_] {:type "text" :text "from the addon"})

(def ^:private addon-tool
  {:name         claimed
   :description  addon-description
   :inputSchema  {:type "object"
                  :properties {"command" {:type "string" :description "op"}}
                  :required ["command"]}
   :handler      addon-handler})

(defn- claiming-addon
  "A reified IAddon that provides `claimed` AND lists it in excluded-tools:
   a CLAIM over the core tool of that name (tool-claims rule 1)."
  []
  (reify proto/IAddon
    (addon-id [_] "test.claimer")
    (addon-type [_] :native)
    (capabilities [_] #{:tools})
    (initialize! [_ _] {:success? true :errors [] :metadata {}})
    (shutdown! [_] {:success? true :errors []})
    (tools [_] [addon-tool])
    (excluded-tools [_] #{claimed})
    (schema-extensions [_] {})
    (health [_] {:status :ok})))

(defn- with-claiming-addon [f]
  (addons/reset-registry!)
  (try
    (addons/register-addon! (claiming-addon))
    (addons/init-addon! "test.claimer")
    (f)
    (finally (addons/reset-registry!))))

(use-fixtures :each with-fresh-registries with-claiming-addon)

(defn- by-name [tools] (first (filter #(= claimed (:name %)) tools)))

(deftest the-claim-is-installed
  (is (contains? (addons/claimed-core-names (addons/resolve-addon-tools)) claimed))
  (is (some #(= claimed (:name %)) (reg/core-tools))
      "the claimed name is also a core tool, so the two layers really collide"))

(deftest every-surface-advertises-the-claimant
  (let [defs     (reg/advertised-tool-defs)
        advert   (reg/get-advertised-tools)
        spec     (:tools (routes/build-server-spec))]
    (testing "the one surface holds the addon's def"
      (is (= addon-description (:description (by-name defs))))
      (is (= 1 (count (filter #(= claimed (:name %)) defs))) "named once"))
    (testing "get-advertised-tools and build-server-spec agree on it"
      (is (= addon-description (:description (by-name advert))))
      (is (= addon-description (:description (by-name spec))))
      (is (= (set (map :name advert)) (set (map :name spec)) (set (map :name defs)))))
    (testing "routes reads the very same surface"
      (is (= (map :name defs) (map :name (routes/advertised-tool-defs)))))))

(deftest divergences-report-on-the-served-surface
  (let [core-handler (:handler (by-name (reg/core-tools)))
        lookup       (fn [n] (when (= n claimed) {:owner :multi/core :handler core-handler}))
        surface      (multi-reg/advertised-surface)]
    (testing "the coherence check reads the claim-resolved surface"
      (is (= addon-handler (:handler (by-name surface))))
      (is (= (map :name (reg/advertised-tool-defs)) (map :name surface))))
    (testing "the addon override of the claimed name is reported against the :multi/core seed"
      (is (= [{:tool claimed :advertised-handler addon-handler :dispatch-owner :multi/core}]
             (multi-reg/dispatch-divergences surface lookup))))
    (testing "the pre-fix surface (consolidated core wins) could not see it"
      (is (empty? (multi-reg/dispatch-divergences
                   (filter :consolidated (reg/core-tools)) lookup))))))
