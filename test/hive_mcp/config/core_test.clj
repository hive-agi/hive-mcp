(ns hive-mcp.config.core-test
  "Accessor facets for hive-mcp.config.core, read through a bound config
   source so no test touches ~/.config/hive-mcp, plus the directory-wide
   contract coverage gate for src/hive_mcp/config."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.merge :as merge]
            [hive-mcp.config.source :as source]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private DottedKey
  "Dotted key strings whose segments are non-empty, so the parse round-trips."
  [:re {:gen/fmap (fn [segs] (str/join "." segs))
        :gen/schema [:vector {:min 1 :max 4} [:enum "memory" "routes" "a" "b1" "kg-backend"]]}
   #"^[a-z0-9-]+(\.[a-z0-9-]+)*$"])

(deftrifecta-from-schema dotted-key-parse hive-mcp.config.core/parse-key-path
  {:in [:cat DottedKey]
   :out [:vector {:min 1} :keyword]
   :rel (fn [[k] out] (= k (str/join "." (map name out))))
   :num-tests 300})

(def ^:private fixture-config
  (assoc merge/default-config
         :project-roots ["/home/u/PP"]
         :parent-rules [{:path-prefix "/home/u/PP/hive" :parent-id "hive"}]
         :project-overrides {"p" {:hot-reload true}}
         :services {:carto {:mode :remote :url "http://carto"}}
         :memory {:default-store :milvus
                  :kanban-store :dual-read
                  :routes {:decision :chroma
                           :snippet {:primary :milvus :projection :chroma}}
                  :stores {:milvus {:addon :hive-milvus :port 19530}}}))

(deftest accessors-read-the-bound-source
  (let [a (atom fixture-config)]
    (binding [config/*config-source* (source/atom-source a merge/default-config)]
      (golden/assert-golden
       (golden/anchor 'hive-mcp.config.core-test "test/golden/config/core-accessors.edn")
       {:project-roots   (config/get-project-roots)
        :defaults        (config/get-defaults)
        :overrides       (config/get-project-overrides "p")
        :project-config  (config/get-project-config "p")
        :unknown-project (config/get-project-config "zz")
        :parent          (config/get-parent-for-path "/home/u/PP/hive/hive-mcp")
        :no-parent       (config/get-parent-for-path "/tmp")
        :route-decision  (config/get-memory-route :decision)
        :route-snippet   (config/get-memory-route :snippet)
        :route-fallback  (config/get-memory-route :unknown)
        :store           (config/get-memory-store :milvus)
        :kanban-mode     (config/get-kanban-store-mode)
        :in-config       (config/get-in-config [:memory :default-store])
        :service         (config/get-service-config :carto)
        :service-mode    (config/get-service-mode :carto)
        :default-mode    (config/get-service-mode :absent)
        :service-url     (config/get-service-value :carto :url)
        :dotted          (config/get-config-value "memory.routes.decision")})
      (testing "writes land in the bound atom, never on disk"
        (config/set-config-value! "defaults.presets-path" "/p")
        (config/update-in-config! [:defaults :hot-reload] not)
        (is (= "/p" (get-in @a [:defaults :presets-path])))
        (is (true? (get-in @a [:defaults :hot-reload])))))))

(deftest kanban-mode-defaults-when-absent
  (binding [config/*config-source* (source/static-source {:memory {:default-store :x}})]
    (is (= :default (config/get-kanban-store-mode)))
    (is (= :x (config/get-memory-route :anything))))
  (testing "a read-only source refuses writes"
    (binding [config/*config-source* (source/static-source {})]
      (is (thrown? clojure.lang.ExceptionInfo
                   (config/update-in-config! [:a] (constantly 1)))))))

(deftest-contract-coverage config-contracts
  {:paths ["src/hive_mcp/config"]})
