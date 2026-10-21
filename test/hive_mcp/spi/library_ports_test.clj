(ns hive-mcp.spi.library-ports-test
  "The kanban ports and the catchup block registry live in hive-spi
   (hive-spi.kanban, hive-spi.kanban.registry, hive-spi.catchup.registry).
   Core reaches them there. The host-local namespaces that carried them before
   hive-spi released them stay one release as deprecated shims for addons that
   still name them, and a shim must forward to the library's state, never keep
   a second registry beside it."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.spi.catchup-registry :as host-blocks]
            [hive-mcp.spi.kanban :as host-kanban]
            [hive-mcp.spi.kanban.registry :as host-kreg]
            [hive-spi.catchup.registry :as blocks]
            [hive-spi.kanban :as kanban]
            [hive-spi.kanban.registry :as kreg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private host-local-ports
  #{"hive-mcp.spi.kanban" "hive-mcp.spi.kanban.registry" "hive-mcp.spi.catchup-registry"})

(def ^:private shim-files
  "The deprecated shims themselves, the only sources allowed to name them."
  #{"src/hive_mcp/spi/kanban.clj"
    "src/hive_mcp/spi/kanban/registry.clj"
    "src/hive_mcp/spi/catchup_registry.clj"})

(defn- ns-form [^java.io.File file]
  (try
    (let [form (read-string {:read-cond :allow} (slurp file))]
      (when (and (seq? form) (= 'ns (first form))) form))
    (catch Exception _unreadable-file-has-no-ns-form nil)))

(defn- required-namespaces [form]
  (->> form
       (filter #(and (seq? %) (#{:require :use} (first %))))
       (mapcat rest)
       (map #(cond (symbol? %) % (sequential? %) (first %)))
       (filter symbol?)
       (map str)))

(defn- host-port-requirers []
  (for [^java.io.File f (file-seq (io/file "src"))
        :let [path (str/replace (.getPath f) "\\" "/")]
        :when (and (.isFile f) (str/ends-with? path ".clj") (not (shim-files path)))
        :let [form (ns-form f)]
        :when form
        required (required-namespaces form)
        :when (host-local-ports required)]
    [path required]))

(deftest core-requires-the-library-ports
  (is (empty? (host-port-requirers))
      "src requires hive-spi.kanban(.registry) / hive-spi.catchup.registry, never the host-local copies"))

(defrecord ShimBoard []
  host-kanban/IKanbanRead
  (list-tasks [_ _] [{:id "t1" :status "todo"}])
  (get-task [_ id] (when (= "t1" id) {:id "t1" :status "todo"})))

(deftest host-shims-forward-to-the-library-state
  (testing "a block registered through the shim is the library's block"
    (let [block {:block/id ::shim-block :block/fn (constantly {:ok 1}) :block/order 999}]
      (try
        (host-blocks/register-block! block)
        (is (blocks/registered? ::shim-block))
        (is (= {:ok 1} (get-in (blocks/compose {}) [:blocks ::shim-block])))
        (blocks/unregister-block! ::shim-block)
        (is (not (host-blocks/registered? ::shim-block)))
        (finally (blocks/unregister-block! ::shim-block)))))
  (testing "the shim's protocol is the library's protocol"
    (is (satisfies? kanban/IKanbanRead (->ShimBoard))))
  (testing "a provider registered through the shim registry answers the library facade"
    (let [prior (kreg/provider :IKanbanRead)
          was?  (kreg/registered? :IKanbanRead)]
      (try
        (host-kreg/register! :IKanbanRead (->ShimBoard))
        (is (kreg/registered? :IKanbanRead))
        (is (= {:id "t1" :status "todo"} (kreg/get-task "t1")))
        (finally
          (if was? (kreg/register! :IKanbanRead prior) (kreg/unregister! :IKanbanRead)))))))
