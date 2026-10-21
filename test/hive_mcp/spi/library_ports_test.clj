(ns hive-mcp.spi.library-ports-test
  "The kanban ports and the catchup block registry live in hive-spi
   (hive-spi.kanban, hive-spi.kanban.registry, hive-spi.catchup.registry).
   Core reaches them there. The host-local namespaces that carried them
   before hive-spi released them were kept one release as forwarding shims,
   then retired once hive-git and hive-knowledge registered through hive-spi
   directly. Nothing in src may name them, and they must not come back."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private host-local-ports
  #{"hive-mcp.spi.kanban" "hive-mcp.spi.kanban.registry" "hive-mcp.spi.catchup-registry"})

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
        :when (and (.isFile f) (str/ends-with? path ".clj"))
        :let [form (ns-form f)]
        :when form
        required (required-namespaces form)
        :when (host-local-ports required)]
    [path required]))

(deftest core-requires-the-library-ports
  (is (empty? (host-port-requirers))
      "src requires hive-spi.kanban(.registry) / hive-spi.catchup.registry, never the host-local copies"))

(deftest the-retired-host-local-ports-are-gone
  ;; hive-git and hive-knowledge register through hive-spi directly, so the
  ;; one-release forwarding shims are deleted. A namespace left on the
  ;; classpath would invite a second registry beside the library's.
  (doseq [ns-str host-local-ports
          :let [base (-> ns-str (str/replace "." "/") (str/replace "-" "_"))]]
    (is (nil? (io/resource (str base ".clj"))) (str ns-str " is still on the classpath"))))
