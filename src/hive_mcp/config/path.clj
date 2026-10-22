(ns hive-mcp.config.path
  "Where the global config.edn lives. A leaf: no requires, so every config
   reader can depend on it without pulling anything else in."
  (:require [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn resolve-config-path
  "The config.edn path for an HIVE_MCP_CONFIG value and a home directory.
   A non-blank ENV-PATH wins; otherwise HOME/.config/hive-mcp/config.edn."
  [env-path home]
  (if (and (string? env-path) (not (.isBlank ^String env-path)))
    env-path
    (str home "/.config/hive-mcp/config.edn")))
(m/=> resolve-config-path
      [:=> [:cat [:any {:gen/schema [:maybe :string]}]
                  [:any {:gen/schema :string}]] :string])

(def config-path
  "The global config.edn of this process. The hive.mcp.config system property,
   then HIVE_MCP_CONFIG, names another file, so a second instance (or a test
   JVM) runs from its own config."
  (resolve-config-path (or (System/getProperty "hive.mcp.config")
                           (System/getenv "HIVE_MCP_CONFIG"))
                       (System/getProperty "user.home")))

(def operator-config-path
  "The operator's own config.edn, whatever override this process runs under.
   A test JVM must never write it."
  (resolve-config-path nil (System/getProperty "user.home")))

(defn test-jvm?
  "True when this JVM was started by a test alias (-Dhive.test=true)."
  []
  (= "true" (System/getProperty "hive.test")))

(m/=> test-jvm? [:=> [:cat] :boolean])
