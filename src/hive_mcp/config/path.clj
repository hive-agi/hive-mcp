(ns hive-mcp.config.path
  "Where the global config.edn lives. A leaf: no requires, so every config
   reader can depend on it without pulling anything else in.")
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

(def config-path
  "The global config.edn of this process. HIVE_MCP_CONFIG names another file,
   so a second instance runs from its own config."
  (resolve-config-path (System/getenv "HIVE_MCP_CONFIG")
                       (System/getProperty "user.home")))
