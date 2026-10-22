(ns hive-mcp.config.io
  "Config file IO — thin shim over hive-di.file.
   Project-local constants (paths) stay here; primitive IO + perm-hardening
   live in hive-di.file. Effect boundary preserved: this ns only forwards."
  (:require [hive-di.file :as di-file]
            [hive-dsl.result :as result]
            [taoensso.timbre :as log]
            [hive-mcp.config.path :as config-path]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Paths
;; =============================================================================

(def config-path
  "Canonical path for global hive config (see hive-mcp.config.path)."
  config-path/config-path)

(def legacy-config-path
  "Legacy path for backward compatibility migration."
  (str (System/getProperty "user.home") "/.config/hive.edn"))

;; =============================================================================
;; Read / Write
;; =============================================================================

(defn read-config-file
  "Read and parse an EDN config file.
   Returns Result: (ok map) on success, (ok nil) if file missing or not a map,
   (err :io/config-read ...) on parse/IO exception.
   Delegates to hive-di.file/read-edn."
  [path]
  (let [r (di-file/read-edn path)]
    (cond
      (and (result/ok? r) (map? (:ok r))) r
      (result/ok? r)                      (result/ok nil)
      :else                               r)))

(m/=> read-config-file [:=> [:cat :string] [:map [:ok {:optional true} [:maybe :map]] [:error {:optional true} :any]]])

(defn write-config!
  "Write config map to disk as EDN. Creates parent dirs if needed.
   Perms hardened to 0600 — config holds secrets.
   A test JVM (-Dhive.test=true) is refused the operator's own config.edn: a
   test declares config, it never rewrites the developer's ~/.config/hive-mcp.
   Returns Result: (ok config) on success, (err :io/config-write ...) on failure."
  ([config] (write-config! config config-path))
  ([config path]
   (if (and (config-path/test-jvm?)
            (= (.getCanonicalPath (java.io.File. ^String path))
               (.getCanonicalPath (java.io.File. ^String config-path/operator-config-path))))
     (do (log/error "Refused: a test JVM tried to write the operator's config.edn" {:path path})
         (result/err :io/config-write-refused {:path path :reason :test-jvm}))
     (let [r (di-file/write-edn! path config {:secret? true})]
       (when (result/ok? r)
         (log/info "Config written to" path))
       r))))

(m/=> write-config! [:function [:=> [:cat :map] :map] [:=> [:cat :map :string] :map]])
