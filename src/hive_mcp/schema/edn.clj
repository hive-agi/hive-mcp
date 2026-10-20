(ns hive-mcp.schema.edn
  "Read shared EDN schema files and produce Malli schemas.

   Schema files live in resources/schema/*.edn and are the single
   source of truth for CLJ↔CLJEL contracts. Both the JVM (this ns)
   and Emacs (hive-mcp-schema.cljel via parseedn) read the same files.

   Usage:
     (schema-for :cider/connect-session)
     ;; => Malli schema object ready for m/validate"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private file-cache
  (atom {}))

(defonce ^:private schema-cache
  (atom {}))

(def SchemaKey
  "A namespaced schema key such as :cider/connect-session. Its namespace names
   the schema file; an unqualified key names no file and resolves to nil."
  [:or :qualified-keyword :keyword])

(def SchemaFile
  "A parsed schema EDN file: schema key -> raw malli form."
  [:map-of :keyword :any])

(def SchemaObject
  "A compiled malli schema, ready for m/validate."
  [:fn {:gen/schema [:enum :int :string :boolean [:map [:name :string]]]
        :gen/fmap m/schema}
   m/schema?])

(defn load-schema-file
  "Load and parse a schema EDN file from classpath.
   ns-name is e.g. \"cider\" -> reads schema/cider.edn"
  [ns-name]
  (or (get @file-cache ns-name)
      (let [path (str "schema/" ns-name ".edn")]
        (when-let [r (io/resource path)]
          (let [schemas (edn/read-string (slurp r))]
            (swap! file-cache assoc ns-name schemas)
            schemas)))))

;; ns-name is nil when schema-for is handed an unqualified key: the path is
;; then schema/.edn, which is absent, so the answer is nil.
(m/=> load-schema-file [:=> [:cat [:maybe :string]] [:maybe SchemaFile]])

(defn schema-for
  "Get Malli schema for a namespaced key like :cider/connect-session.
   Loads the corresponding EDN file on first access and caches the result."
  [key]
  (or (get @schema-cache key)
      (let [ns-part (namespace key)
            schemas (load-schema-file ns-part)
            raw (get schemas key)]
        (when raw
          (let [s (m/schema raw)]
            (swap! schema-cache assoc key s)
            s)))))

(m/=> schema-for [:=> [:cat SchemaKey] [:maybe SchemaObject]])

(defn validate
  "Validate params against EDN-defined schema.
   Returns nil on success, throws ex-info on failure."
  [key params]
  (let [s (schema-for key)]
    (when-not s
      (throw (ex-info (str "Schema not found: " key) {:key key})))
    (when-not (m/validate s params)
      (throw (ex-info "Invalid params"
                      {:type :validation
                       :schema key
                       :errors (m/explain s params)})))))

(m/=> validate [:=> [:cat SchemaKey :any] :nil])

(defn valid?
  "Return true if params match the EDN-defined schema."
  [key params]
  (when-let [s (schema-for key)]
    (m/validate s params)))

(m/=> valid? [:=> [:cat SchemaKey :any] [:maybe :boolean]])

(defn reload!
  "Clear all caches and force re-read from disk on next access."
  []
  (reset! file-cache {})
  (reset! schema-cache {}))

(m/=> reload! [:=> [:cat] [:map-of {:max 0} :any :any]])

(comment
  ;; Load a schema from EDN
  (schema-for :cider/connect-session)

  ;; Validate
  (m/validate (schema-for :cider/connect-session)
              {:name "sisf" :port 7902 :repl_type "cljs"})
  ;; => true

  (m/validate (schema-for :cider/connect-session)
              {:name "" :port 0})
  ;; => false

  ;; Explain errors
  (m/explain (schema-for :cider/connect-session)
             {:name "test" :port 70000})
  )
