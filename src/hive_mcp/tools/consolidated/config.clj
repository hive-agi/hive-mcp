(ns hive-mcp.tools.consolidated.config
  "Consolidated Config CLI tool for managing hive-mcp configuration."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.result-bridge :as rb]
            [hive-mcp.dns.result :as result]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.schema :as schema]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [clojure.edn :as edn]
            [hive-mcp.config.path :as config-path]
            [malli.core :as m]
            [hive-mcp.tools.core :as tc]
            [clojure.string :as str]
            [hive-mcp.config.io :as config-io]
            [hive-mcp.config.secrets :as secrets]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ── Pure Result-returning functions ───────────────────────────────────────────

;; ── Secret redaction (response edge) ─────────────────────────────────────────

(def ^:private sensitive-name-re
  #"(?i)(token|secret|passw|passphrase|credential|api[-_]?key|private[-_]?key|(^|[-_.])keys?($|[-_.]))")

(defn- sensitive-key?
  [k]
  (boolean (when (or (keyword? k) (string? k) (symbol? k))
             (re-find sensitive-name-re (name k)))))

(defn- blank-secret?
  [v]
  (or (nil? v) (and (string? v) (str/blank? v))))

(defn- pass-ref?
  [v]
  (and (string? v) (str/starts-with? v "pass:")))

(defn- raw-secrets
  "The :secrets map as declared, before resolution."
  []
  (if config/*config-source*
    (:secrets (config/get-global-config))
    (let [r (config-io/read-config-file config-io/config-path)]
      (when (contains? r :ok) (:secrets (:ok r))))))

(defn- env-var-name
  [k]
  (or (first (get secrets/secret-registry k))
      (when (keyword? k) (secrets/env-var-for k))))

(defn- secret-source
  [k v raw]
  (let [raw-v (when (map? raw) (get raw k))]
    (cond
      (or (pass-ref? raw-v) (pass-ref? v))  :pass
      (not (blank-secret? raw-v))           :config
      (when-let [ev (env-var-name k)]
        (not (str/blank? (System/getenv ev)))) :env
      :else                                  :pass)))

(defn- secret-state
  [k v raw]
  (if (blank-secret? v)
    {:state :unset}
    {:state :set :source (secret-source k v @raw)}))

(defn- redact-leaf
  [v]
  (cond
    (blank-secret? v) {:state :unset}
    (pass-ref? v)     {:state :set :source :pass}
    :else             {:state :set :source :config}))

(defn- empty-like
  [v]
  (if (record? v) {} (empty v)))

(defn- map-coll
  [f v]
  (if (seq? v) (doall (map f v)) (into (empty v) (map f) v)))

(defn- redact-tree
  [v secret?]
  (cond
    (map? v)  (into (empty-like v)
                    (map (fn [[k x]] [k (redact-tree x (or secret? (sensitive-key? k)))]))
                    v)
    (coll? v) (map-coll #(redact-tree % secret?) v)
    (and secret? (or (string? v) (number? v))) (redact-leaf v)
    :else     v))

(defn- redact-secrets-map
  [m raw]
  (if (map? m)
    (into (empty-like m)
          (map (fn [[k v]] [k (if (coll? v) (redact-tree v true) (secret-state k v raw))]))
          m)
    (redact-tree m true)))

(defn- redact-at
  "Redact `v` as the value sitting at key `path` of the config tree."
  [path v raw]
  (let [path (vec path)]
    (cond
      (= [:secrets] path)                     (redact-secrets-map v raw)
      (and (= :secrets (first path)) (= 2 (count path)) (not (coll? v)))
      (secret-state (second path) v raw)
      (= :secrets (first path))               (redact-tree v true)
      (some sensitive-key? path)              (redact-tree v true)
      (map? v)  (into (empty-like v)
                      (map (fn [[k x]] [k (redact-at (conj path k) x raw)]))
                      v)
      (coll? v) (map-coll #(redact-at path % raw) v)
      :else     v)))

(defn- redact-config
  "Redact `v` found at `path`; the declared secrets are read only on demand."
  ([v] (redact-config [] v))
  ([path v] (redact-at path v (delay (raw-secrets)))))

(defn- redact-errors
  "Malli explanation errors: each :value is redacted by its own :in path."
  [base errors]
  (let [raw (delay (raw-secrets))]
    (mapv (fn [e]
            (if (and (map? e) (contains? e :value))
              (assoc e :value (redact-at (into (vec base) (:in e)) (:value e) raw))
              e))
          errors)))

(defn- secret-strings
  [v secret?]
  (cond
    (map? v)  (mapcat (fn [[k x]] (secret-strings x (or secret? (sensitive-key? k)))) v)
    (coll? v) (mapcat #(secret-strings % secret?) v)
    (and secret? (or (string? v) (number? v)) (>= (count (str/trim (str v))) 4)) [(str/trim (str v))]
    :else     nil))

(defn- scrub-text
  "Backstop: replace any live secret string that still reached the text."
  [text]
  (if-not (string? text)
    text
    (let [ss (->> (secret-strings (config/get-global-config) false)
                  distinct
                  (sort-by count >))]
      (reduce (fn [t s]
                (-> t
                    (str/replace s "[redacted]")
                    (str/replace (str/replace s "/" "\\/") "[redacted]")))
              text ss))))

(defn- respond
  [category f]
  (update (rb/result->mcp (rb/try-result category f)) :text scrub-text))

(defn- get*
  [{:keys [key]}]
  (if (or (nil? key) (= "" key))
    (result/err :config/get {:message "Missing required parameter: key. Example: config get {\"key\": \"embeddings.ollama.host\"}"})
    (result/ok {:key key
                :value (redact-config (config/parse-key-path key)
                                      (config/get-config-value key))})))

(defn- coerce-value
  "EDN-aware coercion for `config set value=...`. MCP transports often
   stringify scalars — `true` arrives as `\"true\"`, `:foo` as `\":foo\"`.
   Without coercion, downstream resolvers comparing `(true? v)` or
   `(= :foo v)` silently fail. This helper restores intent:

     \"true\"/\"false\"        → boolean
     \":kw\"                 → keyword
     numeric string         → long/double
     EDN collection literal → vector/map/set/list
     anything else          → unchanged string

   Non-string inputs pass through untouched (booleans/numbers arriving
   typed already from a richer transport)."
  [v]
  (if-not (string? v)
    v
    (cond
      (= "true" v)               true
      (= "false" v)              false
      (re-matches #"-?\d+" v)    (try (Long/parseLong v) (catch Throwable _ v))
      (re-matches #"-?\d+\.\d+" v) (try (Double/parseDouble v) (catch Throwable _ v))
      (.startsWith ^String v ":") (keyword (subs v 1))
      (re-matches #"^[\[(\{#].*" v) (try (edn/read-string v) (catch Throwable _ v))
      :else                      v)))

(defn- set*
  [{:keys [key value]}]
  (if (or (nil? key) (= "" key))
    (result/err :config/set
                {:message (str "Missing required parameter: key. Example: "
                               "config set {\"key\": \"embeddings.ollama.host\", "
                               "\"value\": \"http://new:11434\"}")})
    (let [coerced (coerce-value value)
          kp      (config/parse-key-path key)
          shown   (redact-config kp coerced)
          secret? (not= shown coerced)]
      (config/set-config-value! key coerced)
      (log/info "Config set:" key (if secret? "(value redacted)" (str "= " coerced)))
      (result/ok {:key key
                  :value shown
                  :raw-input (if secret? shown value)
                  :coerced? (not= coerced value)
                  :status "updated"}))))

(defn- list-config*
  [_params]
  (result/ok {:config (redact-config (config/get-global-config))}))

(defn- reload*
  [_params]
  (let [{:keys [loaded rewire]} ((requiring-resolve 'hive-mcp.server.config-reload/reload!))]
    (result/ok {:status "reloaded"
                :keys (vec (keys loaded))
                :rewire rewire})))

(defn- path*
  "Return config file path info."
  [_params]
  (result/ok {:config-path config-path/config-path
              :exists? (.exists (io/file config-path/config-path))}))

(defn- validate*
  "Validate current config against schema. Optionally validate a section.
   The echoed config, section value and malli errors are redacted."
  [{:keys [key]}]
  (let [cfg (config/get-global-config)]
    (if (and key (not= key ""))
      (let [section-kw (keyword key)]
        (try
          (let [value (get cfg section-kw)
                v (schema/validate-section section-kw value)]
            (result/ok (cond-> (assoc v :section key)
                         (contains? v :value)
                         (update :value #(redact-config [section-kw] %))
                         (contains? v :errors)
                         (update :errors #(redact-errors [section-kw] %))
                         (contains? v :humanized)
                         (update :humanized #(redact-config [section-kw] %)))))
          (catch Exception e
            (result/err :config/validate {:message (ex-message e)}))))
      (let [v (schema/validate-config cfg)]
        (result/ok (cond-> v
                     (contains? v :config)    (update :config redact-config)
                     (contains? v :errors)    (update :errors #(redact-errors [] %))
                     (contains? v :humanized) (update :humanized redact-config)))))))

;; ── Public handlers (MCP boundary) ────────────────────────────────────────────

(defn handle-get
  "Read a config value by dotted key path. Secrets come back as state + source."
  [params]
  (respond :config/get-failed #(get* params)))

(m/=> handle-get [:=> [:cat [:maybe :map]] tc/McpResponse])

(defn handle-set
  "Set a config value at a dotted key path and persist to disk."
  [params]
  (respond :config/set-failed #(set* params)))

(m/=> handle-set [:=> [:cat [:maybe :map]] tc/McpResponse])

(defn handle-list
  "List all configuration values. Secrets come back as state + source."
  [params]
  (respond :config/list-failed #(list-config* params)))

(m/=> handle-list [:=> [:cat [:maybe :map]] tc/McpResponse])

(defn handle-reload
  "Reload config from disk, merging with defaults."
  [params]
  (respond :config/reload-failed #(reload* params)))

(m/=> handle-reload [:=> [:cat [:maybe :map]] tc/McpResponse])

(defn handle-path
  "Show config file path."
  [params]
  (respond :config/path-failed #(path* params)))

(m/=> handle-path [:=> [:cat [:maybe :map]] tc/McpResponse])

(defn handle-validate
  "Validate config against malli schema. Secrets come back as state + source."
  [params]
  (respond :config/validate-failed #(validate* params)))

(m/=> handle-validate [:=> [:cat [:maybe :map]] tc/McpResponse])

(def handlers
  "The `config` verbs, stored as VARS so a reload of THIS namespace reaches the
   table (20260817195749-0d407e9c)."
  {:get      #'handle-get
   :set      #'handle-set
   :list     #'handle-list
   :reload   #'handle-reload
   :path     #'handle-path
   :validate #'handle-validate})

(def handle-config
  (make-cli-handler #'handlers))

(def tool-def
  {:name "config"
   :consolidated true
   :description "Manage ~/.config/hive-mcp/config.edn: get (read value at key path), set (write value at key path), list (show all config), reload (re-read from disk), path (show config file location), validate (check config against schema, optionally key=section). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["get" "set" "list" "reload" "path" "validate" "help"]
                                         :description "Config operation to perform"}
                              "key" {:type "string"
                                     :description "Dotted key path (e.g. \"embeddings.ollama.host\"). For validate: section name (e.g. \"memory\")"}
                              "value" {:description "Value to set (string, number, boolean, or object)"}}
                 :required ["command"]}
   :handler #'handle-config})

(def tools [tool-def])
