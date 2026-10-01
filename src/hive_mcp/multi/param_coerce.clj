(ns hive-mcp.multi.param-coerce
  "Coerce the params `multi` forwards against the TARGET tool's inputSchema.

   Why this hop owns the decoding: an MCP client types every argument by the
   schema of the tool it is calling. `multi` declares only the common params,
   so each target-specific one (cluster's `kinds`, `labels`, `limit`, ...) is
   undeclared from the client's side and arrives as its JSON TEXT. A target
   then reads `(seq \"[\\\"pods\\\"]\")` as a list of characters and fails one
   kind per character. Only `multi` holds both the raw value and the name of
   the tool whose schema says what the value is, so the coercion belongs here,
   once, instead of in every target.

   Pure: schema + params in, Result out. Only STRING values for properties
   declared with a single non-string JSON type are touched; everything else
   passes through for the target's own validation."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-dsl.coerce :as coerce]
            [hive-dsl.result :as r]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private coercible-types
  "Declared JSON types a forwarded string may stand in for."
  #{"array" "object" "integer" "number" "boolean"})

(defn- declared-type
  "The single JSON type a property declares, or nil for anyOf / oneOf / a
   type vector, whose reading the target owns."
  [prop]
  (let [t (when (map? prop) (or (:type prop) (get prop "type")))]
    (when (string? t) t)))

(defn coercion-spec
  "{param-keyword json-type} for every property of INPUT-SCHEMA whose declared
   type is coercible. Property names may be strings or keywords."
  [input-schema]
  (let [props (or (:properties input-schema) (get input-schema "properties"))]
    (into {}
          (keep (fn [[k prop]]
                  (let [t (declared-type prop)]
                    (when (coercible-types t)
                      [(keyword (name k)) t]))))
          props)))

(defn- ->object
  "A JSON object string decoded to a map (string keys, as the wire delivers)."
  [v]
  (if (str/starts-with? (str/trim v) "{")
    (try
      (let [parsed (json/read-str v)]
        (if (map? parsed)
          (r/ok parsed)
          (r/err :coerce/invalid-object {:message "JSON parsed to non-object" :value v})))
      (catch Exception e
        (r/err :coerce/invalid-object
               {:message (str "Invalid JSON object: " (ex-message e)) :value v})))
    (r/err :coerce/invalid-object
           {:message (str "Expected object, got string: \""
                          (subs v 0 (min 50 (count v))) "\"")
            :value v})))

(defn- coerce-value
  [json-type v]
  (case json-type
    "array"   (coerce/->vec v)
    "object"  (->object v)
    "integer" (coerce/->int v)
    "number"  (coerce/->double v)
    "boolean" (coerce/->boolean v)))

(defn coerce-params
  "Result of PARAMS with every string value whose key INPUT-SCHEMA declares as
   array / object / integer / number / boolean decoded to that type.

   A nil schema is a no-op ({:ok params}). The first value that cannot be
   decoded fails the whole call with :param naming it: forwarding the raw
   string instead would hand the target the very text it misreads."
  [input-schema params]
  (reduce-kv
   (fn [acc k json-type]
     (let [v (get params k)]
       (if (string? v)
         (let [res (coerce-value json-type v)]
           (if (r/ok? res)
             (update acc :ok assoc k (:ok res))
             (reduced (assoc res
                             :param (name k)
                             :message (str (name k) ": " (:message res))))))
         acc)))
   (r/ok params)
   (coercion-spec input-schema)))
