(ns hive-mcp.server.routes.wire-coerce
  "Decode wire text into the scalar types a tool's inputSchema declares.

   WIRE-NUMBERS (kanban 20261002211406-17100247): a client that calls a tool
   through a composite, a CLI bridge or a loosely typed model routinely sends
   `parallelism` as \"8\" and `force` as \"false\". Each handler then either
   crashes on `(long \"8\")` or reads the text \"false\" as truthy. Coercing in
   every handler (what `figures` did with as-int / as-bool) leaves every other
   handler wrong; the tool's own inputSchema already says what each value is,
   so the decoding happens once, here, before dispatch.

   Rules, all pure:
   - only properties declaring a SINGLE `type` of integer / number / boolean
     are touched (a type vector or anyOf/oneOf belongs to the handler);
   - only STRING values are touched;
   - the text must be a JSON literal of that type (integer: -?digits that fit
     a long; number: the JSON number grammar; boolean: true / false). Text
     that does not parse is left exactly as it came, so the handler's own
     validation still reports it."
  (:require [clojure.string :as str]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private json-integer #"-?(?:0|[1-9]\d*)")
(def ^:private json-number  #"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?")

(defn- decode-integer [s]
  (let [t (str/trim s)]
    (when (re-matches json-integer t) (parse-long t))))

(defn- decode-number [s]
  (let [t (str/trim s)]
    (when (re-matches json-number t)
      (or (when (re-matches json-integer t) (parse-long t))
          (let [d (parse-double t)]
            (when (and d (Double/isFinite d)) d))))))

(defn- decode-boolean [s]
  (case (str/trim s)
    "true"  true
    "false" false
    nil))

(def ^:private decoders
  {"integer" decode-integer
   "number"  decode-number
   "boolean" decode-boolean})

(defn- declared-type [prop]
  (let [t (when (map? prop) (or (:type prop) (get prop "type")))]
    (when (string? t) t)))

(defn scalar-spec
  "{param-name decoder} for every property of INPUT-SCHEMA declaring a single
   integer / number / boolean type. Names are strings, whatever the schema's
   key type."
  [input-schema]
  (let [props (when (map? input-schema)
                (or (:properties input-schema) (get input-schema "properties")))]
    (into {}
          (keep (fn [[k prop]]
                  (when-let [decode (decoders (declared-type prop))]
                    [(name k) decode])))
          props)))

(defn- coerce-with-spec [spec params]
  (if (or (empty? spec) (not (map? params)))
    params
    (reduce-kv (fn [acc k v]
                 (if-let [decode (and (string? v)
                                      (or (string? k) (keyword? k))
                                      (spec (name k)))]
                   (let [decoded (decode v)]
                     (if (some? decoded) (assoc acc k decoded) acc))
                   acc))
               params
               params)))

(defn coerce-wire-params
  "PARAMS with each string value whose key INPUT-SCHEMA declares as integer /
   number / boolean decoded to that type. Keys may be strings or keywords;
   text that does not parse, and every other value, passes through."
  [input-schema params]
  (coerce-with-spec (scalar-spec input-schema) params))

(defn wrap-handler-wire-coerce
  "Handler that decodes its args against INPUT-SCHEMA before calling HANDLER.
   The spec is computed once, when the tool is built."
  [handler input-schema]
  (let [spec (scalar-spec input-schema)]
    (if (empty? spec)
      handler
      (fn [args] (handler (coerce-with-spec spec args))))))

(m/=> coerce-wire-params [:=> [:cat :any :any] :any])
