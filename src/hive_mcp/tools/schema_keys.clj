(ns hive-mcp.tools.schema-keys
  "Provider-legal property names for every tool schema the host emits.

   LLM providers reject a tool whose inputSchema has a property key outside
   `^[a-zA-Z0-9_.-]{1,64}$` (Anthropic: 400 \"Property keys should match
   pattern\"). One such key on one tool fails the WHOLE request, so a single
   addon param named `kg-rank?` stopped every ling from starting.

   Addons keep their own Clojure-style names (`:preview?`); the projection
   that builds MCP/provider schemas emits only legal ones:

     legal key           kept as is
     `foo?`              advertised as `foo_`, the JSON alias carto (and any
                         handler folding through the same convention) reads
                         back as `:foo?`; dropped when `foo_` is already
                         declared, since the alias is then advertised already
     any other illegal   dropped: no spelling of it can reach a handler

   Calculations only (stratified L1).")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def legal-key-pattern
  "What a provider accepts as a property key."
  #"^[a-zA-Z0-9_.-]{1,64}$")

(defn- key-name [k]
  (cond (keyword? k) (if (namespace k) (str (namespace k) "/" (name k)) (name k))
        (string? k)  k
        :else        (str k)))

(defn legal-key?
  "True when property key `k` (keyword or string) is provider-legal."
  [k]
  (boolean (re-matches legal-key-pattern (key-name k))))

(defn- alias-key
  "The legal alias of an illegal key, in the key's own type, or nil."
  [k]
  (let [n (key-name k)]
    (when (and (> (count n) 1) (= \? (.charAt ^String n (dec (count n)))))
      (let [a (str (subs n 0 (dec (count n))) "_")]
        (when (legal-key? a)
          (if (keyword? k) (keyword a) a))))))

(defn legal-properties
  "`props` with every key provider-legal (see the namespace docstring)."
  [props]
  (if (every? legal-key? (keys props))
    props
    (let [declared (into #{} (map key-name) (keys props))]
      (reduce-kv (fn [acc k v]
                   (cond
                     (legal-key? k) (assoc acc k v)
                     :else (let [a (alias-key k)]
                             (if (and a (not (contains? declared (key-name a))))
                               (assoc acc a v)
                               acc))))
                 (empty props)
                 props))))

(defn- legal-required [required props]
  (let [present (into #{} (map key-name) (keys props))]
    (cond-> required
      (sequential? required)
      (->> (map #(if (or (legal-key? %) (not (alias-key %))) % (key-name (alias-key %))))
           (filter #(contains? present (key-name %)))
           vec))))

(defn legal-tool
  "Tool def `tool` with its :inputSchema properties (and :required) made
   provider-legal. A tool without properties is returned unchanged."
  [tool]
  (let [props (get-in tool [:inputSchema :properties])]
    (if (and (map? props) (not (every? legal-key? (keys props))))
      (let [props' (legal-properties props)]
        (cond-> (assoc-in tool [:inputSchema :properties] props')
          (contains? (:inputSchema tool) :required)
          (update-in [:inputSchema :required] legal-required props')))
      tool)))

(defn illegal-keys
  "Property keys of `tool` a provider would reject. Empty when legal."
  [tool]
  (vec (remove legal-key? (keys (get-in tool [:inputSchema :properties])))))
