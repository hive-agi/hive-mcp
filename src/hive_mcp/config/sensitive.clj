(ns hive-mcp.config.sensitive
  "Which config key names name a secret. A leaf: one regex shared by the
   config tool's response redaction and the config write policy."
  (:require [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def sensitive-name-re
  "A config key name that names a secret or an authenticator."
  #"(?i)(token|secret|passw|passphrase|credential|api[-_]?key|private[-_]?key|bearer|cookie|(^|[-_.])(keys?|pwd|auth|authz|authn|certs?|pins?|salt|signing|signature)($|[-_.]))")

(defn sensitive-name?
  "True when `k` (a keyword, string or symbol) names a secret."
  [k]
  (boolean (when (or (keyword? k) (string? k) (symbol? k))
             (re-find sensitive-name-re (name k)))))

(def ValueShape
  "What may be logged about a config value: its type and, for a string or a
   collection, its size. Never the value or any part of it."
  [:map
   [:type :string]
   [:size {:optional true} nat-int?]])

(defn value-shape
  "The loggable shape of config value `v`: {:type t} plus :size (characters
   of a string, entries of a collection, counted up to 100000). Nothing of the
   value itself, so a log line built from it cannot carry a secret, whatever
   key it was written under or wherever in the value the secret sits."
  [v]
  (cond
    (nil? v)     {:type "nil"}
    (string? v)  {:type "string" :size (count v)}
    (map? v)     {:type "map" :size (bounded-count 100000 v)}
    (vector? v)  {:type "vector" :size (bounded-count 100000 v)}
    (set? v)     {:type "set" :size (bounded-count 100000 v)}
    (coll? v)    {:type "list" :size (bounded-count 100000 v)}
    (keyword? v) {:type "keyword"}
    (boolean? v) {:type "boolean"}
    (number? v)  {:type "number"}
    :else        {:type (.getSimpleName (class v))}))

(m/=> value-shape [:=> [:cat :any] ValueShape])

(m/=> sensitive-name? [:=> [:cat :any] :boolean])
