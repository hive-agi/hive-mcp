(ns hive-mcp.dsl.param-domain
  "Param-addressability vocabulary for the batch DSL.

   Classifies a compiled op's param keys into three roles:
     meta        — batch plumbing, never walked for refs
     prose       — human text stored verbatim; a `$ref:`/`$N` inside is
                   quotation, never collected as a dependency nor substituted
     addressable — everything else; `$ref:` strings are live references

   Pure leaf: no requires, safe from any stratum (compiler, executor,
   extensions).")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def meta-param-keys
  "Batch plumbing keys — excluded from ref collection AND substitution."
  #{:id :tool :command :depends_on :wave})

(def entity-id-key
  "Op key carrying a caller's entity id beside the op's batch label (:id).
   The executor hands its value to the handler as :id. Not a meta key: a
   `$ref:` value here is collected and resolved like any other param."
  :entity_id)

(defn op-label?
  "True when `id` is a compiler-owned op label (a string starting with `$`),
   which names an op and never an entity."
  [id]
  (and (string? id) (.startsWith ^String id "$")))

(def prose-param-keys
  "Params that carry prose. A `$ref:` inside is quotation, not a reference."
  #{:content :description :title :message :prompt :query :text :task
    :commit_msg :reason})

(defn prose-param?
  "Is k a prose param key?"
  [k]
  (contains? prose-param-keys k))

(defn ref-walkable-entries
  "op -> seq of [k v] whose values participate in ref collection/substitution:
   every entry that is neither meta nor prose."
  [op]
  (remove (fn [[k _]]
            (or (contains? meta-param-keys k)
                (contains? prose-param-keys k)))
          op))
