(ns hive-dsl.hooks.defadt
  "clj-kondo analyze-call hook for hive-dsl.adt/defadt.

   defadt expands into 4 top-level vars:
     - <TypeName>             — type metadata def
     - <type-name>            — constructor fn (kebab-case)
     - <type-name>?           — predicate fn
     - -><type-name>          — keyword coercer

   Without this hook clj-kondo sees only the macro call and reports
   `unresolved-symbol` for any reference to the generated names."
  (:require [clj-kondo.hooks-api :as api]
            [clojure.string :as str]))

(defn- camel->kebab
  "Mirror of hive-dsl.adt/camel->kebab for the analyzer."
  [s]
  (-> s
      (str/replace #"([a-z])([A-Z])" "$1-$2")
      str/lower-case))

(defn defadt
  "Rewrite (defadt TypeName ?docstring & variants) into a `do` block of
   `def` forms so clj-kondo registers the generated names: TypeName,
   TypeNameMalli, the constructor, the predicate and the coercer. Variants
   themselves aren't validated here — the runtime macro still does that.
   Bodies are calls whose result type kondo cannot narrow (`map?`, `merge`,
   `get`), never a literal nil: a nil predicate body made every
   `(is (type-name? v))` a constant-false condition."
  [{:keys [node]}]
  (let [[_ type-sym & body] (:children node)
        type-name (api/sexpr type-sym)]
    (when-not (symbol? type-name)
      (throw (ex-info "defadt expects a symbol type name" {})))
    (let [kebab            (camel->kebab (name type-name))
          ctor-sym         (symbol kebab)
          pred-sym         (symbol (str kebab "?"))
          coerce-sym       (symbol (str "->" kebab))
          malli-sym        (symbol (str (name type-name) "Malli"))
          loc              (meta type-sym)
          mk-token         (fn [s] (with-meta (api/token-node s) loc))
          mk-list          (fn [xs] (api/list-node (vec xs)))
          ;; Walk body to keep variant exprs visible to kondo so any embedded
          ;; symbol/predicate references still get analyzed.
          variant-uses     (api/list-node
                            (list* (mk-token 'do)
                                   (mapv (fn [c]
                                           (api/list-node
                                            [(mk-token 'do) c]))
                                         body)))
          rewritten
          (mk-list
           [(mk-token 'do)
            (mk-list [(mk-token 'def) (mk-token type-name)
                      (mk-list [(mk-token 'clojure.core/hash-map)])])
            (mk-list [(mk-token 'def) (mk-token malli-sym)
                      (mk-list [(mk-token 'clojure.core/vector)])])
            (mk-list
             [(mk-token 'defn) (mk-token ctor-sym)
              (mk-list [(api/vector-node [(mk-token 'variant)])
                        (mk-list [(mk-token 'clojure.core/hash-map)
                                  (api/keyword-node :adt/variant)
                                  (mk-token 'variant)])])
              (mk-list [(api/vector-node [(mk-token 'variant) (mk-token 'data)])
                        (mk-list [(mk-token 'clojure.core/merge)
                                  (mk-token 'data)
                                  (mk-list [(mk-token 'clojure.core/hash-map)
                                            (api/keyword-node :adt/variant)
                                            (mk-token 'variant)])])])])
            (mk-list
             [(mk-token 'defn) (mk-token pred-sym)
              (api/vector-node [(mk-token 'x)])
              (mk-list [(mk-token 'clojure.core/map?) (mk-token 'x)])])
            (mk-list
             [(mk-token 'defn) (mk-token coerce-sym)
              (api/vector-node [(mk-token 'kw)])
              (mk-list [(mk-token 'clojure.core/get)
                        (mk-token type-name)
                        (mk-token 'kw)])])
            variant-uses])]
      {:node rewritten})))
