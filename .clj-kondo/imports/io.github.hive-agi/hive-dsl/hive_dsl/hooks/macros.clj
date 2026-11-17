(ns hive-dsl.hooks.macros
  "clj-kondo analyze-call hooks for hive-dsl binding/body macros whose call
   shape has no `:lint-as` equivalent."
  (:require [clj-kondo.hooks-api :as api]))

(defn- do-node
  "Wrap nodes in a `do` so kondo analyzes each as an expression."
  [nodes]
  (api/list-node (list* (api/token-node 'do) nodes)))

(defn- thread-unwrapped
  "Rewrite (ok-> expr f1 f2) into (do expr (fn [g] (-> g f1)) (fn [g] (-> g f2))).
   Every step sees an unknown-typed input: the macro unwraps the Result each
   step returns before handing it to the next, so threading the forms into
   each other would type a step's input as the previous step's Result.
   Arity checks on each form still apply."
  [thread-sym {:keys [node]}]
  (let [[_ expr & forms] (:children node)
        step (fn [form]
               (let [sym (api/token-node (gensym "ok"))]
                 (api/list-node
                  [(api/token-node 'fn)
                   (api/vector-node [sym])
                   (api/list-node [(api/token-node thread-sym) sym form])])))]
    {:node (api/list-node
            (list* (api/token-node 'do) expr (map step forms)))}))

(defn ok->
  "Hook for (ok-> expr & forms)."
  [ctx]
  (thread-unwrapped '-> ctx))

(defn ok->>
  "Hook for (ok->> expr & forms)."
  [ctx]
  (thread-unwrapped '->> ctx))

(defn expr-then-body
  "Hook for (with-gate g & body) / (with-gate-result g & body)."
  [{:keys [node]}]
  {:node (do-node (rest (:children node)))})

(defn adt-case
  "Hook for (adt-case type-ref expr & clauses). Preserve type-ref as a real var
   reference, then model variant/body pairs as case clauses."
  [{:keys [node]}]
  (let [[_ type-ref expr & clauses] (:children node)]
    {:node (api/list-node
            [(api/token-node 'let)
             (api/vector-node [(api/token-node '_) type-ref])
             (api/list-node
              (list* (api/token-node 'case) expr clauses))])}))

(defn with-scope
  "Hook for (with-scope [scope-sym] & body) — binds scope-sym over body.
   The scope is an atom at runtime; bind it to `(atom [])` so kondo types
   it as one (a nil binding made every scope-start!/swap! use an error)."
  [{:keys [node]}]
  (let [[_ binding-vec & body] (:children node)
        sym (first (:children binding-vec))]
    {:node (api/list-node
            (list* (api/token-node 'let)
                   (api/vector-node
                    [sym (api/list-node [(api/token-node 'clojure.core/atom)
                                         (api/vector-node [])])])
                   body))}))

(defn with-go-loop
  "Hook for (with-go-loop [sym handler-fn opts] & body) — binds sym over body,
   analyzes handler-fn and opts as expressions."
  [{:keys [node]}]
  (let [[_ binding-vec & body] (:children node)
        [sym & exprs] (:children binding-vec)]
    {:node (api/list-node
            (list* (api/token-node 'let)
                   (api/vector-node [sym (do-node exprs)])
                   body))}))
