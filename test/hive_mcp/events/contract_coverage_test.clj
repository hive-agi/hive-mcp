(ns hive-mcp.events.contract-coverage-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as reader-types]
            [hive-mcp.events.core]
            [hive-mcp.events.bridge]
            [hive-schemas.coverage :as coverage]))

;; `coverage` enumerates defn only; facade `def` exports and defmulti need
;; their own explicit gate until the shared coverage scanner supports them.
(def ^:private facade-exemptions
  (into {}
        (concat
         (map (fn [n] [(symbol "hive-mcp.events.core" (name n))
                       "Facade export of a hive.events primitive: the public def captures its value; registration/interceptor implementation and contract belong to hive.events."])
              '[->interceptor enqueue trim-v reg-fx reg-cofx inject-cofx])
         (map (fn [n] [(symbol "hive-mcp.events.core" (name n))
                       "Legacy context facade holds the context Var as a value; the implementation and function contract live in hive-mcp.events.context."])
              '[get-coeffect assoc-coeffect update-coeffect get-effect assoc-effect update-effect])
         (map (fn [n] [(symbol "hive-mcp.events.core" (name n))
                       "Legacy metrics facade delegates to the contracted hive-mcp.events.metrics Var; the facade adds no behavior."])
              '[get-metrics reset-metrics! configure-metrics!])
         (map (fn [n] [(symbol "hive-mcp.events.core" (name n))
                       "Legacy registry facade holds the contracted hive-mcp.events.registry Var as a value; registry behavior belongs to that namespace."])
              '[reg-event append-interceptor! get-interceptors handler-registered?
                unreg-event unreg-fx unreg-cofx registered-events registered-effects
                registered-coeffects handler-registry-status reset-all!])
         (map (fn [n] [(symbol "hive-mcp.events.core" (name n))
                       "Legacy dispatch facade holds the contracted hive-mcp.events.dispatch Var as a value; execution behavior belongs to that namespace."])
              '[interceptor? execute do-fx dispatch dispatch-sync validate-event]))))

(def ^:private bridge-exemptions
  {'hive-mcp.events.bridge/hook->event
   "This public defmulti is a method table, not a defn; m/=> on its dispatch function would not contract method inputs or their nil/event-vector outputs. The bridge methods are exercised through the bridge dispatch tests."})

(defn- source-forms [path]
  (let [r (reader-types/string-push-back-reader (slurp path))]
    (binding [reader/*read-eval* false]
      (loop [forms []]
        (let [form (reader/read {:eof ::end} r)]
          (if (= ::end form) forms (recur (conj forms form))))))))

(deftest facade-and-multimethod-exports-are-explicitly-exempted
  (let [forms (source-forms "src/hive_mcp/events/core.clj")
        aliases (into #{}
                      (comp (filter #(and (seq? %) (= 'def (first %))
                                          (symbol? (second %))
                                          (or (symbol? (last %))
                                              (and (seq? (last %)) (= 'var (first (last %)))))))
                            (map #(symbol "hive-mcp.events.core" (name (second %)))))
                      forms)
        callable-aliases (disj aliases 'hive-mcp.events.core/metrics 'hive-mcp.events.core/debug)
        multimethods (into #{}
                           (comp (filter #(and (seq? %) (= 'defmulti (first %))))
                                 (map #(symbol "hive-mcp.events.bridge" (name (second %)))))
                           (source-forms "src/hive_mcp/events/bridge.clj"))]
    (is (= 33 (count callable-aliases)))
    (is (= callable-aliases (set (keys facade-exemptions))))
    (is (= multimethods (set (keys bridge-exemptions))))
    (is (every? #(and (string? %) (> (count %) 20))
                (vals (merge facade-exemptions bridge-exemptions))))))

;; The adjacent handlers.clj and handlers/ subtree belong to batch m11.
;; Enumerating the root files preserves the complete m10 universe without
;; measuring a neighboring batch whose source cannot be edited here.
(coverage/deftest-contract-coverage root-event-contracts
  {:paths ["src/hive_mcp/events/bridge.clj"
           "src/hive_mcp/events/channel_bridge.clj"
           "src/hive_mcp/events/config.clj"
           "src/hive_mcp/events/context.clj"
           "src/hive_mcp/events/contributions.clj"
           "src/hive_mcp/events/core.clj"
           "src/hive_mcp/events/dispatch.clj"
           "src/hive_mcp/events/effects.clj"
           "src/hive_mcp/events/metrics.clj"
           "src/hive_mcp/events/registry.clj"
           "src/hive_mcp/events/schemas.clj"
           "src/hive_mcp/events/write_events.clj"]})

(coverage/deftest-contract-coverage effect-submodule-contracts
  {:paths ["src/hive_mcp/events/effects"]})
