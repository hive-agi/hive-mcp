(ns hive-mcp.resilience.store-test
  "Name-matched facets for `hive-mcp.resilience.store`.

     transient-failure?  trifecta over the contract's own Throwable_
                         generator
     railway             golden over scripted thunks, run once with NO
                         memory store and once with a stub store installed
                         through the registry port. The stub satisfies
                         IMemoryStore and IMemoryStoreLiveness and records
                         every kick and await (with its budget), so the
                         snapshot pins the Result, the attempts, and the
                         heal-loop calls per classification. The stub's
                         await answers at once: no clock, no sleep.

   The fixture saves whatever :default store the JVM holds and puts it back."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.resilience.classify :as classify]
            [hive-mcp.resilience.runner :as runner]
            [hive-mcp.resilience.store :as store]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.memory.ports :as ports]
            [hive-spi.memory.registry :as registry]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f]
    (let [prior (get (registry/registered-stores) :default)]
      (try (registry/unregister-store! :default)
           (f)
           (finally
             (registry/unregister-store! :default)
             (when prior (registry/register-store! :default prior)))))))

(deftrifecta-from-schema transient-failure?-tracks-classify
  #'store/transient-failure?
  {:in [:cat classify/Throwable_]
   :out :boolean
   :rel (fn [[t] out] (= out (= :err/transient (:adt/variant (classify/classify t)))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 300})

(defn- liveness-stub
  "Stub memory store: satisfies the two ports `store` reaches through,
   records kick / await calls in `log`, and reports alive? from `alive?`."
  [log alive?]
  (reify
    ports/IMemoryStore
    ports/IMemoryStoreLiveness
    (-probe! [_] alive?)
    (-kick-reconnect! [_] (swap! log conj :kick) nil)
    (-await-reconnect! [_ budget-ms] (swap! log conj [:await budget-ms]) alive?)))

(def ^:private throwables
  {:transient (fn [] (java.io.IOException. "Connection reset"))
   :schema    (fn [] (ex-info "Milvus code=1804" {:milvus-clj.client/transport :http
                                                  :code 1804 :message "dim"}))
   :fatal     (fn [] (ex-info "too large" {:err/tag :embedder/input-too-large}))})

(defn- show [^Throwable t] (str (.getSimpleName (class t)) ": " (.getMessage t)))

(defn- scripted
  "Run the Result railway over a thunk following `script` (a value is
   returned, a `throwables` key is thrown; the last entry repeats)."
  [script budget]
  (let [calls (atom 0)
        thunk (fn []
                (let [step (nth script (min (dec (count script)) @calls))]
                  (swap! calls inc)
                  (if-let [mk (and (keyword? step) (throwables step))]
                    (throw (mk))
                    step)))
        res   (store/call-with-resilience-result thunk budget)]
    {:result (cond-> res
               (:throwable res) (update :throwable show)
               (:class res)     (update :class :adt/variant))
     :calls  @calls
     :valid? (m/validate runner/Result res)}))

(defn- throwing
  "The legacy surface: the value, or what it threw."
  [script]
  (let [calls (atom 0)
        thunk (fn []
                (let [step (nth script (min (dec (count script)) @calls))]
                  (swap! calls inc)
                  (if-let [mk (and (keyword? step) (throwables step))]
                    (throw (mk))
                    step)))]
    (try {:value (store/call-with-resilience thunk 7)}
         (catch Throwable t
           {:thrown (show t) :tag (:err/tag (ex-data t))}))))

(def ^:private scripts
  {:ok                 [:fine]
   :transient-recovers [:transient :recovered]
   :transient-twice    [:transient]
   :schema-mismatch    [:schema]
   :fatal              [:fatal]})

(deftest-golden railway-with-and-without-a-liveness-store-golden
  "test/golden/hive-mcp/resilience/store-railway.edn"
  (let [no-store (into (sorted-map)
                       (for [[k s] scripts] [k (assoc (scripted s 5) :kick-and-wait (store/kick-and-wait! 5))]))
        log      (atom [])
        _        (registry/register-store! :default (liveness-stub log true))
        with-store (into (sorted-map)
                         (for [[k s] scripts]
                           (do (reset! log [])
                               [k (assoc (scripted s 5) :heal-calls @log)])))
        _        (reset! log [])
        kick     {:default-budget (store/kick-and-wait!) :explicit (store/kick-and-wait! 9)
                  :calls @log}
        legacy   (into (sorted-map) (for [[k s] scripts] [k (throwing s)]))
        _        (registry/register-store! :default (liveness-stub log false))
        dead     (store/kick-and-wait! 3)]
    {:no-store no-store :with-store with-store :kick-and-wait kick
     :legacy legacy :dead-store-kick dead}))

(deftest a-store-without-the-liveness-port-is-never-kicked
  (registry/register-store! :default (reify ports/IMemoryStore))
  (is (false? (store/kick-and-wait! 1))))
