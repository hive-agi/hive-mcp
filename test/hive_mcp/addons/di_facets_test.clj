(ns hive-mcp.addons.di-facets-test
  "Facets for the addon DI module. The service registry is process state:
   every test snapshots and restores it. A property checks the registry
   against a plain map model over generated register / deregister histories;
   a golden pins symbol resolution, IServiceConsumer reads, the dependency
   report and the shared addon pipeline end to end, with the addon a reified
   IAddon stub registered in (and removed from) the real addon registry."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-addon.protocol :as proto]
            [hive-mcp.addons.core :as addons]
            [hive-mcp.addons.di :as di]
            [hive-test.golden :refer [deftest-golden-fn]]
            [malli.generator :as mg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- isolate-services [f]
  (let [saved (into {} (map (juxt identity di/get-service)) (di/list-services))]
    (di/clear-services!)
    (try (f)
         (finally
           (di/clear-services!)
           (when (seq saved) (di/register-services! saved))))))

(use-fixtures :each isolate-services)

(def ^:private Op
  [:or
   [:tuple [:= :register] [:enum :svc/a :svc/b :svc/c] [:enum 1 "x" :v]]
   [:tuple [:= :deregister] [:enum :svc/a :svc/b :svc/c]]
   [:tuple [:= :bulk] [:map-of {:max 2} [:enum :svc/a :svc/c] [:enum 2 "y"]]]])

(defspec registry-follows-a-map-model 200
  (prop/for-all [ops (mg/generator [:vector {:max 8} Op])]
    (di/clear-services!)
    (let [model (reduce (fn [mdl [op k v]]
                          (case op
                            :register   (do (di/register-service! k v) (assoc mdl k v))
                            :deregister (do (di/deregister-service! k) (dissoc mdl k))
                            :bulk       (do (di/register-services! k) (merge mdl k))))
                        {} ops)]
      (and (= (set (keys model)) (di/list-services))
           (every? (fn [[k v]] (and (= v (di/get-service k)) (= v (di/require-service k))
                                    (di/service-registered? k)))
                   model)
           (= :default (di/get-service :svc/never :default))))))

(defrecord Consumer [required optional]
  di/IServiceConsumer
  (required-services [_] required)
  (optional-services [_] optional))

(defn- stub-addon
  "A reified IAddon (optionally an IServiceConsumer) that records the
   config its initialize! received."
  [id seen required optional]
  (reify
    proto/IAddon
    (addon-id [_] id)
    (addon-type [_] :native)
    (capabilities [_] #{})
    (initialize! [_ config] (reset! seen config) {:success? true :errors [] :metadata {}})
    (shutdown! [_] {:success? true :errors []})
    (tools [_] [])
    (schema-extensions [_] {})
    (health [_] {:status :ok})
    di/IServiceConsumer
    (required-services [_] required)
    (optional-services [_] optional)))

(defn- pipeline-view [ctx]
  (when ctx
    {:keys        (vec (sort (keys ctx)))
     :reg-ok?     (get-in ctx [:reg-result :success?])
     :init-ok?    (get-in ctx [:init-result :success?])
     :services    (:injected-services ctx)}))

(defn- var-name
  "A resolved var as its qualified symbol, so the golden stays readable EDN."
  [v]
  (when v (symbol v)))

(defn- var-map [m]
  (when m (into (sorted-map) (map (fn [[k v]] [k (var-name v)])) m)))

(defn- sorted-vec [s] (vec (sort s)))

(defn- deps-view [report]
  (-> report (update :missing sorted-vec) (update :available sorted-vec)))

(defn- config-view
  "addons.core adds its own :runtime/ports (fn values, a set that grows with
   the host) to the init config; keep the services the pipeline injected and
   only whether host ports came along."
  [cfg]
  (-> cfg
      (select-keys [:services])
      (assoc :runtime-ports? (map? (:runtime/ports cfg)))))

(defn- scenario []
  (let [seen       (atom nil)
        store      (atom nil)
        callbacks  (atom 0)
        ok-id      "f08.di-ok"
        missing-id "f08.di-missing"]
    (try
      (di/register-service! :hive/kg-store {:conn :fake})
      (di/register-service! :hive/editor :editor)
      (let [ok-ctx   (di/run-addon-pipeline!
                      (stub-addon ok-id seen #{:hive/kg-store} #{:hive/vessel-registry})
                      {:store-atom store :services #{:hive/editor}
                       :on-success (fn [_] (swap! callbacks inc))})
            init-cfg @seen
            missing  (di/run-addon-pipeline! (stub-addon missing-id (atom nil) #{:hive/absent} #{}) {})
            bad-dep  (di/run-addon-pipeline! (stub-addon "f08.di-bad" (atom nil) #{} #{})
                                             {:extra-deps {:x 'no.such.ns/fn}})]
        {:resolve       {:hit         (var-name (di/resolve-symbol 'clojure.core/identity))
                         :miss        (di/resolve-symbol 'no.such.ns/fn)
                         :all         (var-map (di/resolve-symbols {:i 'clojure.core/identity :c 'clojure.core/count}))
                         :any-missing (di/resolve-symbols {:i 'clojure.core/identity :x 'no.such.ns/fn})}
         :consumer      {:declared (sorted-vec (di/safe-required-services (->Consumer #{:b :a} nil)))
                         :nil-set  (sorted-vec (di/safe-required-services (->Consumer nil nil)))
                         :plain    (sorted-vec (di/safe-optional-services {:not :a-consumer}))}
         :deps          {:satisfied (deps-view (di/check-service-dependencies (->Consumer #{:hive/editor} #{})))
                         :missing   (deps-view (di/check-service-dependencies
                                                (->Consumer #{:hive/editor :hive/absent} #{})))}
         :require-miss  (try (di/require-service :hive/absent) :no-throw
                             (catch clojure.lang.ExceptionInfo e
                               (update (ex-data e) :available sorted-vec)))
         :pipeline-ok   (pipeline-view ok-ctx)
         :init-config   (config-view init-cfg)
         :stored?       (some? @store)
         :callbacks     @callbacks
         :missing-req   (pipeline-view missing)
         :bad-extra-dep (pipeline-view bad-dep)
         :init-as-addon {:ok  (di/init-as-addon! "f08" #(stub-addon "f08.di-helper" (atom nil) #{} #{}))
                         :nil (di/init-as-addon! "f08-nil" (constantly nil))}})
      (finally
        (doseq [id [ok-id missing-id "f08.di-bad" "f08.di-helper"]]
          (try (addons/unregister-addon! id) (catch Throwable _ nil)))))))

(deftest-golden-fn di-pipeline-golden
  "test/golden/addons/di-pipeline.edn"
  scenario)

(deftest register-service-rejects-nil
  (is (thrown? AssertionError (di/register-service! :k nil)))
  (is (nil? (di/clear-services!))))
