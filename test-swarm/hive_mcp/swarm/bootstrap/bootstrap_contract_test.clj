(ns hive-mcp.swarm.bootstrap.bootstrap-contract-test
  "Contract facets for the swarm bootstrap tree: the factory's source
   resolution, the NoopBootstrap and DatahikeBootstrap shims, and the one
   datahike-driver fn with a stable shape. The Datahike store lives in a temp
   directory owned and released by the test; the config port is an injected
   fn. Also the coverage gate for src/hive_mcp/swarm/bootstrap."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.swarm.addon-gate :as gate]
            [hive-mcp.swarm.bootstrap.datahike :as dh]
            [hive-mcp.swarm.bootstrap.datahike-driver :as driver]
            [hive-mcp.swarm.bootstrap.factory :as factory]
            [hive-mcp.swarm.bootstrap.noop :as noop]
            [hive-mcp.swarm.bootstrap.protocol :as bp]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-test.golden :as golden]
            [hive-test.mutation :as mut]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- kind
  "Which implementation a bootstrap is, by its class name."
  [bs]
  (let [n (.getName (class bs))]
    (cond (.endsWith n "NoopBootstrap") :noop
          (.endsWith n "DatahikeBootstrap") :datahike
          :else :emacs)))

(defn- temp-path []
  (str (System/getProperty "java.io.tmpdir") "/m21-bootstrap-" (random-uuid)))

(defn- routing
  "Source resolution: explicit arg, then config, then the :emacs default; an
   unknown or throwing source never escapes."
  []
  (let [path (temp-path)
        built {:explicit-none     (factory/make-bootstrap {:source :none} (constantly "datahike"))
               :string-source     (factory/make-bootstrap {:source "none"} (constantly nil))
               :config-none       (factory/make-bootstrap {} (fn [& _] "none"))
               :config-throws     (factory/make-bootstrap {} (fn [& _] (throw (ex-info "cfg" {}))))
               :default-emacs     (factory/make-bootstrap {} (constantly nil))
               :unknown-source    (factory/make-bootstrap {:source :bogus} (constantly nil))
               :datahike          (factory/make-bootstrap {:source :datahike :db-path path}
                                                          (constantly nil))}]
    (into (sorted-map) (map (fn [[k v]] [k (kind v)])) built)))

(golden/deftest-golden factory-routing-golden
  "test/golden/hive-mcp/swarm/bootstrap/factory-routing.edn"
  (routing))

(mut/deftest-mutations make-bootstrap-mutants-caught
  hive-mcp.swarm.bootstrap.factory/make-bootstrap
  [["always-noop" (fn [_ _] (noop/make-noop-bootstrap))]
   ["ignores-explicit" (fn [_ config-fn]
                         (if (= "none" (try (config-fn :swarm-sync :source) (catch Exception _ nil)))
                           (noop/make-noop-bootstrap)
                           (dh/make-datahike-bootstrap {:db-path (temp-path)})))]]
  (fn []
    (let [r (routing)]
      (is (= :noop (:explicit-none r)))
      (is (= :emacs (:default-emacs r)))
      (is (= :datahike (:datahike r))))))

(deftest every-built-bootstrap-satisfies-the-port-test
  (doseq [bs [(noop/make-noop-bootstrap)
              (noop/->NoopBootstrap)
              (noop/map->NoopBootstrap {:extra 1})
              (dh/make-datahike-bootstrap {:db-path (temp-path)})
              (factory/make-bootstrap {} (constantly nil))]]
    (is (m/validate bp/Bootstrap bs))))

(deftest noop-bootstrap-keeps-nothing-test
  (let [bs (noop/make-noop-bootstrap)]
    (is (= [] (factory/load-slaves bs)))
    (is (identical? bs (factory/snapshot-slave! bs "s1" {:name "s1"})))
    (is (identical? bs (factory/forget-slave! bs "s1")))
    (is (nil? (factory/close! bs)))
    (is (= [] (factory/load-slaves bs)) "a snapshot is discarded")))

(deftest datahike-bootstrap-round-trip-test
  (let [path (temp-path)
        bs   (dh/make-datahike-bootstrap {:db-path path})
        cfg  (:cfg bs)]
    (try
      (is (m/validate dh/Config cfg))
      (is (false? (driver/database-exists? cfg)) "construction is lazy")
      (factory/snapshot-slave! bs "s1" {:name "one" :status :idle :depth 1 :cwd "/w"})
      (factory/snapshot-slave! bs "s2" {:name "two"})
      (factory/forget-slave! bs "s2")
      (let [rows (factory/load-slaves bs)]
        (is (every? #(m/validate bp/SlaveRow %) rows))
        (is (= [{:slave-id "s1" :name "one" :status :idle :depth 1 :cwd "/w"}] rows)))
      (is (true? (driver/database-exists? cfg)))
      (finally
        (factory/close! bs)
        (doseq [f (reverse (file-seq (io/file path)))] (io/delete-file f true))))))

(deftest datahike-record-constructors-test
  (let [cfg (:cfg (dh/make-datahike-bootstrap {:db-path (temp-path)}))]
    (is (m/validate bp/Bootstrap (dh/->DatahikeBootstrap (atom nil) cfg)))
    (is (m/validate bp/Bootstrap (dh/map->DatahikeBootstrap {:conn-atom (atom nil) :cfg cfg})))
    (is (= "data/swarm/datahike" (get-in (dh/make-datahike-bootstrap) [:cfg :store :path])))))

;; NoopBootstrap and DatahikeBootstrap are hive-agent records behind compat
;; shims: without the addon only the shims' addon-missing behaviour is
;; observable, so the tests that build them run only with the addon (see
;; test-ns-hook). The port itself is exercised offline through this stub.
(defrecord StubBootstrap [rows]
  bp/ISwarmBootstrap
  (-load-slaves [_] (vec (vals @rows)))
  (-snapshot-slave! [this id data] (swap! rows assoc id (assoc data :slave-id id)) this)
  (-forget-slave! [this id] (swap! rows dissoc id) this)
  (-close! [_] nil))

(deftest factory-re-exports-drive-any-port-impl-test
  (testing "load/snapshot/forget/close go through the port, whatever implements it"
    (let [bs (->StubBootstrap (atom (sorted-map)))]
      (is (m/validate bp/Bootstrap bs))
      (is (identical? bs (factory/snapshot-slave! bs "s1" {:name "one"})))
      (is (identical? bs (factory/snapshot-slave! bs "s2" {:name "two"})))
      (is (identical? bs (factory/forget-slave! bs "s2")))
      (is (= [{:slave-id "s1" :name "one"}] (factory/load-slaves bs)))
      (is (every? #(m/validate bp/SlaveRow %) (factory/load-slaves bs)))
      (is (nil? (factory/close! bs))))))

(deftest factory-without-the-addon-test
  (testing "the :emacs source is built in hive-mcp and needs no addon"
    (is (= :emacs (kind (factory/make-bootstrap {} (constantly nil)))))
    (is (m/validate bp/Bootstrap (factory/make-bootstrap {:source :emacs} (constantly nil)))))
  (when-not (gate/present?)
    (testing "every source the addon builds raises :swarm/addon-missing"
      (is (gate/addon-missing? #(factory/make-bootstrap {:source :none} (constantly nil))))
      (is (gate/addon-missing? #(factory/make-bootstrap {:source :bogus} (constantly nil))))
      (is (gate/addon-missing? #(factory/make-bootstrap {:source :datahike :db-path (temp-path)}
                                                        (constantly nil))))
      (is (gate/addon-missing? #(noop/make-noop-bootstrap)))
      (is (gate/addon-missing? #(dh/make-datahike-bootstrap {:db-path (temp-path)}))))))

(deftest bootstrap-schemas-generate-test
  (doseq [s [bp/Bootstrap bp/SlaveRow dh/ConnAtom dh/Config factory/ConfigFn]]
    (is (seq (mg/sample s {:size 3})) (pr-str (m/form s)))))

(deftest every-bootstrap-contract-generates-both-sides-test
  (doseq [ns-sym '[hive-mcp.swarm.bootstrap.noop hive-mcp.swarm.bootstrap.datahike
                   hive-mcp.swarm.bootstrap.datahike-driver hive-mcp.swarm.bootstrap.factory]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          :let [s (m/schema schema)]
          arity (if (= :function (m/type s)) (m/children s) [s])
          side (take 2 (m/children arity))]
    (is (seq (mg/sample side {:size 2 :seed 7})) (str ns-sym "/" fn-sym))))

(def ^:private offline?
  "The tests that never build a hive-agent bootstrap."
  (gate/offline #{"factory-re-exports-drive-any-port-impl-test" "factory-without-the-addon-test"
                  "bootstrap-schemas-generate-test"
                  "every-bootstrap-contract-generates-both-sides-test"
                  "swarm-bootstrap-contracts-gate"}
                []))

(defn test-ns-hook [] (gate/run 'hive-mcp.swarm.bootstrap.bootstrap-contract-test offline?))

(deftest-contract-coverage swarm-bootstrap-contracts-gate
  {:paths ["src/hive_mcp/swarm/bootstrap"]
   :exempt
   {'hive-mcp.swarm.bootstrap.datahike-driver/create-database
    "Variadic passthrough to datahike.api/create-database through the hive-agent shim; its argument and return are datahike's own (config map, store-dependent result), owned by datahike, not by this facade."
    'hive-mcp.swarm.bootstrap.datahike-driver/connect
    "Variadic passthrough to datahike.api/connect; returns a datahike Connection object with no stable data shape."
    'hive-mcp.swarm.bootstrap.datahike-driver/transact
    "Variadic passthrough to datahike.api/transact (conn plus tx-data or an arg map); the tx-report and the tx-data grammar are datahike's."
    'hive-mcp.swarm.bootstrap.datahike-driver/q
    "Variadic passthrough to datahike.api/q: a Datalog query plus any inputs, answering whatever the :find spec projects. The domain is genuinely open."
    'hive-mcp.swarm.bootstrap.datahike-driver/pull
    "Variadic passthrough to datahike.api/pull; the result shape is the caller's pull pattern."
    'hive-mcp.swarm.bootstrap.datahike-driver/db
    "Variadic passthrough to datahike.api/db; returns a datahike DB value (a Java object with no stable data shape)."
    'hive-mcp.swarm.bootstrap.datahike-driver/release
    "Variadic passthrough to datahike.api/release over a Connection object; returns datahike's own release result."}})
