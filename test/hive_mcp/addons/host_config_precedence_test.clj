(ns hive-mcp.addons.host-config-precedence-test
  "The host's per-addon declaration (global config :addons <id>) wins over the
   manifest's :addon/config defaults on every manifest mount path: the resolver
   itself, the hive-addon mount boundary driven through the addon-registry host
   (startup mount-compose and hot inject/reload), and init-from-manifest!."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.protocol :as proto]
            [hive-mcp.addons.core :as addon-core]
            [hive-mcp.addons.manifest :as manifest]
            [hive-mcp.addons.runtime-ports :as runtime-ports]
            [hive-mcp.config.core :as config]
            [hive-mcp.extensions.mount-host :as mount-host]))

(def ^:private addon-id "test.host-config")

(def ^:private seen (atom {}))

(defrecord ConfigRecordingAddon [id]
  proto/IAddon
  (addon-id [_] id)
  (addon-type [_] :native)
  (capabilities [_] #{})
  (initialize! [_ config] (swap! seen assoc :init config) {:success? true :errors []})
  (shutdown! [_] {:success? true :errors []})
  (tools [_] [])
  (schema-extensions [_] {})
  (health [_] {:status :ok})
  (excluded-tools [_] #{})
  (hooks [_] {}))

(defn ->recording-addon
  "Constructor named by the test spec; records the config it was built with."
  [config]
  (swap! seen assoc :ctor config)
  (->ConfigRecordingAddon addon-id))

(def ^:private spec
  {:addon/id      addon-id
   :addon/type    :native
   :addon/version "0.0.1"
   :addon/init-ns "hive-mcp.addons.host-config-precedence-test"
   :addon/init-fn "->recording-addon"
   :addon/config  {:stub/port 7925 :stub/only-manifest "manifest"}})

(defn- declared [m]
  (fn [id] (if (= id addon-id) m {})))

(use-fixtures :each
  (fn [t]
    (reset! seen {})
    (addon-core/reset-registry!)
    (with-redefs [runtime-ports/runtime-ports (constantly {})]
      (try (t) (finally (addon-core/reset-registry!))))))

(deftest resolver-layers-host-declaration-over-services-over-manifest
  (with-redefs [config/get-service-config (fn [_] {:mode :local :stub/port 7935 :stub/only-service "svc"})]
    (testing "declared host config wins over config.edn :services and the manifest"
      (with-redefs [addon-core/addon-declared-config (declared {:stub/port 7945})]
        (let [cfg (manifest/prepare-config spec)]
          (is (= 7945 (:stub/port cfg)))
          (is (= "manifest" (:stub/only-manifest cfg)))
          (is (= "svc" (:stub/only-service cfg)))
          (is (not (contains? cfg :mode))))))
    (testing "without a declaration the :services value still wins over the manifest"
      (with-redefs [addon-core/addon-declared-config (declared {})]
        (is (= 7935 (:stub/port (manifest/prepare-config spec))))))))

(deftest a-pure-env-template-in-the-declaration-is-stripped-like-any-other-layer
  (with-redefs [config/get-service-config (constantly nil)
                addon-core/addon-declared-config (declared {:stub/token "${STUB_TOKEN}"})]
    (is (not (contains? (manifest/prepare-config spec) :stub/token)))))

(deftest the-mount-boundary-hands-constructor-and-init-the-host-override
  (with-redefs [config/get-service-config (constantly nil)
                addon-core/addon-declared-config (declared {:stub/port 7945})]
    (let [report (boundary/mount! {:ordered [spec]}
                                  (mount-host/addon-registry-host)
                                  {:resolve-config manifest/prepare-config})]
      (is (:ok? report))
      (is (= 7945 (get-in @seen [:ctor :stub/port])) "constructor seed")
      (is (= 7945 (get-in @seen [:init :stub/port])) "initialize! config")
      (is (= "manifest" (get-in @seen [:init :stub/only-manifest]))))))

(deftest an-in-memory-config-change-reaches-the-next-mount
  (with-redefs [config/get-service-config (constantly nil)]
    (let [config-state (atom {:addons {addon-id {:stub/port 7946}}})]
      (with-redefs [config/get-in-config (fn [path] (get-in @config-state path))]
        (let [report (boundary/mount! {:ordered [spec]}
                                      (mount-host/addon-registry-host)
                                      {:resolve-config manifest/prepare-config})]
          (is (:ok? report))
          (is (= 7946 (get-in @seen [:init :stub/port]))))))))

(deftest init-from-manifest-hands-init-the-host-override
  (with-redefs [config/get-service-config (constantly nil)
                addon-core/addon-declared-config (declared {:stub/port 7945})]
    (let [result (manifest/init-from-manifest! spec addon-core/register-addon! addon-core/init-addon!)]
      (is (:success? result))
      (is (= 7945 (get-in @seen [:init :stub/port]))))))

(deftest hot-bridge-and-startup-share-the-one-resolver
  (let [reload-opts @(requiring-resolve 'hive-mcp.tools.consolidated.hot/reload-opts)]
    (is (identical? manifest/prepare-config (get-in (reload-opts) [:mount-opts :resolve-config])))))
