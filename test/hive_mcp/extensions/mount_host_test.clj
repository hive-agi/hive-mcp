(ns hive-mcp.extensions.mount-host-test
  "Isolated tests for the IMountHost adapter over the hive-mcp addon registry.
   Injected fake seams + fake IAddon ctors — no global registry, no real addon
   init, no shared-store touch."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.extensions.mount-host :as mh]
            [hive-addon.mount.port :as port]
            [hive-addon.mount.compose :as compose]
            [hive-addon.protocol :as proto]
            [hive-dsl.result :as r]
            [hive-addon.mount.boundary :as boundary]))

(defn- fake-addon [id]
  (reify proto/IAddon
    (addon-id [_] id)
    (addon-type [_] :native)
    (capabilities [_] #{:tools})
    (initialize! [_ _cfg] {:success? true :errors []})
    (shutdown! [_] nil)
    (tools [_] [])
    (excluded-tools [_] #{})
    (schema-extensions [_] {})
    (health [_] {:status :ok})
    (hooks [_] {})))

;; Public ctors — compose resolves these via requiring-resolve (init-ns/init-fn).
(defn ctor-knowledge [_cfg] (fake-addon "hive.knowledge"))
(defn ctor-carto [_cfg] (fake-addon "hive.carto"))

(defn- fake-host
  "IMountHost over an atom-backed fake registry that records call order."
  [state]
  (mh/addon-registry-host
   {:reg-fn        (fn [a]
                     (swap! state update :order conj [:register (proto/addon-id a)])
                     (swap! state assoc-in [:reg (proto/addon-id a)] a)
                     {:success? true})
    :init-fn       (fn [id _cfg]
                     (swap! state update :order conj [:init id])
                     {:success? true :errors []})
    :shutdown-fn   (fn [id]
                     (swap! state update :order conj [:shutdown id])
                     {:success? true})
    :registered-fn (fn [id] (get-in @state [:reg id]))}))

(deftest adapter-satisfies-imounthost
  (is (satisfies? port/IMountHost (mh/addon-registry-host))))

(deftest adapter-delegates-register-init-shutdown
  (let [state (atom {:order [] :reg {}})
        host  (fake-host state)
        a     (fake-addon "x.one")]
    (is (identical? host (port/register! host a)) "register! returns host")
    (is (= "x.one" (proto/addon-id (port/registered host "x.one"))))
    (is (= {:success? true :errors []} (port/init! host "x.one" {})))
    (is (nil? (port/shutdown! host "x.one")) "shutdown! returns nil")
    (is (= [[:register "x.one"] [:init "x.one"] [:shutdown "x.one"]]
           (:order @state)))))

(deftest compose-drives-adapter-in-topo-order
  (testing "carto depends on knowledge -> knowledge registers+inits first"
    (let [specs  [{:addon/id "hive.carto"
                   :addon/init-ns "hive-mcp.extensions.mount-host-test"
                   :addon/init-fn "ctor-carto"
                   :addon/dependencies #{"hive.knowledge"}}
                  {:addon/id "hive.knowledge"
                   :addon/init-ns "hive-mcp.extensions.mount-host-test"
                   :addon/init-fn "ctor-knowledge"}]
          state  (atom {:order [] :reg {}})
          host   (fake-host state)
          result (compose/compose! specs [] host {})]
      (is (r/ok? result))
      (is (= ["hive.knowledge" "hive.carto"]
             (->> (:order @state) (filter #(= :init (first %))) (mapv second)))))))

;; =============================================================================
;; Plug-out and failure surfacing
;; =============================================================================

(defn- recording-host
  "A host over a fake registry that records every unregister."
  [state & {:keys [shutdown-result] :or {shutdown-result {:success? true}}}]
  (mh/addon-registry-host
   {:reg-fn        (fn [a] (swap! state assoc-in [:reg (proto/addon-id a)] a) {:success? true})
    :init-fn       (fn [_ _] {:success? true :errors []})
    :shutdown-fn   (fn [id] (swap! state update :order conj [:shutdown id]) shutdown-result)
    :unreg-fn      (fn [id]
                     (swap! state update :order conj [:unregister id])
                     (swap! state update :reg dissoc id)
                     {:success? true})
    :registered-fn (fn [id] (get-in @state [:reg id]))}))

(deftest a-failed-shutdown-answer-is-raised-not-dropped
  (let [state (atom {:order [] :reg {}})
        host  (recording-host state :shutdown-result {:success? false :errors ["socket stuck"]})
        td    (boundary/teardown! host ["x.one"])]
    (is (= ["x.one: socket stuck"] (:errors td))
        "teardown! records the shutdown failure the registry answered as data")))

(deftest the-host-plugs-out-through-whichever-hive-addon-is-loaded
  (let [state (atom {:order [] :reg {}})
        host  (recording-host state)]
    (port/register! host (fake-addon "x.one"))
    (testing "the optional capability follows the hive-addon on the classpath"
      (is (= (some? (resolve 'hive-addon.mount.port/IMountUnregister))
             (mh/ensure-unregister!))))
    (testing "a registered id is unregistered once, through the host's own seam"
      (is (= {:unregistered ["x.one"] :unsupported [] :errors []}
             (mh/unregister! host ["x.one"])))
      (is (= [[:unregister "x.one"]] (filterv #(= :unregister (first %)) (:order @state))))
      (is (nil? (port/registered host "x.one"))))
    (testing "an unknown id is a no-op (idempotent), not an error"
      (is (= {:unregistered ["x.gone"] :unsupported [] :errors []}
             (mh/unregister! host ["x.gone"])))
      (is (= 1 (count (filter #(= :unregister (first %)) (:order @state))))))))

(deftest a-host-without-the-capability-is-reported-unsupported
  (let [bare (reify port/IMountHost
               (register! [this _] this)
               (init! [_ _ _] {:success? true})
               (shutdown! [_ _] nil)
               (registered [_ _] nil))]
    (is (= {:unregistered [] :unsupported ["x.one"] :errors []}
           (mh/unregister! bare ["x.one"])))))

(deftest current-rebuilds-a-host-left-behind-by-a-reload
  (let [state (atom {:order [] :reg {}})
        old   (recording-host state)]
    (is (identical? old (mh/current old)) "a current host is answered as is")
    (is (= :not-a-host (mh/current :not-a-host)))
    (require 'hive-mcp.extensions.mount-host :reload)
    (let [fresh ((resolve 'hive-mcp.extensions.mount-host/current) old)]
      (is (not (identical? (class old) (class fresh))) "the stale record class was replaced")
      (is (= (into {} old) (into {} fresh)) "every seam is carried over")
      (is (satisfies? port/IMountHost fresh)))))
