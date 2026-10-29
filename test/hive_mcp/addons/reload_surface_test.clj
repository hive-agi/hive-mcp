(ns hive-mcp.addons.reload-surface-test
  "A remount leaves the addon's root tools on the surface the contribution
   listeners rebuild.

   An addon's initialize! contributes commands (a listener event) BEFORE the
   host marks it :active and registers its root tools. When that was the last
   event of a remount, the surface refresh it drove re-seated the
   agent-delegation registry without the root tools: after a hive.carto
   reload a ling got \"Unknown tool: carto\" while `code carto <verb>` kept
   working (tool-friction 20261020202536-3357f82c)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.addons.core :as addons]
            [hive-mcp.addons.protocol :as proto]
            [hive-mcp.extensions.registry :as ext]))

(def ^:private root-tool-name "reload_surface_root")
(def ^:private addon-id :reload-surface-addon)

(defn- root-tool []
  {:name        root-tool-name
   :description "A root tool an addon registers."
   :inputSchema {:type "object" :properties {"command" {:type "string"}}}
   :handler     (fn [_] {:type "text" :text "ok"})})

(defrecord ContributingAddon []
  proto/IAddon
  (addon-id [_] addon-id)
  (addon-type [_] :native)
  (capabilities [_] #{:tools})
  (initialize! [_ _opts]
    ;; What hive-carto does: contribute its subdomain to `code` from inside
    ;; initialize!, which notifies the surface listeners right here.
    (ext/contribute-commands! "code" addon-id
                              {"reload-surface" {:handler (fn [_] :ok) :description "sub"}})
    {:success? true :errors []})
  (shutdown! [_] {:success? true :errors []})
  (tools [_] [(root-tool)])
  (schema-extensions [_] {}))

(def ^:private snapshots
  "Each listener event, with what a surface rebuild would have seen then."
  (atom []))

(defn- snapshot-listener [event]
  (swap! snapshots conj
         {:type   (:type event)
          :state  (:state (addons/get-addon-entry addon-id))
          :tools? (contains? (set (map :name (ext/get-registered-tools))) root-tool-name)}))

(defn- isolate [f]
  (addons/reset-registry!)
  (reset! snapshots [])
  (ext/add-contribution-listener! ::snapshot snapshot-listener)
  (try (f)
       (finally
         (ext/remove-contribution-listener! ::snapshot)
         (ext/retract-all-by-addon! addon-id)
         (ext/deregister-tool! root-tool-name)
         (addons/reset-registry!))))

(use-fixtures :each isolate)

(deftest the-last-event-of-an-init-sees-the-root-tools
  (addons/register-addon! (->ContributingAddon))
  (is (:success? (addons/init-addon! addon-id)))
  (testing "a contribution event fired from inside initialize! saw no root tool yet"
    (doseq [e (filter #(= :contribute (:type %)) @snapshots)]
      (is (false? (:tools? e)))))
  (testing "the last event sees the addon :active with its root tools"
    (let [last-event (last @snapshots)]
      (is (= :tools-registered (:type last-event)))
      (is (= :active (:state last-event)))
      (is (true? (:tools? last-event))))))

(deftest a-remount-ends-with-the-root-tools-on-the-surface
  (addons/register-addon! (->ContributingAddon))
  (addons/init-addon! addon-id)
  (addons/shutdown-addon! addon-id)
  (testing "shutdown takes the root tool off the surface"
    (is (false? (:tools? (last @snapshots)))))
  (reset! snapshots [])
  (is (:success? (addons/init-addon! addon-id)))
  (testing "the refresh a remount ends on includes the addon's root tool"
    (is (= :tools-registered (:type (last @snapshots))))
    (is (= :active (:state (last @snapshots))))
    (is (true? (:tools? (last @snapshots))))))

(deftest an-addon-without-root-tools-fires-nothing-extra
  (let [noop (reify proto/IAddon
               (addon-id [_] :reload-surface-noop)
               (addon-type [_] :native)
               (capabilities [_] #{})
               (initialize! [_ _] {:success? true :errors []})
               (shutdown! [_] {:success? true :errors []})
               (tools [_] [])
               (schema-extensions [_] {}))]
    (addons/register-addon! noop)
    (addons/init-addon! :reload-surface-noop)
    (is (empty? (filter #(= :tools-registered (:type %)) @snapshots)))))

(deftest notify-tools-registered-reaches-every-listener
  (is (= ["a" "b"] (ext/notify-tools-registered! :x ["a" "b"])))
  (is (= :tools-registered (:type (last @snapshots)))))
