(ns hive-mcp.plan.strategy-addon-integration-test
  "Workflow addon's real hook data crosses the host's addon, strategy and plan
   field ports. Each case restores the registries it found, even on failure."
  (:require [clojure.test.check.generators :as gen]
            [hive-addon.protocol :as addon]
            [hive-mcp.addons.core :as addons]
            [hive-mcp.plan.field-registry :as fields]
            [hive-mcp.plan.gate :as gate]
            [hive-mcp.workflows.strategy-registry :as strategies]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-workflows.strategy-addon :as workflow]))

(def ^:private owner "hive.workflows.strategy")
(def ^:private methods #{:dag-wave :saa :forge-belt})
(def ^:private witness-owner "strategy-integration-witness")

(defn- fixture-addon []
  (reify addon/IAddon
    (addon-id [_] owner)
    (addon-type [_] :native)
    (capabilities [_] #{:workflow-method})
    (initialize! [_ _] {:success? true :metadata (workflow/install!)})
    (shutdown! [_] (workflow/stop!) {:success? true})
    (tools [_] [])
    (schema-extensions [_] [])
    (excluded-tools [_] #{})
    (hooks [_] (workflow/hooks))
    (health [_] {:status :ok})))

(defn- with-isolated-registries [f]
  (let [previous-methods (get-in (strategies/snapshot) [:data :by-id])
        previous-fields (concat (vals (fields/step-fields))
                                (vals (fields/plan-fields)))]
    (try
      (workflow/stop!)
      (strategies/reset-for-test!)
      (fields/reset-registry!)
      (f)
      (finally
        (when (addons/addon-registered? owner)
          (addons/unregister-addon! owner))
        (workflow/stop!)
        (strategies/reset-for-test!)
        (fields/reset-registry!)
        (doseq [[method {:keys [owner strategy]}] previous-methods]
          (strategies/register! owner method {:strategy strategy}))
        (doseq [spec previous-fields]
          (fields/register-field! spec))))))

(defn exercise-strategy-addon
  "Drive install via a fixture IAddon, then the real gate and owner-scoped stop."
  [method]
  (with-isolated-registries
    (fn []
      (let [stub (fixture-addon)
            _ (fields/register-by-key! witness-owner :plan/witness
                                       {:scope :plan :key :witness})
            registered? (:success? (addons/register-addon! stub))
            initialized? (:success? (addons/init-addon! owner {}))
            installed (set (strategies/all-methods))
            owned? (every? #(= owner (:owner (strategies/lookup %))) methods)
            step-spec (get (fields/step-fields) :method)
            plan-spec (get (fields/plan-fields) :default-method)
            content (pr-str {:id "strategy-fixture" :title "Strategy fixture"
                             :default-method "saa"
                             :steps [{:id "one" :title "First"}
                                     {:id "two" :title "Second" :method method}]})
            result (gate/validate-for-storage content)
            normalized (:plan result)
            shutdown? (:success? (addons/shutdown-addon! owner))
            removed? (and (every? #(nil? (strategies/lookup %)) methods)
                          (nil? (get (fields/step-fields) :method))
                          (nil? (get (fields/plan-fields) :default-method)))
            witness? (= witness-owner (:owner (get (fields/plan-fields) :witness)))]
        {:requested method
         :registered? registered?
         :initialized? initialized?
         :methods (select-keys (zipmap installed (repeat true)) methods)
         :owned? owned?
         :step-owner (:owner step-spec)
         :plan-owner (:owner plan-spec)
         :valid? (:valid? result)
         :phase (:phase result)
         :unknown? (boolean (some #(clojure.string/includes? % ":plan/unknown-method")
                                  (:errors result)))
         :inherited (get-in normalized [:steps 0 :method])
         :explicit (get-in normalized [:steps 1 :method])
         :default (:default-method normalized)
         :shutdown? shutdown?
         :removed? removed?
         :witness? witness?}))))

(defn- expected [method]
  {:requested method
   :registered? true :initialized? true
   :methods {:dag-wave true :saa true :forge-belt true}
   :owned? true :step-owner owner :plan-owner owner
   :valid? (contains? methods method)
   :phase (when-not (contains? methods method) :fields)
   :unknown? (not (contains? methods method))
   :inherited (when (contains? methods method) :saa)
   :explicit (when (contains? methods method) method)
   :default (when (contains? methods method) :saa)
   :shutdown? true :removed? true :witness? true})

(deftrifecta strategy-addon-host-contract
  hive-mcp.plan.strategy-addon-integration-test/exercise-strategy-addon
  {:golden-path "test/golden/hive-mcp/plan/strategy-addon.edn"
   :cases {:dag-wave :dag-wave :saa :saa :forge-belt :forge-belt
           :unknown :not-installed}
   :gen (gen/elements [:dag-wave :saa :forge-belt :not-installed])
   :pred (fn [result] (= (expected (:requested result)) result))
   :num-tests 12
   :mutations [["reject-all" (fn [method] (assoc (expected method) :valid? false))]
               ["accept-all" (fn [method] (assoc (expected method) :valid? true))]]})
