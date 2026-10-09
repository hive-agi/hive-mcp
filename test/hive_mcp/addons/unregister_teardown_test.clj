(ns hive-mcp.addons.unregister-teardown-test
  "unregister-addon! performs the full owner-safe teardown.

   The subject is driven through a unary scenario adapter: a scenario names
   the state the addon is in when it is unregistered and how many times the
   unregister is repeated; the answer is what is OBSERVABLE afterwards in the
   registries the addon contributed to. A teardown that only called the
   addon's own shutdown! (the prior behaviour) leaves its tools and schema
   extensions advertised, and the golden pins that they are gone.

   Mutants are self-contained and never call the subject.
   Kanban 20260712173944-76190acb."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.addons.protocol :as proto]
            [hive-mcp.addons.core :as addons]
            [hive-mcp.extensions.registry :as ext]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private addon-id :unreg-teardown)
(def ^:private tool-name "unreg_teardown_probe")
(def ^:private schema-tool "unreg_teardown_host_tool")

(defrecord TeardownAddon [id ok? shutdowns]
  proto/IAddon
  (addon-id [_] id)
  (addon-type [_] :native)
  (capabilities [_] #{:tools})
  (initialize! [_ _opts]
    (if ok?
      {:success? true :errors []}
      {:success? false :errors ["init refused"]}))
  (shutdown! [_]
    (swap! shutdowns inc)
    {:success? true :errors []})
  (tools [_]
    [{:name tool-name
      :description "probe tool owned by the teardown addon"
      :inputSchema {:type "object"
                    :properties {"q" {:type "string"}}}
      :handler (fn [_] {:type "text" :text "ok"})}])
  (schema-extensions [_]
    {schema-tool {"probe_param" {:type "string" :description "probe"}}})
  (health [_] {:status :ok}))

(defn- observe []
  {:tool-advertised?   (boolean (some #(= tool-name (:name %))
                                      (ext/get-registered-tools)))
   :schema-advertised? (contains? (or (ext/get-schema-extensions schema-tool) {})
                                  "probe_param")
   :registered?        (addons/addon-registered? addon-id)})

(defn run-unregister
  "Unary adapter: {:state <:active|:registered|:error|:absent> :repeat n}
   -> {:results [..] :after {..}}. Isolates the registries around the run."
  [{:keys [state repeat]}]
  (addons/reset-registry!)
  (ext/clear-all-tools!)
  (ext/clear-all-schemas!)
  (try
    (let [shutdowns (atom 0)]
      (when-not (= state :absent)
        (addons/register-addon! (->TeardownAddon addon-id (not= state :error) shutdowns))
        (case state
          :active (addons/init-addon! addon-id)
          :error  (do (addons/init-addon! addon-id)
                      ;; a partial surface left behind by a failed init
                      (ext/register-schema! addon-id schema-tool
                                            {"probe_param" {:type "string"}}))
          nil))
      (let [results (vec (for [_ (range (max 1 (or repeat 1)))]
                           (select-keys (addons/unregister-addon! addon-id)
                                        [:success? :shutdown-errors])))]
        {:results   results
         :shutdowns @shutdowns
         :after     (observe)}))
    (finally
      (addons/reset-registry!)
      (ext/clear-all-tools!)
      (ext/clear-all-schemas!))))

(def ^:private clean {:tool-advertised? false :schema-advertised? false :registered? false})

(deftrifecta unregister-teardown-contract
  hive-mcp.addons.unregister-teardown-test/run-unregister
  {:golden-path "test/golden/addons/unregister-teardown.edn"
   :cases       {:active          {:state :active :repeat 1}
                 :active-twice    {:state :active :repeat 2}
                 :registered-only {:state :registered :repeat 1}
                 :error-partial   {:state :error :repeat 1}
                 :absent          {:state :absent :repeat 1}}
   :gen         (gen/hash-map :state (gen/elements [:active :registered :error :absent])
                              :repeat (gen/choose 1 3))
   :pred        (fn [{:keys [after]}] (= clean after))
   :num-tests   30
   :mutations   [["shutdown-only — the prior behaviour, surface stays advertised"
                  (fn [{:keys [state]}]
                    {:results [{:success? true}] :shutdowns (if (= state :active) 1 0)
                     :after (assoc clean :tool-advertised? (= state :active)
                                   :schema-advertised? (contains? #{:active :error} state))})]
                 ["never-removes-entry"
                  (fn [_] {:results [{:success? true}] :shutdowns 0
                           :after (assoc clean :registered? true)})]]
   :assert      (fn []
                  (let [{:keys [results shutdowns after]} (run-unregister {:state :active :repeat 2})]
                    (is (= clean after) "nothing the addon contributed is left advertised")
                    (is (= 1 shutdowns) "the addon's own shutdown! runs exactly once")
                    (is (= [true false] (mapv :success? results))
                        "the second unregister reports not-registered")))})

(deftest unregister-active-retracts-surface
  (testing "an active addon's tools and schema extensions are withdrawn"
    (is (= clean (:after (run-unregister {:state :active :repeat 1}))))))

(deftest unregister-error-retracts-partial-surface
  (testing "an addon in :error has its owner-keyed contributions retracted"
    (is (= clean (:after (run-unregister {:state :error :repeat 1}))))))

(deftest unregister-throwing-shutdown-retracts-surface
  (testing "an active addon whose shutdown! throws still has its surface withdrawn"
    (addons/reset-registry!)
    (ext/clear-all-tools!)
    (ext/clear-all-schemas!)
    (try
      (let [base (->TeardownAddon addon-id true (atom 0))
            addon (reify proto/IAddon
                    (addon-id [_] addon-id)
                    (addon-type [_] :native)
                    (capabilities [_] #{:tools})
                    (initialize! [_ opts] (proto/initialize! base opts))
                    (shutdown! [_] (throw (ex-info "shutdown boom" {})))
                    (tools [_] (proto/tools base))
                    (schema-extensions [_] (proto/schema-extensions base))
                    (health [_] {:status :ok}))]
        (addons/register-addon! addon)
        (addons/init-addon! addon-id)
        (let [result (addons/unregister-addon! addon-id)]
          (is (:success? result) "the entry is removed regardless")
          (is (seq (:shutdown-errors result)) "the shutdown failure is reported")
          (is (= clean (observe)) "nothing the addon contributed is left advertised")))
      (finally
        (addons/reset-registry!)
        (ext/clear-all-tools!)
        (ext/clear-all-schemas!)))))
