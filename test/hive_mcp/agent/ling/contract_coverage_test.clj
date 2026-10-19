(ns hive-mcp.agent.ling.contract-coverage-test
  "Gate: every public fn under src/hive_mcp/agent/ling carries an `m/=>`
   contract or a REASONED exemption, and no exemption is stale. Also the
   trifectas of the pure ling helpers (preflight trust, preset order, row
   filtering, ctx and Ling construction) and the registries driven with the
   recording stubs under hive-mcp.test.stub."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.ling.headless-addon-strategy :as hstrat]
            [hive-mcp.agent.ling.headless-registry :as hreg]
            [hive-mcp.agent.ling.lifecycle :as lifecycle]
            [hive-mcp.agent.ling.spawn :as spawn]
            [hive-mcp.agent.ling.spawn-store :as spawn-store]
            [hive-mcp.agent.ling.start-preflight :as pf]
            [hive-mcp.agent.ling.status :as status]
            [hive-mcp.agent.ling.strategy :as strategy]
            [hive-mcp.agent.ling.terminal-addon-strategy :as tstrat]
            [hive-mcp.agent.ling.terminal-registry :as treg]
            [hive-mcp.test.stub.headless-backend :as stub-h]
            [hive-mcp.test.stub.terminal-addon :as stub-t]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]
            [hive-mcp.swarm.delegate :as delegate]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage ling-contracts-gate
  {:paths ["src/hive_mcp/agent/ling"]
   :exempt {}})

;;; ---------------------------------------------------------------------------
;;; start-preflight
;;; ---------------------------------------------------------------------------

(def ^:private Dir [:enum "/srv/repo" "/srv/repo/" "/srv/repo/src/x" "/tmp" "/"])
(def ^:private Root [:enum "/srv/repo" "/srv" nil])

(defn- strip [p] (if (and (> (count p) 1) (str/ends-with? p "/")) (subs p 0 (dec (count p))) p))

(hst/deftrifecta-from-schema trust-candidates-contract
  hive-mcp.agent.ling.start-preflight/trust-candidates
  {:in        [:cat Dir Root]
   :out       [:vector [:string {:min 1}]]
   ;; DIR first, each next its parent, ending at ROOT when ROOT is on the
   ;; path, else at the filesystem root
   :rel       (fn [[dir root] out]
                (and (= (strip dir) (first out))
                     (every? (fn [[a b]] (str/starts-with? a b)) (partition 2 1 out))
                     (or (= (some-> root strip) (last out)) (= "/" (last out)))))
   :num-tests 200
   :golden-path "test/golden/agent/ling/trust-candidates.edn"})

(hst/deftrifecta-from-schema trusted-contract
  hive-mcp.agent.ling.start-preflight/trusted?
  {:in        [:cat [:vector {:max 3} [:enum "/a" "/a/b" "/c"]] [:set [:enum "/a" "/z"]]]
   :out       :boolean
   :rel       (fn [[cands trusted] out] (= out (boolean (some (set trusted) cands))))})

(hst/deftrifecta-from-schema refusal-message-contract
  hive-mcp.agent.ling.start-preflight/refusal-message
  {:in        [:cat [:enum "/w/a" "/w/b/c"] [:enum "/w" "/w/b"]]
   :out       [:string {:min 80}]
   :rel       (fn [[cwd root] out] (and (str/includes? out cwd) (str/includes? out root)))
   :golden-path "test/golden/agent/ling/refusal-message.edn"})

(defn- trust-source [roots checkout]
  (reify pf/IFolderTrust
    (trusted-roots [_] roots)
    (checkout-root [_ _] checkout)))

(def ^:private sources
  {:trusts-repo  (trust-source #{"/w/repo"} "/w/repo")
   :trusts-none  (trust-source #{} "/w/repo")
   :no-checkout  (trust-source #{"/w"} nil)})

(hst/deftrifecta-from-schema untrusted-refusal-contract
  hive-mcp.agent.ling.start-preflight/untrusted-refusal
  ;; a source that trusts nothing is half the domain, so the seeded cases
  ;; reach the refusal arm; a nil cwd is checked in ensure-startable-contract-test
  {:in        [:cat [:enum (:trusts-none sources) (:trusts-repo sources) (:no-checkout sources)]
               [:enum :claude :vterm :headless]
               [:enum "/w/repo" "/w/repo/sub" "/elsewhere"]]
   :out       [:maybe pf/Refusal]
   ;; only an interactive mode is ever refused, and then exactly when no
   ;; trust candidate is trusted
   :rel       (fn [[src mode cwd] out]
                (if (contains? pf/interactive-modes mode)
                  (= (some? out)
                     (not (pf/trusted? (pf/trust-candidates cwd (pf/checkout-root src cwd))
                                       (pf/trusted-roots src))))
                  (nil? out)))
   :num-tests 200
   :n-cases   12})

(deftest ensure-startable-contract-test
  (testing "an admitted spawn answers nil, a refused one throws with the refusal"
    (binding [pf/*folder-trust* (:trusts-repo sources)]
      (is (nil? (pf/ensure-startable! :claude "/w/repo/sub"))))
    (binding [pf/*folder-trust* (:trusts-none sources)]
      (let [e (try (pf/ensure-startable! :claude "/w/repo") nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (= :spawn/untrusted-folder (:reason (ex-data e))))
        (is (m/validate pf/Refusal (select-keys (ex-data e) [:cwd :root :message])))))
    (binding [pf/*folder-trust* (reify pf/IFolderTrust
                                  (trusted-roots [_] (throw (ex-info "unreadable" {})))
                                  (checkout-root [_ _] nil))]
      (is (nil? (pf/ensure-startable! :claude "/w/repo")) "an unreadable source admits")))
  (is (m/validate pf/FolderTrust (pf/claude-code-trust))))

;;; ---------------------------------------------------------------------------
;;; spawn helpers
;;; ---------------------------------------------------------------------------

(def ^:private SlaveRow
  [:map
   [:slave/presets {:optional true} [:set [:enum "b" "a" "c"]]]
   [:slave/preset-order {:optional true} [:enum "[\"b\" \"a\"]" "[\"c\"]" "not edn ((" "[1 2]"]]
   [:slave/cwd {:optional true} [:enum "/w"]]
   [:slave/project-id {:optional true} [:enum "p"]]
   [:ling/spawn-mode {:optional true} [:enum :vterm :claude]]
   [:ling/model {:optional true} [:enum "m"]]
   [:ling/turn-budget {:optional true} [:enum 10]]])

(hst/deftrifecta-from-schema ordered-presets-contract
  hive-mcp.agent.ling.spawn/ordered-presets
  {:in        SlaveRow
   :out       spawn/Presets
   ;; the snapshot order when it is a string vector naming exactly the
   ;; presets, else the presets sorted
   :rel       (fn [row out]
                (let [legacy (:slave/presets row)]
                  (and (= (set legacy) (set out))
                       (or (= out (vec (sort legacy)))
                           (= (pr-str out) (:slave/preset-order row))))))
   :num-tests 200
   :golden-path "test/golden/agent/ling/ordered-presets.edn"})

(hst/deftrifecta-from-schema slave-ling-opts-contract
  hive-mcp.agent.ling.spawn/slave->ling-opts
  {:in        SlaveRow
   :out       spawn/LingOpts
   :rel       (fn [row out]
                (and (= (get row :ling/spawn-mode :claude) (:spawn-mode out))
                     (= (:slave/cwd row) (:cwd out))
                     (= (contains? row :ling/turn-budget) (contains? out :turn-budget))))
   :num-tests 200
   :golden-path "test/golden/agent/ling/slave-ling-opts.edn"})

(def ^:private filters
  {:none   nil
   :drop   (fn [_ attrs] (dissoc attrs :cwd))
   :throws (fn [_ _] (throw (ex-info "boom" {})))
   :nonmap (fn [_ _] :not-a-map)})

(hst/deftrifecta-from-schema row-attrs-contract
  hive-mcp.agent.ling.spawn/row-attrs
  {:in        [:cat (into [:enum] (vals filters)) [:enum "ling-1"]
               [:map [:status [:enum :idle]] [:depth [:enum 1]] [:cwd [:enum "/w"]]
                [:project-id {:optional true} [:enum "p"]]]]
   :out       :map
   ;; nil is identity, a map-returning filter is applied, and a filter that
   ;; throws or answers a non-map fails closed to the safe keys
   :rel       (fn [[f _ attrs] out]
                (condp = f
                  (:none filters) (= attrs out)
                  (:drop filters) (= (dissoc attrs :cwd) out)
                  (= (select-keys attrs spawn/row-safe-keys) out)))
   :num-tests 100})

(hst/deftrifecta-from-schema ling-construction-contract
  hive-mcp.agent.ling.spawn/->ling
  {:in        [:cat [:enum "ling-a" "ling-b"]
               [:map [:spawn-mode [:enum :claude :vterm]]
                [:cwd {:optional true} [:enum "/w"]]
                [:presets {:optional true} [:vector [:enum "p1" "p2"]]]
                [:model {:optional true} [:enum "m"]]
                [:token-budget {:optional true} [:enum 1000]]]]
   :out       spawn/LingValue
   :rel       (fn [[id opts] out]
                (and (= id (:id out))
                     (= (:spawn-mode opts) (:spawn-mode out))
                     (= (get opts :presets []) (:presets out))
                     (= (contains? opts :token-budget) (some? (:token-budget out)))))
   :num-tests 100})

;;; ---------------------------------------------------------------------------
;;; lifecycle
;;; ---------------------------------------------------------------------------

(hst/deftrifecta-from-schema ling-ctx-contract
  hive-mcp.agent.ling.lifecycle/ling-ctx
  {:in        [:map [:id [:enum "l"]] [:cwd [:enum "/w"]] [:spawn-mode [:enum :claude]]
               [:provider {:optional true} [:enum :venice]]
               [:verbose? {:optional true} :boolean]
               [:sandbox {:optional true} [:enum false {:net false}]]
               [:unrelated {:optional true} [:enum 1]]]
   :out       lifecycle/LingCtx
   ;; identity keys always, the optional ones exactly when set, nothing else
   :rel       (fn [in out]
                (and (= (:id in) (:id out))
                     (= (contains? in :provider) (contains? out :provider))
                     (= (contains? in :verbose?) (contains? out :verbose?))
                     (= (contains? in :sandbox) (contains? out :sandbox))
                     (not (contains? out :unrelated))))
   :num-tests 200
   :golden-path "test/golden/agent/ling/ling-ctx.edn"})

(hst/deftrifecta-from-schema resolve-effective-mode-contract
  hive-mcp.agent.ling.lifecycle/resolve-effective-mode
  {:in        [:map [:spawn-mode [:enum :claude :vterm :opencode]]]
   :out       :keyword
   :rel       (fn [in out] (= (:spawn-mode in) out))})

(deftest lifecycle-contract-test
  (is (keyword? (lifecycle/default-spawn-mode)))
  (is (nil? (lifecycle/resolve-api-key-for-provider :no-such-provider)))
  (is (nil? (lifecycle/resolve-api-key-for-provider nil)))
  (testing "an unregistered mode throws, naming what is registered"
    (let [e (try (lifecycle/resolve-strategy :zz-unregistered) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :zz-unregistered (:mode (ex-data e))))))
  (testing "no mode resolves to nil: an unregistered nil also throws"
    (is (thrown? clojure.lang.ExceptionInfo (lifecycle/resolve-strategy nil)))))

;;; ---------------------------------------------------------------------------
;;; registries and strategies, driven with the recording stubs
;;; ---------------------------------------------------------------------------

(deftest headless-registry-contract-test
  (stub-h/without-backends
    (let [b (stub-h/->backend :zz-h)]
      (is (= {:registered? true :headless-id :zz-h}
             (hreg/register-headless! :zz-h b {:provides [:claude] :priority 7})))
      (is (false? (:registered? (hreg/register-headless! :zz-bad :not-a-backend))))
      (is (= #{:zz-h} (hreg/registered-headless)))
      (is (identical? b (hreg/get-headless-backend :zz-h)))
      (is (m/validate hreg/BackendMetadata (hreg/headless-metadata :zz-h)))
      (is (= #{:claude} (:provides (hreg/headless-metadata :zz-h))))
      (is (set? (hreg/headless-capabilities :zz-h)))
      (is (nil? (hreg/headless-capabilities :zz-absent)))
      (is (= :zz-h (hreg/best-headless-for-provider :claude)))
      (is (nil? (hreg/best-headless-for-provider :zz-nobody)))
      (is (= :zz-h (hreg/best-headless-for-provider nil)))
      (is (m/validate strategy/LingStrategy (hreg/resolve-headless-strategy :zz-h)))
      (is (nil? (hreg/resolve-headless-strategy :zz-absent)))
      (is (keyword? (hreg/resolve-default-backend nil)))
      (is (= {:deregistered? true :headless-id :zz-h} (hreg/deregister-headless! :zz-h)))
      (is (= {:deregistered? false :headless-id :zz-h} (hreg/deregister-headless! :zz-h)))
      (is (nil? (hreg/clear-registry!))))))

(deftest terminal-registry-contract-test
  (let [before (into {} (map (juxt identity treg/get-terminal-addon)) (treg/registered-terminals))]
    (try
      (let [t (stub-t/->terminal :zz-t)]
        (is (= {:registered? true :terminal-id :zz-t} (treg/register-terminal! :zz-t t)))
        (is (false? (:registered? (treg/register-terminal! :zz-bad :not-an-addon))))
        (is (contains? (treg/registered-terminals) :zz-t))
        (is (identical? t (treg/get-terminal-addon :zz-t)))
        (is (m/validate strategy/LingStrategy (treg/resolve-terminal-strategy :zz-t)))
        (is (nil? (treg/resolve-terminal-strategy :zz-absent)))
        (is (= {:deregistered? true :terminal-id :zz-t} (treg/deregister-terminal! :zz-t)))
        (is (= {:deregistered? false :terminal-id :zz-t} (treg/deregister-terminal! :zz-t)))
        (is (nil? (treg/clear-registry!))))
      (finally
        (treg/clear-registry!)
        (doseq [[id a] before] (treg/register-terminal! id a))))))

(deftest strategy-constructors-contract-test
  (is (m/validate strategy/LingStrategy (hstrat/->headless-addon-strategy (stub-h/->backend :zz))))
  (is (m/validate strategy/LingStrategy (tstrat/->terminal-addon-strategy (stub-t/->terminal :zz))))
  (is (thrown? Throwable (hstrat/->headless-addon-strategy :not-a-backend)))
  (is (thrown? Throwable (tstrat/->terminal-addon-strategy :not-an-addon))))

(deftest spawn-store-contract-test
  (is (m/validate spawn-store/SpawnStore (spawn-store/get-store)))
  (is (m/validate spawn-store/SpawnStore (spawn-store/reset-store!)))
  (is (m/validate spawn-store/SpawnStore (spawn-store/set-store! (spawn-store/get-store))))
  (testing "only a coordinator SESSION without a row gets one"
    (is (nil? (spawn-store/ensure-coordinator-session! (spawn-store/get-store) nil)))
    (is (nil? (spawn-store/ensure-coordinator-session! (spawn-store/get-store) "coordinator")))
    (is (nil? (spawn-store/ensure-coordinator-session! (spawn-store/get-store) "ling-x")))))

(def ^:private swarm-store?
  "Is the swarm DataScript store on this classpath? status and the critical-op
   guard read it, so without it they raise :swarm/addon-missing."
  (delegate/available? "hive-datascript.swarm.queries"))

(deftest status-contract-test
  (if swarm-store?
    (do
      (is (nil? (status/get-ling "zz-no-such-ling")))
      (is (nil? (status/get-ling-for-task "zz-no-such-task")))
      (is (m/validate [:sequential spawn/LingValue] (status/list-lings)))
      (is (m/validate [:sequential spawn/LingValue] (status/list-lings "zz-no-such-project")))
      (is (= {:success? false :ling-id "zz-no-such-ling" :errors ["Ling not found: zz-no-such-ling"]}
             (status/interrupt-ling! "zz-no-such-ling")))
      (is (= 42 (lifecycle/with-critical-op "zz-ling" :wrap (fn [] 42)))))
    (doseq [call [#(status/get-ling "zz") #(status/list-lings)
                  #(status/get-ling-for-task "zz") #(status/interrupt-ling! "zz")
                  #(lifecycle/with-critical-op "zz" :wrap (fn [] 42))]]
      (is (true? (:swarm/addon-missing
                  (ex-data (try (call) nil (catch clojure.lang.ExceptionInfo e e)))))))))

(deftest ling-schemas-generate-test
  (doseq [s [strategy/LingStrategy hstrat/HeadlessBackend tstrat/TerminalAddon
             hreg/RegistrationOpts hreg/BackendMetadata hreg/Registration
             lifecycle/LingCtx spawn-store/SpawnStore spawn/Presets spawn/LingOpts spawn/LingValue
             pf/FolderTrust pf/Refusal]]
    (is (seq (mg/sample s {:size 3})) (pr-str (m/form s)))))
