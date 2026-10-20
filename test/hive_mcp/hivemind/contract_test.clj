(ns hive-mcp.hivemind.contract-test
  "Gate: every public fn under src/hive_mcp/hivemind carries an m/=> contract
   or a reasoned exemption, and every contract generates.

   conversation, event-registry and state are compat shims over hive-agent,
   and messaging's ask fns delegate there too; their behaviour is exercised by
   the swarm suite (test-swarm/hive_mcp/hivemind), where the addon is on the
   classpath. What runs here without the addon: the gate, generation, the
   shims failing loudly when hive-agent is absent, and trifectas over the
   pure messaging fns (registry->messages, cap-message, empty-shout?) and the
   event-type schema builder."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.hivemind.conversation :as conv]
            [hive-mcp.hivemind.event-registry :as er]
            [hive-mcp.hivemind.messaging :as msg]
            [hive-mcp.hivemind.state :as state]
            [hive-mcp.hivemind.status]
            [hive-mcp.hivemind.tools :as tools]
            [hive-mcp.swarm.delegate :as delegate]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage hivemind-contracts-gate
  {:paths ["src/hive_mcp/hivemind"]
   :exempt {}})

(def ^:private hivemind-nss
  '[hive-mcp.hivemind.conversation hive-mcp.hivemind.event-registry hive-mcp.hivemind.state
    hive-mcp.hivemind.messaging hive-mcp.hivemind.status hive-mcp.hivemind.tools])

(deftest every-hivemind-contract-generates-both-sides
  (doseq [ns-sym hivemind-nss
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (let [s (m/schema schema)] (if (= :function (m/type s)) (m/children s) [s]))
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

(deftest shims-fail-loudly-without-the-addon
  (if (delegate/available? "hive-agent.swarm.hivemind.state")
    (is true "hive-agent is on the classpath; the swarm suite exercises the shims")
    (doseq [call [#(conv/pending-count)
                  #(er/severity :progress)
                  #(state/get-swarm-prompts)
                  #(msg/pending-ask-entry {:agent-id "a"})]]
      (let [e (try (call) nil (catch clojure.lang.ExceptionInfo e e))]
        (is (true? (:swarm/addon-missing (ex-data e))))
        (is (= "hive-agent" (:addon (ex-data e))))))))

;; ---------------------------------------------------------------------------
;; Pure messaging

(deftrifecta-from-schema registry-messages-is-one-row-per-message
  #'msg/registry->messages
  {:in [:cat msg/RegistrySnapshot]
   :out [:sequential msg/PiggybackRow]
   :rel (fn [[snap] out]
          (= (count out) (reduce + 0 (map #(count (get-in (val %) [:data :messages])) snap))))
   :golden-path "test/golden/hive-mcp/hivemind/registry-messages.edn"})

(deftrifecta-from-schema cap-message-bounds-the-payload
  #'msg/cap-message
  {:in [:cat [:or :string :nil [:map-of :keyword :int]] [:int {:min 3 :max 40}]]
   :out [:maybe :string]
   :rel (fn [[v cap] out]
          (cond (nil? v) (nil? out)
                (= "" v) (= "" out)
                :else (and (<= (count out) cap)
                           (let [s (if (string? v) v (pr-str v))]
                             (if (<= (count s) cap) (= s out) (.endsWith ^String out "…"))))))
   :golden-path "test/golden/hive-mcp/hivemind/cap-message.edn"})

(deftrifecta-from-schema empty-shout-sees-no-signal
  #'msg/empty-shout?
  {:in [:cat [:or {:gen/elements [nil "" [] {} {:directory "/x" :project-id "p"} {:task ""} {:message "hi"}
                                  {:task "t"} {:extra 1} {:extra nil} "text" 42]} :any]]
   :out :boolean
   :rel (fn [[d] out]
          (= out (or (nil? d)
                     (and (map? d)
                          (every? #(or (nil? %) (and (string? %) (= "" %)) (and (coll? %) (empty? %)))
                                  (vals (dissoc d :directory :project-id))))
                     (and (not (map? d)) (or (= "" d) (and (coll? d) (empty? d)))))))
   :golden-path "test/golden/hive-mcp/hivemind/empty-shout.edn"})

(deftrifecta-from-schema shout-slave-status-is-nil-without-a-known-type
  #'msg/shout-slave-status
  {:in [:cat :nil :any]
   :out [:maybe :keyword]
   :rel (fn [_ out] (nil? out))
   :num-tests 20})

(deftrifecta-from-schema event-type-schema-is-a-json-string-schema
  #'er/event-type-schema
  {:in [:cat :string]
   :out [:map [:type [:= "string"]] [:description :string]]
   :rel (fn [[d] out] (= d (:description out)))
   :num-tests 30})

(deftrifecta-from-schema register-tools-hands-every-tool-over
  #'tools/register-tools!
  {:in [:cat [:enum (fn [_] nil)]]
   :out :nil
   :rel (fn [_ out] (nil? out))
   :num-tests 5})

(deftest register-tools-passes-each-tool-once
  (let [seen (atom [])]
    (is (nil? (tools/register-tools! #(swap! seen conj (:name %)))))
    (is (= (mapv :name tools/tools) @seen))))

(golden/deftest-golden empty-shout-is-not-delivered-golden
  "test/golden/hive-mcp/hivemind/empty-shout-verdict.edn"
  {:verdict (msg/shout-with-verdict! "ling-a" :progress {})
   :boolean (msg/shout! "ling-a" :progress {:directory "/x"})})

(deftest verdict-arms-validate-only-as-themselves
  (let [refused {:delivered false :reason :empty-payload}
        sent {:delivered true :shout-id "s" :routing :direct :to "ling-b"}]
    (is (m/validate msg/Verdict refused))
    (is (m/validate msg/Verdict sent))
    (is (not (m/validate msg/Verdict (assoc refused :routing :direct))))
    (is (not (m/validate msg/Verdict (dissoc sent :shout-id))))
    (testing "an empty shout never reaches the routing"
      (is (= refused (msg/shout-with-verdict! "ling-a" :progress nil))))))
