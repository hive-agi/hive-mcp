(ns hive-mcp.agora.signal-trifecta-test
  "Trifectas for the `hive-mcp.agora.signal` compat shim, driven by its own
   contracts. The shim delegates to hive-agent, so these run in the swarm
   suite (test-swarm/), where hive-agent is on the classpath."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agora.signal :as signal]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private SignalMapIn
  "Structured input that reaches every arm: a known type, an unknown one, a
   missing type, a missing message, both key spellings and both strength keys."
  [:map
   [:type {:optional true} [:enum :propose :approve :lgtm "counter" "bogus"]]
   [:signal {:optional true} [:enum :defer "no-change"]]
   [:strength {:optional true} [:double {:min -2.0 :max 3.0}]]
   [:confidence {:optional true} [:double {:min 0.0 :max 1.0}]]
   [:message {:optional true} [:enum "a" "because"]]])

(hst/deftrifecta-from-schema signal-from-map-contract
  hive-mcp.agora.signal/signal-from-map
  {:in        SignalMapIn
   :out       signal/SignalOrError
   ;; a signal is built exactly when a known type and a message are present,
   ;; and its strength is the given one clamped into [0, 1]
   :rel       (fn [in out]
                (let [t (some-> (or (:type in) (:signal in)) keyword)
                      ok? (and t (contains? signal/all-valid-signals t) (:message in))]
                  (if ok?
                    (and (= t (:type out))
                         (= (:message in) (:message out))
                         (= (-> (or (:strength in) (:confidence in) 1.0) double (max 0.0) (min 1.0))
                            (:strength out)))
                    (string? (:error out)))))
   :num-tests 200
   :golden-path "test/golden/agora/signal-from-map.edn"})

(def ^:private LegacyIn
  "Legacy text: a known prefix in any case, an unknown prefix, or none."
  [:enum "[SIGNAL: propose] change" "[signal: APPROVE]   ok " "[SIGNAL: no-change]"
   "[SIGNAL: lgtm] fine" "[SIGNAL: shrug] hmm" "plain text" ""])

(hst/deftrifecta-from-schema signal-from-legacy-contract
  hive-mcp.agora.signal/signal-from-legacy
  {:in        LegacyIn
   :out       [:tuple signal/Signal :string [:enum :prefix :default]]
   ;; a recognised prefix yields its own type and the trimmed rest; anything
   ;; else is a :propose that keeps the text as it came
   :rel       (fn [in [sig cleaned method]]
                (if (= :prefix method)
                  (and (= cleaned (:message sig))
                       (contains? signal/all-valid-signals (:type sig)))
                  (and (= :propose (:type sig)) (= in cleaned))))
   :n-cases   7
   :golden-path "test/golden/agora/signal-from-legacy.edn"})

(hst/deftrifecta-from-schema equilibrium-contribution-contract
  hive-mcp.agora.signal/equilibrium-contribution
  {:in        [:or [:enum :approve :no-change :lgtm :propose :counter :defer :zzz nil]
                   [:map [:type [:enum :approve :counter :defer]]]]
   :out       signal/Contribution
   :rel       (fn [in out]
                (let [t (if (map? in) (:type in) in)]
                  (= out (cond (#{:approve :no-change :lgtm} t) :positive
                               (#{:defer} t) :neutral
                               :else :negative))))
   :n-cases   12
   :golden-path "test/golden/agora/equilibrium-contribution.edn"})

(hst/deftrifecta-from-schema format-signal-prefix-contract
  hive-mcp.agora.signal/format-signal-prefix
  {:in        [:cat [:enum :approve :counter "defer"] [:enum "LGTM" "" nil]]
   :out       [:string {:min 11}]
   :rel       (fn [[t msg] out] (= out (str "[SIGNAL: " (name t) "] " msg)))
   :golden-path "test/golden/agora/format-signal-prefix.edn"})

(hst/deftrifecta-from-schema parse-signal-contract
  hive-mcp.agora.signal/parse-signal
  {:in        [:or LegacyIn
                   [:enum :approve :defer :lgtm :nope 42 nil]
                   [:map [:type [:enum :propose "approve" "bogus"]] [:message [:enum "m"]]]]
   :out       signal/SignalOrError
   :rel       (fn [in out]
                (cond
                  (string? in)  (contains? signal/all-valid-signals (:type out))
                  (keyword? in) (if (contains? signal/all-valid-signals in)
                                  (= in (:type out))
                                  (string? (:error out)))
                  (map? in)     (if (contains? signal/all-valid-signals (keyword (:type in)))
                                  (= (keyword (:type in)) (:type out))
                                  (string? (:error out)))
                  :else         (string? (:error out))))
   :n-cases   12
   :golden-path "test/golden/agora/parse-signal.edn"})

(hst/deftrifecta-predicate valid-signal-predicate
  hive-mcp.agora.signal/valid-signal?
  ;; the predicate's own domain: it reads type, message and strength only
  {:schema [:map [:type signal/SignalType] [:strength number?] [:message :string]]})

(hst/deftrifecta-from-schema signal-type-contract
  hive-mcp.agora.signal/signal-type
  {:in        [:maybe [:map [:type {:optional true} [:enum :approve :counter]]]]
   :out       [:maybe [:enum :approve :counter]]
   :rel       (fn [in out] (= out (:type in)))})

(deftest parse-signal-default-message-arity-test
  (is (= "custom" (:message (signal/parse-signal :approve "custom"))))
  (is (= "Signal: approve" (:message (signal/parse-signal :approve)))))

(deftest json-schema-test
  (let [js  (signal/signal->json-schema)
        out (-> (m/function-schemas)
                (get-in ['hive-mcp.agora.signal 'signal->json-schema :schema])
                m/children second)]
    (is (m/validate out js))
    (is (= #{"signal" "message" "confidence"} (set (:required js))))
    (is (= (set (map name signal/signal-types)) (set (get-in js [:properties :signal :enum]))))))

(deftest signal-union-discriminates-test
  (testing "a built signal is not a refusal, and a refusal is not a signal"
    (let [ok  (signal/signal-from-map {:type :approve :message "m"})
          bad (signal/signal-from-map {:message "m"})]
      (is (m/validate signal/Signal ok))
      (is (not (m/validate signal/SignalError ok)))
      (is (m/validate signal/SignalError bad))
      (is (not (m/validate signal/Signal bad))))))

(deftest signal-schemas-generate-test
  (doseq [s [signal/SignalType signal/Signal signal/SignalError signal/SignalOrError
             signal/SignalInput signal/Contribution signal/Classifiable]]
    (is (seq (mg/sample s {:size 4})) (pr-str s))))
