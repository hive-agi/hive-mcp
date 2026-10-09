(ns hive-mcp.tools.memory.crud.role-gate-source-test
  "RoleCard validator source selection: injected > extension registry > hive-spi.
   A provider registers its validator INTO hive-mcp under
   `role-card-validator-extension-key`, so the gate stays live without the
   public source naming the private namespace that owns the contract."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.memory.crud.write :as wr]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private source-precedence [:injected :extension :spi])

(deftrifecta role-card-validator-selection
  hive-mcp.tools.memory.crud.write/select-role-card-validator
  {:golden-path "test/golden/hive-mcp/memory/role-card-validator-selection.edn"
   :cases {:none      {}
           :spi-only  {:spi :s}
           :extension {:extension :e :spi :s}
           :injected  {:injected :i :extension :e :spi :s}}
   :gen (gen/let [present (gen/vector (gen/elements source-precedence) 0 3)]
          (into {} (map (fn [k] [k k])) present))
   :pred (fn [input]
           (let [{:keys [source validator]} (wr/select-role-card-validator input)
                 expected (some #(when (get input %) %) source-precedence)]
             (and (= (or expected :none) source)
                  (= (get input expected) validator))))
   :num-tests 60
   :mutations [["spi-first" (fn [m] (let [k (some #(when (get m %) %) (rseq source-precedence))]
                                      {:source (or k :none) :validator (get m k)}))]
               ["always-none" (fn [_] {:source :none :validator nil})]]})

(deftest registered-extension-feeds-the-gate
  (testing "a validator registered under the extension key is the live validator"
    (let [k wr/role-card-validator-extension-key
          v {:valid? (constantly false) :explain (constantly {:role/_ "rejected"})}]
      (wr/set-role-card-validator! nil)
      (ext/register! k v)
      (try
        (is (= v (wr/current-role-card-validator)))
        (let [e (try (#'wr/validate-role-gate! "{:role/id :role/x :role/name \"X\"}")
                     (catch clojure.lang.ExceptionInfo ex ex))]
          (is (= :role-gate-rejected (:type (ex-data e)))))
        (finally (ext/deregister! k))))))

(deftest a-malformed-registration-is-rejected-loudly
  (testing "a bare fn registered instead of a {:valid? :explain} map rejects, never NPEs"
    (let [k wr/role-card-validator-extension-key]
      (wr/set-role-card-validator! nil)
      (ext/register! k (constantly true))
      (try
        (let [e (try (#'wr/validate-role-gate! "{:role/id :role/x :role/name \"X\"}")
                     (catch clojure.lang.ExceptionInfo ex ex))]
          (is (= :role-gate-rejected (:type (ex-data e))))
          (is (= :role-gate/malformed-validator (:explanation (ex-data e)))))
        (finally (ext/deregister! k))))))
