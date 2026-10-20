(ns hive-mcp.agent.hints-trifecta-test
  "Facets for the memory-hints wire format, derived from its contracts:
   whatever `serialize-hints` writes, `parse-hints-from-context` reads
   back (axiom and read ids survive the round trip)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [hive-mcp.agent.hints :as h]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id
  "A whitespace-free token, as a memory id is."
  [:string {:min 1 :gen/fmap (fn [s] (apply str "i" (filter #(Character/isLetterOrDigit ^char %) s)))}])

(def ^:private HintsIn
  [:map [:memory-hints [:map
                        [:axiom-ids [:vector {:max 4} Id]]
                        [:read-ids [:vector {:max 4} Id]]]]])

(deftrifecta-from-schema serialize-hints-roundtrip-trifecta
  hive-mcp.agent.hints/serialize-hints
  {:in HintsIn
   :out :string
   :rel (fn [in out]
          (let [parsed (h/parse-hints-from-context out)
                mh (:memory-hints in)]
            ;; A read id that is also an axiom id is reported once, as an
            ;; axiom: the parser drops it from :read-ids (probed).
            (and (= (:axiom-ids mh) (vec (:axiom-ids parsed [])))
                 (= (vec (remove (set (:axiom-ids mh)) (:read-ids mh)))
                    (vec (:read-ids parsed []))))))
   :num-tests 100})

(golden/deftest-golden-fn hints-golden
  "test/golden/agent/hints-wire.edn"
  (fn []
    (let [wire (h/serialize-hints {:memory-hints {:axiom-ids ["a1"] :read-ids ["r1"] :queries ["q"]}})]
      {:parsed (h/parse-hints-from-context wire)
       :none (h/parse-hints-from-context "no hints")
       :nil (h/parse-hints-from-context nil)})))

(deftest-contract-coverage hints-contract-coverage
  {:paths ["src/hive_mcp/agent/hints.clj"]})
