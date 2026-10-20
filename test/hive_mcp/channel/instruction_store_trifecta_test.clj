(ns hive-mcp.channel.instruction-store-trifecta-test
  "Schema trifectas for the instruction-store envelope and queue helpers, and
   the local and backbone-less NATS stores driven through the port."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.channel.instruction-store :as is]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private AgentId [:enum "ling-1" "ling-2" "coord" nil])

(def ^:private Instruction
  [:map
   [:type [:enum :task :halt]]
   [:instr/id {:optional true} [:enum "fixed-1" "fixed-2"]]])

(deftrifecta-from-schema subject-round-trip
  hive-mcp.channel.instruction-store/instruction-subject
  {:in AgentId
   :out is/Subject
   :rel (fn [agent out] (= (or agent "unknown") (is/agent-id-from-subject out)))
   :golden-path "test/golden/channel/instruction-subject.edn"})

(deftrifecta-from-schema subject-parsing
  hive-mcp.channel.instruction-store/agent-id-from-subject
  {:in [:or [:enum "hive.v1.instruction.ling-1" "hive.v1.instruction." "hive.v1.other.x" "" nil]
        [:string {:gen/max 10}]]
   :out [:maybe :string]
   :rel (fn [s out]
          (if (and (string? s) (.startsWith ^String s "hive.v1.instruction."))
            (= out (subs s (count "hive.v1.instruction.")))
            (nil? out)))
   :golden-path "test/golden/channel/instruction-agent-from-subject.edn"})

(deftrifecta-from-schema envelope-wrapping
  hive-mcp.channel.instruction-store/wrap-envelope
  {:in [:cat AgentId [:maybe Instruction]]
   :out is/Envelope
   :rel (fn [[agent instr] out]
          (and (= agent (:instr/agent out))
               (= (dissoc instr :instr/id) (:instr/payload out))
               (if (:instr/id instr) (= (:instr/id instr) (:instr/id out)) true)
               (= (dissoc instr :instr/id) (is/unwrap-envelope out))))
   ;; ids are random UUIDs unless the instruction carries one; the golden
   ;; pins only inputs that do.
   :mutation true})

(deftrifecta-from-schema queue-append
  hive-mcp.channel.instruction-store/append-envelope
  {:in [:cat [:maybe [:map-of [:enum "a" "b"] [:vector {:gen/max 2} :int]]] [:enum "a" "b" "c"] :int]
   :out is/Queues
   :rel (fn [[queues agent env] out]
          (and (= (conj (get queues agent []) env) (get out agent))
               (= (dissoc (or queues {}) agent) (dissoc out agent))))
   :golden-path "test/golden/channel/instruction-append.edn"})

(deftrifecta-from-schema queue-take
  hive-mcp.channel.instruction-store/take-queue
  {:in [:cat [:maybe [:map-of [:enum "a" "b"] [:vector {:gen/max 2} :int]]] [:enum "a" "b" "c"]]
   :out [:tuple [:vector :any] [:maybe is/Queues]]
   :rel (fn [[queues agent] [taken rest]]
          (and (= taken (get queues agent []))
               (not (contains? rest agent))
               (= (dissoc queues agent) rest)))
   :golden-path "test/golden/channel/instruction-take.edn"})

(deftest unwrap-and-id-arms
  (testing "envelope arm"
    (let [env (is/wrap-envelope "a" {:instr/id "fixed-1" :type :task})]
      (is (= "fixed-1" (is/envelope-id env)))
      (is (= {:type :task} (is/unwrap-envelope env)))))
  (testing "legacy arm passes through"
    (is (nil? (is/envelope-id "raw")))
    (is (= {:type :task} (is/unwrap-envelope {:type :task})))))

(deftest new-ids-are-uuids
  (is (m/validate [:re #"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"]
                  (is/new-instruction-id)))
  (is (not= (is/new-instruction-id) (is/new-instruction-id))))

(deftest stores-honour-the-port
  (doseq [[label store] [[:local (is/local-store (atom {}))]
                         [:nats-without-backbone (is/nats-backed-store (atom {}) nil)]]]
    (testing (name label)
      (is (m/validate is/InstructionStore store))
      (is (string? (is/push! store "a" {:type :task :n 1})))
      (is/push! store "a" {:type :task :n 2})
      (is (= [1 2] (mapv :n (is/peek* store "a"))))
      (is (= [1 2] (mapv :n (is/drain! store "a"))))
      (is (= [] (is/drain! store "a")))
      (is (nil? (is/start! store)))
      (is/clear! store))))

(deftest store-schemas-generate
  (doseq [s [is/QueuesAtom is/InstructionStore is/Envelope is/Queues]]
    (is (seq (mg/sample s {:size 3})))))
