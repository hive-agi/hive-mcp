(ns hive-mcp.agora.schema-trifecta-test
  "Trifectas for the `hive-mcp.agora.schema` compat shim (DataScript CRUD of
   dialogues and turns), driven by its contracts. Runs in the swarm suite: the
   shim delegates to hive-agent."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agora.schema :as schema]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private turns
  "The turns of the fixed dialogue \"tri-d\", in order."
  [["a-1" :propose] ["b-2" :approve] ["a-1" :defer] ["b-2" :counter] ["a-1" :approve]])

(defn- build! []
  (schema/reset-conn!)
  (schema/create-dialogue! {:id "tri-d" :name "tri" :participants ["a-1" "b-2"]
                            :config {:threshold 0.5}})
  (schema/create-dialogue! {:id "tri-empty" :name "empty" :participants ["a-1"]})
  (doseq [[from sig] turns]
    (schema/add-turn! "tri-d" {:sender from :receiver "all" :message (name sig) :signal sig})))

(use-fixtures :once (fn [f] (build!) (f)))

(def ^:private DialogueIdIn [:enum "tri-d" "tri-empty" "tri-missing"])

(hst/deftrifecta-from-schema config-round-trip-contract
  hive-mcp.agora.schema/config->edn
  {:in        [:maybe [:map [:threshold {:optional true} [:double {:min 0.0 :max 1.0}]]
                            [:timeout-ms {:optional true} [:int {:min 0}]]]]
   :out       :string
   ;; what is written reads back as the same config
   :rel       (fn [in out] (= in (schema/edn->config out)))
   :golden-path "test/golden/agora/config-edn.edn"})

(hst/deftrifecta-from-schema edn-config-contract
  hive-mcp.agora.schema/edn->config
  {:in        [:maybe [:enum "{:threshold 0.8}" "{}" "{:timeout-ms 5, :threshold 1.0}"]]
   :out       [:maybe :map]
   :rel       (fn [in out] (= in (some-> out schema/config->edn)))
   :golden-path "test/golden/agora/edn-config.edn"})

(hst/deftrifecta-from-schema count-signals-contract
  hive-mcp.agora.schema/count-signals
  {:in        DialogueIdIn
   :out       schema/SignalCounts
   :rel       (fn [id out]
                (= out (merge {:propose 0 :counter 0 :approve 0 :no-change 0 :defer 0}
                              (when (= "tri-d" id) (frequencies (map second turns))))))
   :golden-path "test/golden/agora/count-signals.edn"})

(hst/deftrifecta-from-schema schema-check-consensus-contract
  hive-mcp.agora.schema/check-consensus
  {:in        DialogueIdIn
   :out       [:map [:reached? :boolean] [:approval-ratio :double] [:threshold number?]
               [:signal-counts schema/SignalCounts]]
   ;; approve and defer both count toward the ratio
   :rel       (fn [id out]
                (if (= "tri-d" id)
                  (and (= 0.6 (:approval-ratio out)) (= 0.5 (:threshold out)) (true? (:reached? out)))
                  (and (= 0.0 (:approval-ratio out)) (false? (:reached? out)))))
   :golden-path "test/golden/agora/schema-check-consensus.edn"})

(hst/deftrifecta-from-schema get-turns-contract
  hive-mcp.agora.schema/get-turns
  {:in        DialogueIdIn
   :out       [:sequential schema/Turn]
   :rel       (fn [id out]
                (and (= (if (= "tri-d" id) (map second turns) []) (map :signal out))
                     (= (range 1 (inc (count out))) (map :turn-number out))))})

(hst/deftrifecta-from-schema get-turns-by-sender-contract
  hive-mcp.agora.schema/get-turns-by-sender
  {:in        [:cat DialogueIdIn [:enum "a-1" "b-2" "z-9"]]
   :out       [:sequential schema/Turn]
   :rel       (fn [[id sender] out]
                (and (every? #(= sender (:sender %)) out)
                     (= (count out) (if (= "tri-d" id) (count (filter #(= sender (first %)) turns)) 0))))})

(hst/deftrifecta-from-schema get-dialogue-contract
  hive-mcp.agora.schema/get-dialogue
  {:in        DialogueIdIn
   :out       [:maybe schema/Dialogue]
   :rel       (fn [id out] (if (= "tri-missing" id) (nil? out) (= id (:id out))))})

(hst/deftrifecta-from-schema gen-id-contract
  hive-mcp.agora.schema/gen-id
  {:in        [:cat]
   :out       schema/Uuid
   :rel       (fn [_ out] (uuid? (parse-uuid out)))})

(hst/deftrifecta-from-schema now-contract
  hive-mcp.agora.schema/now
  {:in        [:cat]
   :out       schema/Epoch
   :rel       (fn [_ out] (<= (- (System/currentTimeMillis) 60000) out (System/currentTimeMillis)))})

(deftest connection-test
  (is (m/validate schema/Conn (schema/get-conn)))
  (is (identical? (schema/get-conn) (schema/ensure-conn)))
  (is (m/validate schema/Conn (schema/create-conn)))
  (is (not (identical? (schema/get-conn) (schema/create-conn))) "create-conn is a fresh connection"))

(deftest dialogue-crud-test
  (let [{:keys [id tx-report]} (schema/create-dialogue! {:participants #{"x-1" "y-2"}})]
    (testing "a set of participants is accepted and an id is generated"
      (is (m/validate schema/Uuid id))
      (is (m/validate schema/TxReport tx-report))
      (is (= #{"x-1" "y-2"} (set (:participants (schema/get-dialogue id))))))
    (testing "status update answers a report, or nil for an unknown dialogue"
      (is (m/validate schema/TxReport (schema/update-dialogue-status! id :aborted)))
      (is (= :aborted (:status (schema/get-dialogue id))))
      (is (nil? (schema/update-dialogue-status! "nope" :active))))
    (testing "list-dialogues filters by status"
      (is (some #(= id (:id %)) (schema/list-dialogues :aborted)))
      (is (not-any? #(= id (:id %)) (schema/list-dialogues :active)))
      (is (m/validate [:sequential schema/DialogueRow] (schema/list-dialogues))))))

(deftest turn-crud-test
  ;; a scratch dialogue of its own, so the fixture the trifectas read is untouched
  (schema/create-dialogue! {:id "tri-scratch" :name "scratch" :participants ["a-1"]})
  (let [t (schema/add-turn! "tri-scratch" {:sender "a-1" :receiver "b-2" :message "m"})]
    (testing "the signal defaults to :propose and the turn reads back"
      (is (= 1 (:turn-number t)))
      (is (= :propose (:signal (schema/get-turn (:id t)))))
      (is (m/validate schema/Turn (schema/get-turn (:id t)))))
    (testing "a reply carries its parent"
      (let [r (schema/add-turn! "tri-scratch" {:sender "b-2" :receiver "a-1" :message "r"
                                               :signal :approve :in-reply-to (:id t)})]
        (is (= (:id t) (:in-reply-to (schema/get-turn (:id r)))))))
    (testing "unknown ids answer nil"
      (is (nil? (schema/add-turn! "nope" {:sender "a" :receiver "b" :message "m"})))
      (is (nil? (schema/get-turn "nope"))))))

(deftest reset-conn-answers-nil-test
  (let [before (schema/get-conn)]
    (is (nil? (schema/reset-conn!)))
    (is (not (identical? before (schema/get-conn))))
    (build!)))

(deftest schema-schemas-generate-test
  (doseq [s [schema/DialogueId schema/TurnId schema/DialogueStatus schema/TurnSignal schema/Conn
             schema/TxReport schema/Epoch schema/Uuid schema/Dialogue schema/DialogueRow schema/Turn
             schema/SignalCounts schema/CreateDialogueOpts schema/TurnOpts]]
    (is (seq (mg/sample s {:size 4})) (pr-str s))))
