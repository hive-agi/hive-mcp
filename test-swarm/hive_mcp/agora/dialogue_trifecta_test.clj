(ns hive-mcp.agora.dialogue-trifecta-test
  "Trifectas for `hive-mcp.agora.dialogue` driven by its contracts, over a
   fixed set of dialogues built once per namespace. Runs in the swarm suite:
   dialogue state lives behind the agora schema shim, which delegates to
   hive-agent."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agora.dialogue :as dialogue]
            [hive-mcp.agora.schema :as schema]
            [hive-schemas.test :as hst]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private fixture-turns
  {"dlg-agree" [["w-1" "c-2" :propose] ["c-2" "w-1" :approve] ["w-1" "c-2" :no-change]]
   "dlg-open"  [["w-1" "c-2" :propose] ["c-2" "w-1" :counter]]
   "dlg-empty" []})

(defn- build! []
  (dialogue/reset-dialogues!)
  (doseq [[id turns] fixture-turns]
    (schema/create-dialogue! {:id id :name (str "topic " id) :participants ["w-1" "c-2"]})
    (doseq [[from to sig] turns]
      (schema/add-turn! id {:sender from :receiver to :message (name sig) :signal sig})))
  (schema/create-dialogue! {:id "dlg-done" :name "done" :participants ["w-1" "c-2"]})
  (schema/update-dialogue-status! "dlg-done" :consensus))

(use-fixtures :once (fn [f] (build!) (f)))

(def ^:private DialogueIdIn [:enum "dlg-agree" "dlg-open" "dlg-empty" "dlg-done" "dlg-missing"])

(hst/deftrifecta-from-schema parse-signal-contract
  hive-mcp.agora.dialogue/parse-signal
  {:in        [:enum "[SIGNAL: propose] Here's my change" "[SIGNAL: Approve] test"
               "[SIGNAL: no-change] Nothing" "[SIGNAL: unknown] text" "plain" ""]
   :out       [:tuple [:enum :propose :approve :no-change] :string dialogue/Detection]
   :rel       (fn [in [sig cleaned method]]
                (if (= :default method)
                  (and (= :propose sig) (= in cleaned))
                  (not= in cleaned)))
   :n-cases   8
   :golden-path "test/golden/agora/dialogue-parse-signal.edn"})

(hst/deftrifecta-from-schema dialogue-summary-contract
  hive-mcp.agora.dialogue/dialogue-summary
  {:in        DialogueIdIn
   :out       [:maybe [:map [:id :string] [:topic [:maybe :string]] [:status schema/DialogueStatus]
                       [:participants [:vector :string]] [:turn-count [:int {:min 0}]]
                       [:last-signal [:maybe schema/TurnSignal]] [:consensus? :boolean]]]
   :rel       (fn [id out]
                (if (= "dlg-missing" id)
                  (nil? out)
                  (and (= (count (fixture-turns id)) (:turn-count out))
                       (= (some-> (fixture-turns id) last (nth 2)) (:last-signal out))
                       (= (= "dlg-done" id) (:consensus? out)))))
   :n-cases   10
   :golden-path "test/golden/agora/dialogue-summary.edn"})

(hst/deftrifecta-from-schema get-participants-contract
  hive-mcp.agora.dialogue/get-participants
  {:in        DialogueIdIn
   :out       [:set :string]
   :rel       (fn [id out] (= out (if (= "dlg-missing" id) #{} #{"w-1" "c-2"})))
   :n-cases   10})

(hst/deftrifecta-from-schema nash-equilibrium-contract
  hive-mcp.agora.dialogue/nash-equilibrium?
  {:in        DialogueIdIn
   :out       :boolean
   ;; the basic check reads the last turn ADDRESSED to each participant.
   ;; An unknown dialogue has no participants, so nobody agrees: false.
   :rel       (fn [id out] (= out (= "dlg-agree" id)))
   :n-cases   10})

(hst/deftrifecta-from-schema dialogue-active-contract
  hive-mcp.agora.dialogue/dialogue-active?
  {:in        DialogueIdIn
   :out       :boolean
   :rel       (fn [id out] (= out (contains? fixture-turns id)))
   :n-cases   10})

(hst/deftrifecta-from-schema get-dialogue-turns-contract
  hive-mcp.agora.dialogue/get-dialogue-turns
  {:in        DialogueIdIn
   :out       [:sequential dialogue/TurnView]
   :rel       (fn [id out] (= (map #(nth % 2) (fixture-turns id)) (map :signal out)))
   :n-cases   10})

(hst/deftrifecta-from-schema get-last-turn-for-contract
  hive-mcp.agora.dialogue/get-last-turn-for
  {:in        [:cat DialogueIdIn [:enum "w-1" "c-2" "z-9"]]
   :out       [:maybe dialogue/TurnView]
   :rel       (fn [[_ who] out] (or (nil? out) (= who (:receiver out))))
   :n-cases   12})

(hst/deftrifecta-from-schema get-dialogue-contract
  hive-mcp.agora.dialogue/get-dialogue
  {:in        DialogueIdIn
   :out       [:maybe dialogue/DialogueView]
   :rel       (fn [id out] (if (= "dlg-missing" id) (nil? out) (= id (:id out))))
   :n-cases   10})

(deftest lifecycle-contracts-test
  (let [id (dialogue/create-dialogue {:participants ["p-1" "p-2" "p-3"] :topic "t"})]
    (is (string? id))
    (is (true? (dialogue/join-dialogue id "p-4")))
    (is (false? (dialogue/join-dialogue "nope" "p-4")))
    (is (true? (dialogue/leave-dialogue id "p-4")))
    (is (false? (dialogue/leave-dialogue "nope" "p-4")))
    (is (= :active (:status (dialogue/get-dialogue id))))
    (testing "leaving below two participants aborts the dialogue"
      (dialogue/leave-dialogue id "p-1")
      (dialogue/leave-dialogue id "p-2")
      (is (= :aborted (:status (dialogue/get-dialogue id))))))
  (is (m/validate [:sequential [:map [:id :string] [:topic :string] [:status schema/DialogueStatus]]]
                  (dialogue/list-dialogues))))

(deftest dispatch-refuses-an-inactive-dialogue-test
  (testing "the status names the refusal before any swarm dispatch is attempted"
    (let [e (try (dialogue/dialogue-dispatch {:dialogue-id "dlg-done" :to "c-2" :message "m"})
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= "Dialogue already reached consensus" (ex-message e)))
      (is (= :consensus (:status (ex-data e)))))
    (let [e (try (dialogue/approve "dlg-missing" "c-2" "ok") nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= "Dialogue not active" (ex-message e))))))

(deftest reset-dialogues-answers-nil-test
  (is (nil? (dialogue/reset-dialogues!)))
  (is (empty? (dialogue/list-dialogues)))
  (build!))
