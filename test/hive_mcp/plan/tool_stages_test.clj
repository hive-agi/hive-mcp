(ns hive-mcp.plan.tool-stages-test
  "Trifectas over the pure stages `plan-to-kanban` is composed of: the
   lifecycle guard and the success payload. The end-to-end conversion paths
   stay pinned by conversion-lifecycle-test and tool-test."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.plan.tool :as tool]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- guard-outcome
  "Ternary subject: what `conversion-guard` decides, reduced to a label."
  [id entry force]
  (let [r (#'tool/conversion-guard id entry force)]
    (cond (nil? r) :proceed
          (:isError r) :refused
          :else :already-converted)))

(def ^:private plan-entry {:id "p1" :type "plan" :content "x" :tags ["plan"]})

(deftrifecta conversion-guard
  hive-mcp.plan.tool-stages-test/guard-outcome
  {:golden-path "test/golden/plan/tool-conversion-guard.edn"
   :apply? true
   :cases {:no-entry        ["p0" nil false]
           :note-entry      ["n1" {:id "n1" :type "note"} false]
           :note-forced     ["n1" {:id "n1" :type :note} true]
           :draft-plan      ["p1" plan-entry false]
           :completed       ["p1" (assoc plan-entry :plan-status "completed") false]
           :completed-force ["p1" (assoc plan-entry :plan-status "completed") true]
           :active-no-ids   ["p1" (assoc plan-entry :plan-status "active") false]}
   :gen (gen/tuple (gen/return "p")
                   (gen/one-of [(gen/return nil)
                                (gen/hash-map :id (gen/return "p") :type (gen/elements ["plan" "note"]))])
                   gen/boolean)
   :pred #{:proceed :refused :already-converted}
   :num-tests 50
   :mutations [["never-refuses" (constantly :proceed)]
               ["ignores-type" (fn [_ e _] (if e :proceed :proceed))]]})

(defn- response-shape [plan-id source result]
  (#'tool/conversion-response plan-id source result))

(deftrifecta conversion-response
  hive-mcp.plan.tool-stages-test/response-shape
  {:golden-path "test/golden/plan/tool-conversion-response.edn"
   :apply? true
   :cases {:empty   ["p1" :memory {}]
           :full    ["file:/x.edn" :file {:plan {:title "T"} :decision-id "d1"
                                         :task-ids ["t1" "t2"] :waves {"s1" 0 "s2" 2}
                                         :kg-edges [{:id "e"}] :kg-degraded? nil
                                         :pending? true :kg-warnings ["w"]
                                         :step-mapping {"s1" "t1"}}]}
   :gen (gen/tuple (gen/return "p") (gen/elements [:file :memory])
                   (gen/hash-map :task-ids (gen/vector gen/string-alphanumeric 0 4)
                                 :waves (gen/map gen/string-alphanumeric (gen/choose 0 5))))
   :pred (fn [r] (and (:success r) (= (count (:task-ids r)) (:task-count r))
                      (boolean? (:kg-degraded? r)) (boolean? (:kg-pending? r))))
   :num-tests 50
   :mutations [["drops-count" (fn [p s r] (dissoc (response-shape p s r) :task-count))]
               ["min-wave" (fn [p s r] (assoc (response-shape p s r) :max-wave
                                              (when (seq (:waves r)) (apply min (vals (:waves r))))))]]})

(deftest conversion-failure-carries-fsm-detail
  (let [r (#'tool/conversion-failure (ex-info "bad" {:validation {:k 1} :errors ["e1" "e2"]}))
        t (:text r)]
    (is (:isError r))
    (is (re-find #"Plan conversion failed: bad" t))
    (is (re-find #"Validation: \{:k 1\}" t))
    (is (re-find #"Errors: e1, e2" t)))
  (let [r (#'tool/conversion-failure (RuntimeException. "boom"))]
    (is (re-find #"Failed to convert plan to kanban: boom" (:text r)))))
