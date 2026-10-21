(ns hive-mcp.tools.consolidated.transcript-trifecta-test
  "Trifecta over transcript/handle-transcript with the :runs port injected:
   the runs command answers exactly the run ids the port lists, and any
   unknown command leaves as an MCP error envelope."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-dsl.result :as r]
            [hive-mcp.tools.consolidated.transcript :as tr]
            [hive-mcp.tools.core :as tc]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- runs-port [ids]
  {:list-runs (constantly (r/ok (mapv (fn [i] {:run-id i :agent-id (str "ling-" i)
                                               :project-id "m27" :created 1000})
                                      ids)))
   :run-tip   (constantly (r/ok nil))})

(defn run-ids-of
  "Unary adapter: list runs through an injected port listing IDS."
  [ids]
  (let [resp (tr/handle-transcript {:runs (runs-port ids)} {:command "runs" :limit 100})]
    (if (:isError resp)
      {:error (:text resp)}
      (set (map :run_id (:runs (json/read-str (:text resp) :key-fn keyword)))))))

(deftrifecta runs-through-injected-port
  hive-mcp.tools.consolidated.transcript-trifecta-test/run-ids-of
  {:golden-path "test/golden/tools/consolidated/transcript-runs-port.edn"
   :cases       {:none [] :one ["r1"] :three ["r1" "r2" "r3"]}
   :gen         (gen/vector-distinct (gen/fmap #(str "run-" %) gen/nat) {:max-elements 5})
   :property-type :oracle
   :oracle      set
   :num-tests   50
   :mutations   [["drops-one" (fn [ids] (set (rest ids)))]
                 ["empty" (constantly #{})]]})

(deftest unknown-command-is-an-error-envelope
  (let [resp (tr/handle-transcript {:runs (runs-port [])} {:command "m27-bogus"})]
    (is (m/validate tc/McpResponse resp))
    (is (true? (:isError resp)))))

(deftest hive-agent-runs-answers-the-port-shape
  (let [port (tr/hive-agent-runs)]
    (is (m/validate tr/RunsPort port))
    (is (r/ok? ((:list-runs port))))
    (is (every? #(m/validate tr/RunsPort %) (mg/sample tr/RunsPort {:size 3})))))
