(ns hive-mcp.tools.analysis-bridge-contract-test
  "Contract facets for the enriched analysis bridge-status. build-status is
   pure; roots are drawn from a small alphabet so the de-duplication across
   the registered and resolved axes is exercised. The live probes
   (bridge-status, install!) reach the live LSP bridge and the extension
   registry, so they are contracted but not driven here."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.analysis-bridge :as bridge]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Ws
  [:map
   [:root {:optional true} [:enum "/a" "/b" "/c" nil]]
   [:source [:enum :live-lsp :sidecar-cache :in-process]]])

(def ^:private Probe
  [:map
   [:registered {:optional true} [:maybe [:vector Ws]]]
   [:resolved {:optional true} [:maybe [:vector Ws]]]
   [:live-probe {:optional true} [:maybe [:map [:lsp-available {:optional true} :boolean]]]]])

(defn- union-count
  "Oracle: entries without a root all count; rooted entries count once per root."
  [ws]
  (+ (count (remove :root ws)) (count (distinct (keep :root ws)))))

(deftrifecta-from-schema build-status-counts-the-union
  #'bridge/build-status
  {:in [:cat Probe]
   :out bridge/BridgeStatus
   :rel (fn [[{:keys [registered resolved live-probe]}] out]
          (let [all (concat registered resolved)]
            (and (= (union-count all) (:workspace-count out) (count (:workspaces out)))
                 (= (vec registered) (:workspaces-registered out))
                 (= (count registered) (get-in out [:sources :live-lsp]))
                 (= (count (filter #(= :in-process (:source %)) resolved))
                    (get-in out [:sources :in-process]))
                 (= (boolean (:lsp-available live-probe)) (:lsp-available out)))))
   :golden-path "test/golden/hive-mcp/tools/analysis-bridge-build-status.edn"
   :num-tests 200})

(deftest empty-probes-still-answer-vectors
  (let [s (bridge/build-status {})]
    (is (m/validate bridge/BridgeStatus s))
    (is (= [] (:workspaces s)))
    (is (not (m/validate bridge/BridgeStatus (assoc s :workspaces nil))))))
