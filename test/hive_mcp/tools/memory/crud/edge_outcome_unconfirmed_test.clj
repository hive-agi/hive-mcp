(ns hive-mcp.tools.memory.crud.edge-outcome-unconfirmed-test
  "RUN-LEDGER-6: when a memory add cannot confirm its KG edges (an edge write
   failed or outran its budget, or the :kg-outgoing link stage did), the
   :kg_check names EXACTLY which edges are unconfirmed, each as
   {:from :to :relation}, so a caller counting through them can re-check
   or re-link just those."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.memory.crud.edge-outcome :as eo]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private requests
  [{:relation "refines" :to "r1"} {:relation "depends-on" :to "d1"}])

(deftest failed-edge-writes-are-named
  (let [errs  (mapv #(assoc % :error "stage :kg-edges over budget") requests)
        check (eo/outcome-check "e1" errs nil requests)]
    (is (= "unconfirmed" (:status check)))
    (is (= [{:from "e1" :to "r1" :relation "refines"}
            {:from "e1" :to "d1" :relation "depends-on"}]
           (:unconfirmed_edges check)))
    (is (re-find #"kg edge from=e1" (:relink check)))))

(deftest a-pending-link-names-every-requested-edge
  (testing "the edges exist but the :kg-outgoing link outran its budget"
    (let [check (eo/outcome-check "e1" [] {:stage :link-outgoing :reason :budget-exceeded}
                                  requests)]
      (is (= [{:from "e1" :to "r1" :relation "refines"}
              {:from "e1" :to "d1" :relation "depends-on"}]
             (:unconfirmed_edges check))))))

(deftest nothing-unconfirmed-is-nil
  (is (nil? (eo/outcome-check "e1" [] nil requests))))

(deftest three-arity-back-compat
  (testing "callers that do not pass requests still get targets"
    (let [check (eo/outcome-check "e1" [{:relation "refines" :to "r1" :error "x"}] nil)]
      (is (= ["r1"] (:targets check)))
      (is (= [{:from "e1" :to "r1" :relation "refines"}] (:unconfirmed_edges check))))))

(deftest edge-errors-carry-from
  (let [out (eo/attempt-edges #(throw (ex-info "store closed" {})) requests "e1")]
    (is (= [{:from "e1" :relation "refines" :to "r1" :error "store closed"}
            {:from "e1" :relation "depends-on" :to "d1" :error "store closed"}]
           (:edge-errors out)))))
