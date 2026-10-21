(ns hive-mcp.scheduler.housekeeping-test
  "Name-matched facets for `hive-mcp.scheduler.housekeeping`.

     disjoint-sweep-owners?  trifecta: names drawn from the owned inventory
                             and from foreign names, in vectors, sets and
                             lists, so both arms are reached
     lifecycle               golden over the stopped scheduler (stop! on a
                             stopped executor, status, the owned inventory)

   `housekeeping-sweep!` is not driven here: every task is reached through
   requiring-resolve on a concrete namespace (swarm callbacks, the bounded-atom
   GC, the KG GC lane, the terminal liveness sweep...) and the sweep ends in an
   explicit System/gc. It has no injectable port, so there is no stub seam
   short of with-redefs; recorded in the f06 report."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.scheduler.housekeeping :as hk]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private SweepName
  [:enum "lings/ledger-cold" "memory/pending-reembed" "knowledge-graph/gc-lane"
   "rss/poll" "kanban/archive" "carto/reindex" "lings/ledger" "x"])

(def ^:private RegisteredNames
  "Registered ISweepable names in any collection shape the contract admits;
   at most two names, so a collision is drawn about half the time."
  [:or [:vector {:max 2} SweepName] [:set {:max 2} SweepName]
   [:sequential {:max 2} SweepName] :nil])

(deftrifecta-from-schema disjoint-sweep-owners?-is-set-disjointness
  #'hk/disjoint-sweep-owners?
  {:in [:cat RegisteredNames]
   :out :boolean
   :rel (fn [[names] out]
          (= out (not-any? #(contains? hk/housekeeping-sweep-names %) names)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 300})

(deftest-golden housekeeping-stopped-lifecycle-golden
  "test/golden/hive-mcp/scheduler/housekeeping-stopped.edn"
  (let [stopped (hk/stop!)
        s       (hk/status)]
    {:stop        stopped
     :running?    (:running? s)
     :config      (:config s)
     :inventory   (vec (sort hk/housekeeping-sweep-names))
     :disjoint    {:empty   (hk/disjoint-sweep-owners? [])
                   :foreign (hk/disjoint-sweep-owners? #{"rss/poll"})
                   :owned   (hk/disjoint-sweep-owners? '("system/jvm-gc-hint"))}}))

(deftest stop-is-idempotent-when-stopped
  (is (= {:stopped false :reason "not-running"} (hk/stop!)))
  (is (= {:stopped false :reason "not-running"} (hk/stop!))))
