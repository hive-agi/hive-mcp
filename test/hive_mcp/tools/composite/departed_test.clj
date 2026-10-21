(ns hive-mcp.tools.composite.departed-test
  "Facets for dispatch across a departed subdomain.

   command-root is pure: a trifecta from its contract. The rest reads the
   departures ledger, which this suite drives through its own public API
   (record! / clear!), and dispatch takes its tree and its runner as
   arguments, so every arm is reached with injected fns and no with-redefs."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.extensions.departures :as departures]
            [hive-mcp.tools.composite.departed :as dep]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f] (departures/clear!) (try (f) (finally (departures/clear!)))))

(def ^:private Command
  "Command values: keyword, padded, upper-cased, tab-separated, blank, and
   values that are neither string nor keyword."
  [:enum nil 0 "kg traverse" "  Memory add" "CARTO" "" "   " "\tagent\tspawn" :kg :Batch])

(defn- root-oracle [c]
  (let [s (cond (keyword? c) (name c) (string? c) c)
        w (some-> s str/trim (str/split #"\s+") first)]
    (when-not (str/blank? w) (str/lower-case w))))

(deftrifecta-from-schema command-root-is-the-lowercased-first-word
  hive-mcp.tools.composite.departed/command-root
  {:in          Command
   :out         [:maybe :string]
   :rel         (fn [in out] (= out (root-oracle in)))
   :num-tests   300
   :n-cases     12
   :golden-path "test/golden/tools/composite/departed-command-root.edn"})

(def ^:private handlers {:kg identity "memory" identity})

(defn- run-tagged [hs params] {:ran (:command params) :handlers (sort (map name (keys hs)))})

(deftest-golden departed-root-and-dispatch-arms
  "test/golden/tools/composite/departed-arms.edn"
  (let [now (System/currentTimeMillis)]
    (departures/record! "multi" :hive.carto ["carto"] now)
    (departures/record! "multi" :hive.old ["olds"] (- now 3600000))
    (binding [dep/*return-wait* {:budget-ms 60 :poll-ms 10 :reload-window-ms 60000}]
      {:present      (dep/departed-root "multi" handlers "kg traverse")
       :never-left   (dep/departed-root "multi" handlers "bogus")
       :departed     (dissoc (dep/departed-root "multi" handlers "carto scan") :at-ms)
       :run-present  (dep/dispatch "multi" (constantly handlers) run-tagged {:command "memory add"})
       :old-gone     (update (dep/dispatch "multi" (constantly handlers) run-tagged {:command "olds x"})
                             :text #(str/replace % #"\d+s ago" "Ns ago"))
       :reload-timeout (dep/dispatch "multi" (constantly handlers) run-tagged {:command "carto scan"})
       :returns      (let [calls (atom 0)
                           tree  (fn [] (if (< (swap! calls inc) 3) handlers (assoc handlers :carto identity)))]
                       (dep/dispatch "multi" tree run-tagged {:command "carto scan"}))})))

(deftest departed-error-names-both-arms
  (let [gone {:root "carto" :addon :hive.carto :at-ms (System/currentTimeMillis)}]
    (testing "reloading is retryable"
      (let [e (dep/departed-error "carto scan" gone handlers true)]
        (is (:isError e))
        (is (str/includes? (:text e) "Retryable"))
        (is (str/includes? (:text e) "kg, memory"))))
    (testing "not loaded is not"
      (let [e (dep/departed-error "carto scan" gone handlers false)]
        (is (str/includes? (:text e) "is not loaded"))
        (is (not (str/includes? (:text e) "Retryable")))))))
