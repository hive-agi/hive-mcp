(ns hive-mcp.tools.events.core-test
  "Facets for the `events` tool router.

   The routes are driven with params each subcommand rejects before it
   touches hive-agent (a missing id), so the router's arms are pinned
   without the addon and without any side effect."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.core :as core]
            [hive-mcp.tools.events.core :as ec]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Unknown
  "A command no handler is registered for, as string, colon-string or keyword."
  [:map [:command [:enum "bogus" ":nope" :frobnicate "tails" "STATS"]]])

(deftrifecta-from-schema unknown-command-lists-the-valid-ones
  hive-mcp.tools.events.core/handle
  {:in        Unknown
   :out       core/McpResponse
   :rel       (fn [_ out]
                (let [body (json/read-str (:text out) :key-fn keyword)]
                  (and (:isError out)
                       (false? (:ok body))
                       (str/includes? (:error body) "Valid: disable, dump, enable, help"))))
   :num-tests 100})

(defn- body [resp] (update (json/read-str (:text resp) :key-fn keyword) :result
                           #(if (string? %) (first (str/split-lines %)) %)))

(deftest-golden events-router-arms
  "test/golden/tools/events/core-router.edn"
  {:no-command   [(boolean (:isError (ec/handle nil))) (body (ec/handle nil))]
   :help         [(boolean (:isError (ec/handle {:command "help"}))) (body (ec/handle {:command "help"}))]
   :non-string   (body (ec/handle {:command 5}))
   :unknown      [(:isError (ec/handle {:command "bogus"})) (body (ec/handle {:command "bogus"}))]
   :stats-no-id  (body (ec/handle {:command ":stats"}))
   :tail-no-id   (body (ec/handle {:command :tail}))
   :unreg-no-id  (body (ec/handle {:command "unreg-fx"}))})

(deftest tool-def-routes-to-handle
  (is (= "events" (:name ec/tool-def)))
  (is (= [ec/tool-def] ec/tools))
  (is (= ["command"] (get-in ec/tool-def [:inputSchema :required]))))
