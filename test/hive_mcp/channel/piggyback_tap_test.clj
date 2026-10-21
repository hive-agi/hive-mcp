(ns hive-mcp.channel.piggyback-tap-test
  "Golden over `hive-mcp.channel.piggyback-tap/drain-all!`: the conversation
   inbox is filled through its own API and the catchup channel is a stub
   drain fn registered under `:cu/piggyback-drain` in the extension
   registry, so the rendered blocks, their order and the nil-when-empty
   rule are pinned without a live hivemind. Agent ids are unique to this
   test, so the hivemind and memory channels have nothing for them."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.channel.conversation-inbox :as inbox]
            [hive-mcp.channel.piggyback-tap :as tap]
            [hive-mcp.extensions.registry :as ext]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f]
    (let [prior (ext/get-extension :cu/piggyback-drain)]
      (try (f)
           (finally
             (run! inbox/clear! ["f02-tap-a" "f02-tap-b" "f02-tap-c"])
             (if prior
               (ext/register! :cu/piggyback-drain prior)
               (ext/deregister! :cu/piggyback-drain)))))))

(defn- catchup-stub
  "Catchup drain: two blocks for f02-tap-b, a throw for f02-tap-c, else nil."
  [caller-id]
  (case caller-id
    "f02-tap-b" {:axioms "remember the axiom" :kanban-hints {:todo 2}}
    "f02-tap-c" (throw (ex-info "catchup down" {}))
    nil))

(deftest-golden-fn drain-all-golden
  "test/golden/channel/piggyback-tap-drain-all.edn"
  (fn []
    (ext/register! :cu/piggyback-drain catchup-stub)
    (inbox/push! "f02-tap-a" {:event-type :conversation/tell :from "coord" :message "hi"})
    (inbox/push! "f02-tap-a" {:event-type :conversation/ask :from "coord" :ask-id "q1" :question "ok?"})
    (inbox/push! "f02-tap-c" {:event-type :conversation/respond :ask-id "q1" :answer "yes"})
    [[:inbox-only      (tap/drain-all! "f02-tap-a" "f02-proj")]
     [:inbox-drained   (tap/drain-all! "f02-tap-a" "f02-proj")]
     [:catchup-only    (tap/drain-all! "f02-tap-b" nil)]
     [:catchup-throws  (tap/drain-all! "f02-tap-c" nil {:tokens #{}})]
     [:nothing         (tap/drain-all! "f02-tap-none" "f02-proj")]]))

(deftest empty-drain-is-nil-test
  (ext/deregister! :cu/piggyback-drain)
  (is (nil? (tap/drain-all! "f02-tap-a" nil))))
