(ns hive-mcp.channel.conversation-inbox-golden-test
  "Facets for the conversation inbox: a property that any push sequence
   drains back as the newest `cap` envelopes of each section, oldest first,
   and a golden over one scripted session (sections, peek, pending, the
   cap, clear and reset)."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.channel.conversation-inbox :as inbox]
            [hive-test.golden :refer [deftest-golden-fn]]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f]
    (inbox/reset-all!)
    (try (f) (finally (inbox/reset-all!) (inbox/set-section-cap! inbox/default-section-cap)))))

(def ^:private Envelope
  [:map
   [:event-type [:enum :conversation/tell :conversation/ask :conversation/respond :other/kind]]
   [:from [:enum "a" "b"]]
   [:n :int]])

(def ^:private Script
  [:tuple [:int {:min 1 :max 4}] [:vector {:max 12} Envelope]])

(defn- section-of [{:keys [event-type]}]
  (case event-type :conversation/ask :ask :conversation/respond :respond :tell))

(defspec push-then-drain-keeps-the-newest-cap-per-section 200
  (prop/for-all [[cap envs] (mg/generator Script)]
    (inbox/reset-all!)
    (inbox/set-section-cap! cap)
    (run! #(inbox/push! "ling-p" %) envs)
    (let [pending (inbox/pending-count "ling-p")
          out     (inbox/drain! "ling-p")
          expect  (into {} (for [[s es] (group-by section-of envs)]
                             [s (vec (take-last cap es))]))]
      (and (= (if (seq envs) expect nil) out)
           (= pending (reduce + 0 (map count (vals expect))))
           (m/validate inbox/Sections (or out {}))
           (nil? (inbox/drain! "ling-p"))))))

(deftest-golden-fn inbox-session-golden
  "test/golden/channel/conversation-inbox-session.edn"
  (fn []
    (inbox/reset-all!)
    (inbox/set-section-cap! 2)
    [[:empty-peek    (inbox/peek* "l1")]
     [:push-tell     (inbox/push! "l1" {:event-type :conversation/tell :from "a" :message "hi"})]
     [:push-ask      (inbox/push! "l1" {:event-type :conversation/ask :from "a" :ask-id "q1" :question "?"})]
     [:push-unknown  (inbox/push! "l1" {:event-type :odd :from "b"})]
     [:push-over-cap (inbox/push! "l1" {:event-type :conversation/tell :from "c" :message "third"})]
     [:push-other    (inbox/push! "l2" {:event-type :conversation/respond :ask-id "q1" :answer "yes"})]
     [:pending?      [(inbox/pending? "l1") (inbox/pending? "nobody")]]
     [:counts        [(inbox/pending-count "l1") (inbox/pending-count "l2") (inbox/agent-count)]]
     [:peek          (inbox/peek* "l1")]
     [:drain         (inbox/drain! "l1")]
     [:drain-again   (inbox/drain! "l1")]
     [:clear-l2      (inbox/clear! "l2")]
     [:clear-l2-again (inbox/clear! "l2")]
     [:reset         (inbox/reset-all!)]
     [:agents        (inbox/agent-count)]]))

(deftest push-refuses-bad-arguments-test
  (is (thrown? AssertionError (inbox/push! :not-a-string {})))
  (is (thrown? AssertionError (inbox/set-section-cap! 0))))
