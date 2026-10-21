(ns hive-mcp.channel.inbound-trifecta-test
  "Facets for `hive-mcp.channel.inbound`: a trifecta on the pure
   `hivemind-response` over client messages that reach every arm (keyword
   and string keys, answer vs decision, blank fields, non-maps).

   `handle-client-message!` delivers an answer through
   `hivemind.messaging/respond-ask!`, resolved at call time and delegating
   to hive-agent: a concretion with no injectable seam here, so only its
   never-throws contract is asserted."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.channel.inbound :as inbound]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Msg
  [:or
   ;; a well-formed answer, keyword keyed, answer or decision, by present or not
   [:map
    [:type [:enum "hivemind-response" :hivemind-response]]
    [:ask-id [:enum "q-1" :q-kw]]
    [:answer {:optional true} [:enum "yes" "Let it ship"]]
    [:decision [:enum "approve" " "]]
    [:by {:optional true} [:enum "pedro" "" nil]]]
   ;; near misses: blank or missing ids and answers, wrong type
   [:map
    [:type [:enum "hivemind-response" "other"]]
    [:ask-id {:optional true} [:enum "q-1" "" "  " nil]]
    [:answer {:optional true} [:enum "yes" "" nil]]]
   ;; string keyed (the bencode channel)
   [:map-of [:enum "type" "ask-id" "answer" "by"] [:enum "hivemind-response" "q-2" "no" "emacs"]]
   [:enum nil "text" 42 true]])

(defn- field [m k] (let [v (get m (keyword k) (get m k))] (if (keyword? v) (name v) v)))

(defn- expected [msg]
  (when (and (map? msg) (= "hivemind-response" (field msg "type")))
    (let [id (field msg "ask-id")
          a  (or (field msg "answer") (field msg "decision"))
          by (field msg "by")]
      (when (and (string? id) (not (str/blank? id)) (string? a) (not (str/blank? a)))
        [id a (if (str/blank? by) "human" by)]))))

(deftrifecta-from-schema hivemind-response-trifecta
  hive-mcp.channel.inbound/hivemind-response
  {:in          Msg
   :out         [:maybe inbound/AskAnswer]
   :rel         (fn [msg out] (= out (expected msg)))
   :num-tests   400
   :n-cases     16
   :golden-path "test/golden/channel/inbound-hivemind-response.edn"})

(deftest answers-are-recognised-across-key-styles-test
  (is (= ["q" "yes" "human"] (inbound/hivemind-response {:type "hivemind-response" :ask-id "q" :answer "yes"})))
  (is (= ["q" "ok" "emacs"] (inbound/hivemind-response {"type" "hivemind-response" "ask-id" "q" "decision" "ok" "by" "emacs"}))))

(deftest handle-client-message-never-throws-test
  (doseq [m [{:type "ping"} {"type" "hivemind-response" "ask-id" "q"} nil "text" [1 2]
             {:type "hivemind-response" :ask-id "f02-no-such-ask" :answer "yes"}]]
    (is (nil? (inbound/handle-client-message! m "client-1")) (pr-str m))))
