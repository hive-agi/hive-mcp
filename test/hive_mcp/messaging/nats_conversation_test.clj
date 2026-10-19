(ns hive-mcp.messaging.nats-conversation-test
  "NATS conversation subject vocabulary contracts and directory gate."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.messaging.nats-conversation :as conv]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :as hst]))

(deftest-contract-coverage messaging-contracts
  {:paths ["src/hive_mcp/messaging"]})

(hst/deftrifecta-from-schema tell-subject-contract
  hive-mcp.messaging.nats-conversation/tell-subject
  {:in [:enum "ling-1" "a" "foo.bar"]
   :out [:string {:min 15}]
   :rel (fn [id subject] (= subject (str "hive.conv.tell." id)))
   :golden-path "test/golden/hive-mcp/messaging/tell-subject-contract.edn"
   :num-tests 100})

(hst/deftrifecta-from-schema respond-subject-contract
  hive-mcp.messaging.nats-conversation/respond-subject
  {:in [:enum "ask-1" "a" "some-id"]
   :out [:string {:min 18}]
   :rel (fn [id subject] (= subject (str "hive.conv.respond." id)))
   :golden-path "test/golden/hive-mcp/messaging/respond-subject-contract.edn"
   :num-tests 100})

(deftest wildcard-subjects
  (is (= "hive.conv.tell.>" (conv/tell-wildcard)))
  (is (= "hive.conv.ask.>" (conv/ask-wildcard)))
  (is (= "hive.conv.respond.>" (conv/respond-wildcard))))
