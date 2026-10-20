(ns hive-mcp.events.effects.conversation-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.effects.conversation :as conversation]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(deftrifecta-from-schema inbox-without-recipient
  #'conversation/handle-inbox-push
  {:in [:map [:agent-id {:optional true} :nil]
        [:envelope {:optional true} [:map [:kind :keyword]]]]
   :out :nil
   :rel (fn [_ result] (nil? result))
   :num-tests 500})

(deftest conversation-registration-repeats
  (is (true? (conversation/register-conversation-effects!)))
  (is (true? (conversation/register-conversation-effects!)))
  (is (false? (conversation/reset-registration!))))
