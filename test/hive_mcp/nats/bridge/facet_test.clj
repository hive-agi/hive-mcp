(ns hive-mcp.nats.bridge.facet-test
  "Facets for hive-mcp.nats.bridge (cov-facets f07): the subject builders are
   driven from their contracts, the publishers and subscription lifecycle
   through an injected recording IEventBackbone installed with the port's own
   setter (eb/set-backbone!). The pure summarize-error / token-guard cases
   stay in bridge_test.clj."
  (:require [clojure.string :as str]
            [hive-mcp.nats.bridge :as bridge]
            [hive-mcp.protocols.event-backbone :as eb]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private NamedToken
  "A non-blank, dot-free token: what name/str render verbatim into a subject."
  [:or [:enum "agent-spawn" "memory-add" "hive" "x_1"] :keyword :symbol])

(deftrifecta-from-schema event-subject-contract bridge/event-subject
  {:in  [:cat NamedToken]
   :out [:string {:min 14}]
   :rel (fn [[t] out] (= out (str "hive.v1.event." (name t))))
   :golden-path "test/golden/hive-mcp/f07-bridge-event-subject.edn"})

(deftrifecta-from-schema agent-subject-contract bridge/agent-subject
  {:in  [:cat [:string {:min 1 :max 12}] [:enum :completed :failed :progress]]
   :out [:string {:min 14}]
   :rel (fn [[ling ev] out] (= out (str "hive.v1.agent." (name ev) "." ling)))
   :golden-path "test/golden/hive-mcp/f07-bridge-agent-subject.edn"})

(deftrifecta-from-schema agent-wildcard-contract bridge/agent-wildcard
  {:in  [:cat [:enum :completed :failed "progress"]]
   :out [:string {:min 14}]
   :rel (fn [[ev] out] (= out (str "hive.v1.agent." (name ev) ".>")))
   :golden-path "test/golden/hive-mcp/f07-bridge-agent-wildcard.edn"})

(deftrifecta-from-schema tool-subject-contract bridge/tool-subject
  {:in  [:cat [:or NamedToken [:enum "" "  " nil]]]
   :out [:string {:min 14}]
   :rel (fn [[t] out]
          (if (or (nil? t) (str/blank? (name t)))
            (= out "hive.v1.tool.unknown")
            (= out (str "hive.v1.tool." (name t)))))
   :golden-path "test/golden/hive-mcp/f07-bridge-tool-subject.edn"})

(defrecord RecordingBackbone [log]
  eb/IEventBackbone
  (backbone-id [_] :recording)
  (connected? [_] true)
  (publish! [_ subject payload]
    (swap! log conj [:pub subject (dissoc payload :nats/source-node)
                     (string? (:nats/source-node payload))])
    nil)
  (subscribe! [_ subject _handler] (swap! log conj [:sub subject]) nil)
  (unsubscribe! [_ subject] (swap! log conj [:unsub subject]) nil))

(defn- with-recording-backbone
  "Install a RecordingBackbone through the port's own setter, run f, restore
   the previous backbone, and answer {:returns (f) :log [...]}."
  [f]
  (let [log  (atom [])
        prev (eb/get-backbone)]
    (eb/set-backbone! (->RecordingBackbone log))
    (try {:returns (f) :log @log}
         (finally (eb/set-backbone! prev)))))

(deftest-golden bridge-publishers-golden
  "test/golden/hive-mcp/f07-bridge-publishers.edn"
  (with-recording-backbone
    #(vector
      (bridge/publish-shout! {:agent-id "a1" :project-id "hive" :message "m"})
      (bridge/publish-shout! {:message "no ids"})
      (bridge/publish-event! {:type :agent-spawn})
      (bridge/publish-tool-notification! {:tool-name :memory-add})
      (bridge/publish-tool-notification! {:tool-name ""})
      (bridge/publish-agent-event! {:event-type :failed :ling-id "l1"})
      (bridge/publish-wave-event! {:task-id "t-without-run"})
      (bridge/publish-wave-event! {:run-id "r1" :task-id "t1"})
      (bridge/start-subscriptions!)
      (bridge/stop-subscriptions!))))
