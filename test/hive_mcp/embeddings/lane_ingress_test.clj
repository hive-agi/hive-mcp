(ns hive-mcp.embeddings.lane-ingress-test
  "MCP tool calls embed in the :interactive lane; work they push to the
   background, and everything outside a tool call, stays :batch."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-mcp.embeddings.shared-gate :as shared]
            [hive-mcp.server.routes.middleware :as mw]))

(defn blocking-provider
  "Stub EmbeddingProvider: records each text, blocks on `release` for \"hold\"."
  [events entered release]
  (reify proto/EmbeddingProvider
    (embed-text [_ text]
      (when (= text "hold") (deliver entered true) @release)
      (swap! events conj text)
      [1.0])
    (embed-batch [this texts] (mapv #(proto/embed-text this %) texts))
    (embedding-dimension [_] 1)))

(defn tool-call
  "Run `f` the way an MCP tool handler runs: inside wrap-handler-context."
  [f]
  (binding [mw/*identity-get-slave* (constantly nil)]
    ((mw/wrap-handler-context (fn [_] (f))) {})))

(deftest lane-defaults-to-batch-outside-a-tool-call
  (is (= :batch (shared/current-lane))))

(deftest tool-ingress-binds-interactive
  (is (= :interactive (tool-call shared/current-lane))))

(deftest background-work-from-a-tool-call-is-batch
  (testing "a future conveys the binding, batch-fn drops it back"
    (is (= :batch (tool-call #(deref (future ((shared/batch-fn shared/current-lane)))))))
    (is (= :batch (tool-call #(deref (future (shared/with-lane :batch (shared/current-lane)))))))))

(deftest tool-call-overtakes-queued-batch-work
  (let [entered (promise)
        release (promise)
        events  (atom [])
        gate    (shared/new-gate 1 5000)
        port    (shared/gated-provider (blocking-provider events entered release) gate)
        holder  (future (proto/embed-text port "hold"))]
    (try
      (is (deref entered 2000 false))
      (let [batch1 (future (proto/embed-text port "batch-1"))
            _      (is (shared/await-waiters gate :batch 1 2000))
            batch2 (future (proto/embed-text port "batch-2"))
            _      (is (shared/await-waiters gate :batch 2 2000))
            search (future (tool-call #(proto/embed-text port "search")))]
        (is (shared/await-waiters gate :interactive 1 2000))
        (deliver release true)
        (is (= [1.0] (deref search 2000 ::timeout)))
        (deref batch1 2000 nil)
        (deref batch2 2000 nil)
        (is (= "search" (second @events)) "the tool call takes the first free permit")
        (is (= #{"batch-1" "batch-2"} (set (drop 2 @events)))))
      (finally (deliver release true) (deref holder 2000 nil)))))

(deftest permits-are-configurable
  (is (= shared/default-permits (shared/permits-from-config {})))
  (is (= 3 (shared/permits-from-config {:embeddings {:gate {:permits 3}}})))
  (is (= shared/default-permits (shared/permits-from-config {:embeddings {:gate {:permits 0}}}))))
