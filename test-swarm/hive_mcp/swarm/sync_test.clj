(ns hive-mcp.swarm.sync-test
  "Tests for swarm sync module, especially Layer 4 hook integration.

   Layer 4 of 4-layer convergence pattern:
   - ARCHITECTURAL GUARANTEE that task completion emits a shout
   - Even if ling forgot to call hivemind_shout, synthetic shout fires
   - Ensures coordinator always sees task completion

   The shout is captured through the IDeliveryChannel port, not with-redefs:
   the synthetic shout reaches this process's own delivery channels, so a
   recording channel registered for the test sees exactly what a reader
   sees.

   Position is not evidence. A shared JVM leaves other namespaces' hooks and
   channels in place, so a layer4 shout is found by its :source marker
   rather than by being first in the ring."
  (:require [clojure.test :refer [deftest testing is use-fixtures run-tests]]
            [hive-mcp.swarm.sync :as sync]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.hooks.core :as hooks]
            [hive-mcp.protocols.delivery-channel :as dc]
            [hive-test.isolation :as iso]
            hive-mcp.isolation-methods
            [hive-mcp.dns.result :as dns]))

;; =============================================================================
;; Test State
;; =============================================================================

(def ^:dynamic *shouts* nil)

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn- recording-channel
  "An IDeliveryChannel that appends every delivered event to `sink`.

   Registered on the delivery port, so the shout reaches it the way it
   reaches a real reader: through hive-mcp.delivery.fanout, with the same
   envelope wrapping and the same fault isolation. Always available."
  [sink]
  (reify dc/IDeliveryChannel
    (channel-id [_] :sync-test-recorder)
    (available? [_] true)
    (deliver! [_ event] (swap! sink conj event) nil)))

(defn shouts-capture-fixture
  "Bind *shouts* to a fresh atom and route the process's delivery fanout
   into it through a recording IDeliveryChannel.

   Only the test's own channel is registered: any channel another namespace
   left behind would otherwise deliver at random into the same process. The
   previous registry is restored afterwards."
  [f]
  (binding [*shouts* (atom [])]
    (let [previous (dc/get-channels)]
      (dc/clear-channels!)
      (dc/register-channel! (recording-channel *shouts*))
      (try (f)
           (finally
             (dc/clear-channels!)
             (doseq [ch previous] (dns/rescue nil (dc/register-channel! ch))))))))

(use-fixtures :each
  (iso/with-isolations :swarm-ds)
  shouts-capture-fixture)

;; =============================================================================
;; Layer 4: Synthetic Shout Tests
;; =============================================================================

(defn- layer4-shout
  "The synthetic shout emitted by Layer 4, wherever it sits in the capture.

   A shared JVM can deliver foreign shouts into the same process (another
   namespace's registered channel, a hook left on the registry), so the
   layer4 shout is identified by its own :source marker rather than by
   being first."
  [shouts]
  (some #(when (= "layer4-synthetic" (get-in % [:data :source])) %) shouts))

(deftest layer4-synthetic-shout-on-task-complete-test
  (testing "Task completion triggers synthetic shout (Layer 4 guarantee)"
    ;; Setup: Add a slave and task to DataScript
    (ds/add-slave! "test-ling-1" {:status :working :name "test" :depth 1})
    (ds/add-task! "task-123" "test-ling-1" {:status :dispatched :files []})

    ;; Ensure hooks registry exists via injection
    (when-not (sync/get-hooks-registry)
      (sync/set-hooks-registry! (hooks/create-registry)))

    ;; Simulate task completion event (as would come from channel)
    (#'sync/handle-task-completed {:task-id "task-123"
                                   :slave-id "test-ling-1"})

    ;; Verify: Synthetic shout was emitted
    (let [shout (layer4-shout @*shouts*)]
      (is (some? shout) "A layer4-synthetic shout should be emitted on task completion")
      (when shout
        (is (= "test-ling-1" (:agent-id shout))
            "Shout should be from the completing ling")
        (is (= :completed (:event-type shout))
            "Shout event type should be :completed")))))

(deftest layer4-shout-is-found-beside-a-foreign-shout-test
  (testing "A shout another namespace left in the JVM does not shift the layer4 one out of view"
    (ds/add-slave! "noisy-ling" {:status :working :name "noisy" :depth 1})
    (ds/add-task! "task-noisy" "noisy-ling" {:status :dispatched :files []})

    ;; Stand in for another namespace sharing this JVM: a foreign shout is
    ;; already FIRST in the capture when the completion is handled, which is
    ;; what a hook or channel left behind by another namespace produces.
    (swap! *shouts* #(into [{:agent-id "someone-else"
                             :event-type :progress
                             :data {:source "some-other-ns"}}]
                           %))

    (when-not (sync/get-hooks-registry)
      (sync/set-hooks-registry! (hooks/create-registry)))

    (#'sync/handle-task-completed {:task-id "task-noisy"
                                   :slave-id "noisy-ling"})

    (let [shout (layer4-shout @*shouts*)]
      (is (= 2 (count @*shouts*)) "the foreign shout is still in the capture")
      (is (= "someone-else" (:agent-id (first @*shouts*)))
          "the foreign shout is still first, so position alone is not enough")
      (is (some? shout) "the layer4 shout is found by its marker, not by position")
      (when shout
        (is (= :completed (:event-type shout)))))))

(deftest layer4-shout-includes-task-id-test
  (testing "Synthetic shout includes task-id for traceability"
    (ds/add-slave! "ling-abc" {:status :working :name "abc" :depth 1})
    (ds/add-task! "task-xyz-789" "ling-abc" {:status :dispatched :files []})

    (when-not (sync/get-hooks-registry)
      (sync/set-hooks-registry! (hooks/create-registry)))

    (#'sync/handle-task-completed {:task-id "task-xyz-789"
                                   :slave-id "ling-abc"})

    (let [shout (layer4-shout @*shouts*)]
      (is (some? shout) "the layer4 shout is emitted with a task id")
      (when shout
        (is (= "task-xyz-789" (get-in shout [:data :task-id]))
            "Shout data should include task-id")))))

(deftest layer4-works-without-registry-test
  (testing "Handle task completion gracefully when no registry"
    ;; This ensures the architectural guarantee doesn't crash
    ;; even if hooks system isn't fully initialized
    (ds/add-slave! "orphan-ling" {:status :working :name "orphan" :depth 1})
    (ds/add-task! "orphan-task" "orphan-ling" {:status :dispatched :files []})

    ;; Temporarily nil out registry via injection
    (let [orig-registry (sync/get-hooks-registry)]
      (sync/set-hooks-registry! nil)

      (try
        ;; Should not throw
        (#'sync/handle-task-completed {:task-id "orphan-task"
                                       :slave-id "orphan-ling"})
        ;; Verify task was still completed in DataScript
        (is (= :completed (:task/status (ds/get-task "orphan-task")))
            "Task should be marked complete even without hooks")
        (finally
          (sync/set-hooks-registry! orig-registry))))))

;; =============================================================================
;; Integration with DataScript
;; =============================================================================

(deftest task-completion-updates-datascript-test
  (testing "Handle-task-completed updates DataScript status"
    (ds/add-slave! "ds-ling" {:status :working :name "ds" :depth 1})
    (ds/add-task! "ds-task-1" "ds-ling" {:status :dispatched :files ["/a.clj"]})
    ;; claim-file! takes an opts map (not a bare task-id string). Passing a
    ;; string silently destructures to {:task-id nil}, so the claim is created
    ;; with no :claim/task ref and release-claims-for-task! can't find it.
    (ds/claim-file! "/a.clj" "ds-ling" {:task-id "ds-task-1"})

    (when-not (sync/get-hooks-registry)
      (sync/set-hooks-registry! (hooks/create-registry)))

    (#'sync/handle-task-completed {:task-id "ds-task-1"
                                   :slave-id "ds-ling"})

    (is (= :completed (:task/status (ds/get-task "ds-task-1")))
        "Task status should be :completed")

    ;; File claim should be released
    (is (nil? (ds/get-claims-for-file "/a.clj"))
        "File claim should be released after task completion")))

(comment
  ;; Run tests from REPL
  (run-tests 'hive-mcp.swarm.sync-test))
