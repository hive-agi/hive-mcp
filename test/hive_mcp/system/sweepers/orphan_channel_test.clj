(ns hive-mcp.system.sweepers.orphan-channel-test
  "The sweep must never act on an owner it cannot name.

   `unregister-resource-owner!` removes BY ID. Releasing an owner's resources
   and then unregistering nil removes nothing, so the owner stays registered,
   the next sweep finds the same dead owner, and it is released again every
   five minutes forever. Nothing throws and nothing logs the owner, because
   the id the log line carries is the nil."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.system.registry :as reg]
            [hive-mcp.system.sweepers.orphan-channel :as sweeper]
            [hive-spi.lifecycle.ports :as ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- ->owner
  "An IResourceOwner whose `owner-id` answers ID until `poisoned?` is flipped,
   and which records its releases in `released`.

   Poisoned AFTER registration on purpose. `register-resource-owner!` keys the
   registry by (owner-id impl), so an owner that always threw could never be
   registered at all. The reachable case is the one this sweeper exists for: a
   LING THAT DIED, whose owner-id reads through the thing that just went away."
  [id released poisoned?]
  (reify ports/IResourceOwner
    (owner-id [_]
      (if @poisoned?
        (throw (ex-info "owner-id: underlying ling is gone" {:id id}))
        id))
    (owned-resources [_] [])
    (release-all! [_] (swap! released conj id) nil)))

(def ^:private registered (atom #{}))

(defn- register! [owner]
  (swap! registered conj (ports/owner-id owner))
  (reg/register-resource-owner! owner))

(defn- clean-registry [f]
  (reset! registered #{})
  (try
    (f)
    (finally
      ;; Only ids this namespace registered. The registry is process-global and
      ;; sibling suites hold their own owners (axiom 20260629165653-461fcd11).
      (doseq [id @registered]
        (reg/unregister-resource-owner! id))
      (reset! registered #{}))))

(use-fixtures :each clean-registry)

(defn- sweep-with
  "Run one sweep, choosing who is alive. `alive` is the set of ids find-ling
   should resolve; everything else is orphaned."
  [alive]
  (ports/sweep! (sweeper/->OrphanChannelSweep (fn [id] (contains? alive id)))
                {}))

(defn- still-registered? [id]
  (some? (reg/get-resource-owner id)))

;; =============================================================================
;; The ratchet
;; =============================================================================

(deftest an-owner-that-cannot-be-named-is-not-released
  (testing "releasing it would strand it registered, so it is skipped and reported"
    (let [released (atom [])
          poisoned (atom false)
          owner    (->owner "ling-poison" released poisoned)]
      (register! owner)
      (reset! poisoned true)
      (let [{:keys [swept errors]} (sweep-with #{})]
        (is (empty? @released)
            "resources were released for an owner that cannot then be unregistered")
        (is (zero? swept)
            "an owner whose id could not be read is not a swept owner")
        (is (= 1 (count errors))
            "the failure must be reported, not swallowed into a nil id")
        (is (nil? (:owner-id (first errors))))))))

(deftest a-dead-owner-is-released-and-unregistered-by-its-own-id
  (testing "the ordinary path still works"
    (let [released (atom [])
          owner    (->owner "ling-dead" released (atom false))]
      (register! owner)
      (let [{:keys [swept errors]} (sweep-with #{})]
        (is (= ["ling-dead"] @released))
        (is (= 1 swept))
        (is (empty? errors))
        (is (not (still-registered? "ling-dead"))
            "unregister must be keyed by the owner's real id")))))

(deftest a-live-owner-is-left-alone
  (testing "find-ling resolving the id means the owner is not orphaned"
    (let [released (atom [])
          owner    (->owner "ling-alive" released (atom false))]
      (register! owner)
      (let [{:keys [swept errors]} (sweep-with #{"ling-alive"})]
        (is (empty? @released))
        (is (zero? swept))
        (is (empty? errors))
        (is (still-registered? "ling-alive"))))))

(deftest one-unnameable-owner-does-not-stop-the-sweep
  (testing "identify, decide, act are separate passes so one bad owner is not fatal"
    (let [released (atom [])
          poisoned (atom false)
          bad      (->owner "ling-poison" released poisoned)
          good     (->owner "ling-dead" released (atom false))]
      (register! bad)
      (register! good)
      (reset! poisoned true)
      (let [{:keys [swept errors]} (sweep-with #{})]
        (is (= ["ling-dead"] @released))
        (is (= 1 swept))
        (is (= 1 (count errors)))
        (is (not (still-registered? "ling-dead")))))))

(deftest no-find-ling-means-nothing-is-orphaned
  (testing "conservative default: an unresolvable probe must not orphan anyone"
    (let [released (atom [])
          owner    (->owner "ling-unknown" released (atom false))]
      (register! owner)
      (let [{:keys [swept]} (ports/sweep! (sweeper/->OrphanChannelSweep nil) {})]
        ;; find-ling-fn nil falls back to resolve-find-ling, which is nil when
        ;; the spawn ns is not loaded. Either way this owner must survive.
        (is (empty? @released))
        (is (zero? swept))))))
