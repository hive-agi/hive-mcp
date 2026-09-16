(ns hive-mcp.tools.register-gate-test
  "The second increment of kanban 20260916134011-1246379c.

   Three more registration sites had a `defonce`'d gate that skipped the
   body on every call after the first. Because the gate itself is a
   `defonce`, a hot reload left it reading true, the body never re-ran, and
   the registry kept dispatching into the closure compiled BEFORE the
   reload. \"Idempotent\" had come to mean \"refuses to re-register\".

   A gate is correct only where registration ACCUMULATES. Every registry
   reached from here is key-addressed and last-writer-wins — `reg-fx`,
   `reg-cofx`, `reg-event-fx`, `reg-event` — so re-registering is free and
   is the whole repair. `hive-mcp.crystal.hooks/register-hooks!` was on the
   same list and is deliberately NOT changed: `hooks.core/register-hook`
   does `(swap! registry update event conj handler)`, which appends, so its
   gate is the correct kind and re-running there would duplicate the hook."
  (:require [clojure.test :refer [deftest is testing]]
            [hive.events :as ev]
            [hive-mcp.tools.kanban.events :as kev]
            [hive-mcp.tools.migrate.kanban.events :as mev]
            [hive-mcp.tools.session-complete :as sc]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- private-fn
  "Deref a var by namespace and name, private or not — the registered
   handler has to be compared against what the namespace defines NOW."
  [ns-sym sym]
  (some-> (find-ns ns-sym) (ns-resolve sym) deref))

;; =============================================================================
;; tools.kanban.events/init!
;; =============================================================================

(deftest kanban-events-init-re-registers
  (testing "every call registers, so a reload can rewire the handlers"
    (is (true? (kev/init!)) "first call registers")
    (is (true? (kev/init!)) "and so does every call after it"))

  (testing "the registry holds what the namespace defines NOW"
    (kev/init!)
    (doseq [[event-id handler-sym] {:kanban/move  'move-fx
                                    :kanban/retag 'retag-fx
                                    :kanban/edit  'edit-fx}]
      (is (identical? (private-fn 'hive-mcp.tools.kanban.events handler-sym)
                      (:handler (ev/get-event event-id)))
          (str "a gate would leave the pre-reload closure registered for "
               event-id))))

  (testing "the leaf registries it fans out to are reached too"
    (kev/init!)
    (is (identical? (private-fn 'hive-mcp.tools.kanban.effects 'track-movement!)
                    (ev/get-fx :kanban/track-movement))
        "effects/register-all! is key-addressed and must re-run")))

;; =============================================================================
;; tools.migrate.kanban.events/init!
;; =============================================================================

(deftest migrate-kanban-events-init-re-registers
  (testing "the :ok contract is unchanged, on the first call and after it"
    (is (= :ok (mev/init!)))
    (is (= :ok (mev/init!))))

  (testing "the named fx is re-pointed at the current var"
    (mev/init!)
    (is (identical? (private-fn 'hive-mcp.tools.migrate.kanban.events
                                'log-progress!)
                    (ev/get-fx :kanban-mig/log))))

  (testing "the anonymous observers are genuinely re-installed"
    ;; Every handler this namespace registers is an anonymous closure, so
    ;; there is no var to compare against — which is also why the staleness
    ;; scan cannot see them. What CAN be observed is that a second init!
    ;; replaces the closure rather than leaving the first one in place.
    (mev/init!)
    (let [before (:handler (ev/get-event :kanban-mig/run-done))]
      (mev/init!)
      (let [after (:handler (ev/get-event :kanban-mig/run-done))]
        (is (some? before))
        (is (some? after))
        (is (not (identical? before after))
            "a gated init! would hand back the identical pre-reload closure")))))

;; =============================================================================
;; tools.session-complete/register-handler!
;; =============================================================================

(deftest session-complete-register-handler-re-registers
  (testing "every call registers and returns true"
    (is (true? (sc/register-handler!)))
    (is (true? (sc/register-handler!))))

  (testing ":ling/session-complete dispatches into the current var"
    (sc/register-handler!)
    (is (identical? (private-fn 'hive-mcp.tools.session-complete
                                'handle-ling-session-complete)
                    (:handler (ev/get-event :ling/session-complete)))
        "a gate here would pin the closure compiled before the reload")))

;; =============================================================================
;; crystal.hooks — the site that keeps its gate, and why
;; =============================================================================

(deftest register-hook-appends-so-crystal-hooks-keeps-its-gate
  (testing "hooks.core/register-hook accumulates per event key"
    ;; This is the measurement that decided crystal.hooks/register-hooks!
    ;; stays gated. If register-hook ever becomes key-addressed, this test
    ;; fails and the gate there has to be revisited with it.
    (let [registry (atom {})
          handler-a (fn [_] :a)
          handler-b (fn [_] :b)
          register  (requiring-resolve 'hive-mcp.hooks.core/register-hook)]
      (register registry :session-end handler-a)
      (register registry :session-end handler-b)
      (is (= 2 (count (get @registry :session-end)))
          "registering twice must ACCUMULATE, which is what makes a gate correct"))))
