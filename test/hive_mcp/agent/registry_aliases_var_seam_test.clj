(ns hive-mcp.agent.registry-aliases-var-seam-test
  "The registry compat shims resolve their FUNCTIONS through the hive-spi var.

   Card 20261116190025-31139c15 ([CBV-hive-mcp] registry aliases A, epic
   20260821211613-0c0e618f) counted 44 `(def f spi/f)` aliases across four
   shims. Each captured the owner's fn VALUE at load time, so a reload or redef
   of the hive-spi var was dead to every caller that spells the hive-mcp name:
   the mechanism behind restarting hive-mcp to make a fix take effect.

   Two layers, because either alone is escapable:

     1. CENSUS: no public var of a shim holds the very fn object its owner var
        holds. Derived from the namespaces, not a hand-kept list, so an alias
        added later fails here. Data aliases (maps, sets, protocols) are left
        out on purpose: they are values, and nothing about them reloads.

     2. BEHAVIOUR: rebind the owner var after the shim is loaded and call the
        shim; the call must reach the rebound fn. This is what would still
        fail if the census were satisfied by a wrapper that deref'd early."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.addons.headless-caps]
            [hive-mcp.agent.spawn-mode-registry]
            [hive-mcp.agent.type-registry]
            [hive-mcp.server.guards]
            [hive-spi.addon.headless-caps]
            [hive-spi.swarm.agent-types]
            [hive-spi.swarm.guards]
            [hive-spi.swarm.spawn-modes]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private shim->owner
  '{hive-mcp.agent.type-registry       hive-spi.swarm.agent-types
    hive-mcp.agent.spawn-mode-registry hive-spi.swarm.spawn-modes
    hive-mcp.server.guards             hive-spi.swarm.guards
    hive-mcp.addons.headless-caps      hive-spi.addon.headless-caps})

(defn- fn-reexports
  "[shim-var owner-var] for every public shim var whose owner namesake holds a fn."
  []
  (for [[shim owner] shim->owner
        [sym shim-var] (ns-publics shim)
        :let [owner-var (ns-resolve owner sym)]
        :when (and (var? owner-var)
                   (= owner (ns-name (:ns (meta owner-var))))
                   (fn? @owner-var)
                   (not (:macro (meta shim-var))))]
    [shim-var owner-var]))

(deftest the-census-is-not-vacuous
  (testing "every shim loaded and the 44 function re-exports are all seen"
    (is (= 44 (count (fn-reexports))))
    (is (= {'hive-mcp.agent.type-registry 12 'hive-mcp.agent.spawn-mode-registry 11
            'hive-mcp.server.guards 12 'hive-mcp.addons.headless-caps 9}
           (frequencies (map (comp ns-name :ns meta first) (fn-reexports)))))))

(deftest no-shim-holds-its-owners-fn-value
  (let [captured (for [[shim-var owner-var] (fn-reexports)
                       :when (identical? @shim-var @owner-var)]
                   (symbol shim-var))]
    (is (empty? captured)
        (str "captured by value, a redef of the owner is invisible through: "
             (pr-str (sort captured))))))

(defn- seen-through-shim?
  "Rebind OWNER-VAR to a recorder that still answers like the original, call
   the shim with ARGS, restore. True when the call reached the rebound fn."
  [shim-var owner-var args]
  (let [impl @owner-var
        hits (atom 0)]
    (try
      (alter-var-root owner-var (constantly (fn [& xs] (swap! hits inc) (apply impl xs))))
      (apply @shim-var args)
      (pos? @hits)
      (finally (alter-var-root owner-var (constantly impl))))))

;; Extended, not reified: a call on an instance that implements the protocol
;; INTERFACE takes the compiler's inline fast path and never reads the method
;; var, so it cannot observe a rebinding either way. An `extend`-ed backend
;; dispatches through the method var, which is exactly the path a captured
;; value froze (Clojure re-roots the method vars on every `extend`).
(defrecord ^:private StubBackend [])

(extend-protocol hive-spi.addon.headless-caps/IHookable StubBackend
  (register-hooks! [_ _ _] {:registered? true :hook-count 0})
  (active-hooks [_ _] nil))
(extend-protocol hive-spi.addon.headless-caps/ICheckpointable StubBackend
  (checkpoint! [_ _] {:checkpoint-id "c" :created-at 0})
  (rewind! [_ _ id] {:rewound? true :checkpoint-id id}))
(extend-protocol hive-spi.addon.headless-caps/ISubagentHost StubBackend
  (register-subagents! [_ _ _] {:registered? true :agent-count 0})
  (list-subagents [_ _] nil))
(extend-protocol hive-spi.addon.headless-caps/IBudgetGuardable StubBackend
  (set-budget! [_ _ usd] {:budget-set? true :max-usd usd})
  (budget-status [_ _] nil))

(def ^:private stub-backend (->StubBackend))

(def ^:private sample-args
  "One harmless call per re-export. A name missing here fails the test below,
   so a new re-export cannot slip past the behavioural layer."
  {'valid-type? [:ling] 'type-depth [:ling] 'depth->agent-type [0]
   'spawnable? [:ling] 'valid-spawn-mode? [:ling :claude]
   'has-capability? [:ling :x] 'has-permission? [:ling :x]
   'can-chain-tools? [:ling] 'slot-limit [:ling] 'default-model-tier [:ling]
   'mcp-enum [] 'describe [:ling]
   'registry [] 'valid-mode? [:claude] 'resolve-alias [:claude]
   'requires-emacs? [:claude] 'io-model [:claude] 'capabilities [:claude]
   'register-mode! [:cbv-seam-probe {}] 'deregister-mode! [:cbv-seam-probe]
   'child-ling? [] 'coordinator? [] 'get-role [] 'ling-depth []
   'child-ling-env [] 'guard-status [] 'coordinator-running? []
   'set-enforcement-mode! [nil] 'enable-guards! [] 'disable-guards! []
   'mark-coordinator-running! [] 'mark-coordinator-stopped! []
   'register-hooks! [stub-backend "l" {}] 'active-hooks [stub-backend "l"]
   'checkpoint! [stub-backend "l"] 'rewind! [stub-backend "l" "c"]
   'register-subagents! [stub-backend "l" {}] 'list-subagents [stub-backend "l"]
   'set-budget! [stub-backend "l" 1.0] 'budget-status [stub-backend "l"]
   'provided-capabilities [stub-backend]})

(def ^:private overrides
  "Calls whose argument list depends on the shim, not just the name."
  {'hive-mcp.agent.spawn-mode-registry {'slot-limit [:claude] 'has-capability? [:claude :x]}})

(defn- args-for [shim-var]
  (let [sym  (:name (meta shim-var))
        shim (ns-name (:ns (meta shim-var)))]
    (get-in overrides [shim sym] (get sample-args sym ::missing))))

(deftest a-redef-of-the-owner-is-seen-through-every-shim
  (let [running? (hive-spi.swarm.guards/coordinator-running?)
        enabled? hive-spi.swarm.guards/*guard-enabled?*
        mode     hive-spi.swarm.guards/*enforcement-mode*]
    (try
      (doseq [[shim-var owner-var] (fn-reexports)
              :let [args (args-for shim-var)]]
        (is (not= ::missing args) (str "no sample call for " (symbol shim-var)))
        (when (not= ::missing args)
          (is (seen-through-shim? shim-var owner-var args)
              (str (symbol shim-var) " did not reach a rebound " (symbol owner-var)))))
      (finally
        (hive-spi.swarm.spawn-modes/deregister-mode! :cbv-seam-probe)
        (if running?
          (hive-spi.swarm.guards/mark-coordinator-running!)
          (hive-spi.swarm.guards/mark-coordinator-stopped!))
        (alter-var-root #'hive-spi.swarm.guards/*guard-enabled?* (constantly enabled?))
        (alter-var-root #'hive-spi.swarm.guards/*enforcement-mode* (constantly mode))))))

(deftest control-a-def-alias-is-blind-to-the-redef
  (testing "the pre-fix shape, so the behavioural test above measures something"
    (let [owner-var #'hive-spi.swarm.agent-types/valid-type?
          impl      @owner-var
          frozen    impl]
      (try
        (alter-var-root owner-var (constantly (fn [_] :rebound)))
        (is (not= :rebound (frozen :ling)) "a captured value keeps the old fn")
        (is (= :rebound (hive-mcp.agent.type-registry/valid-type? :ling))
            "the delegating defn reaches the rebound var")
        (finally (alter-var-root owner-var (constantly impl)))))))
