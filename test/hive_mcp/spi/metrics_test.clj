;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.spi.metrics-test
  "The metrics port has three states and the kernel cannot tell them apart by
   control flow: an installed implementation, a late-bound host namespace, and
   nothing at all. Only the first two record anything.

   `soft/*resolve*` is bound instead of redefining a host var, so these cases
   also describe a classpath the telemetry namespace has already left (HIVE-KERNEL
   E2)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.spi.metrics :as port]
            [hive-mcp.swarm.adapters.soft :as soft]))

(defn- clean-registry [f]
  (port/uninstall!)
  (try (f) (finally (port/uninstall!))))

(use-fixtures :each clean-registry)

(defn- recording-impl [seen]
  (reify port/IMetrics
    (-inc-events! [_ type severity] (swap! seen conj [:inc-events type severity]) nil)
    (-observe-request-duration! [_ tool seconds] (swap! seen conj [:duration tool seconds]) nil)
    (-set-lings-active! [_ n] (swap! seen conj [:lings n]) nil)
    (-handle-effect! [_ data] (swap! seen conj [:effect data]) nil)))

(defn- stub-resolver
  "Stands in for `requiring-resolve`: every telemetry symbol resolves to a fn
   that records its name and args."
  [seen]
  (fn [sym]
    (when (= "hive-mcp.telemetry.prometheus" (namespace sym))
      (fn [& args] (swap! seen conj (vec (cons (symbol (name sym)) args))) nil))))

(deftest an-absent-telemetry-namespace-is-a-silent-noop
  (binding [soft/*resolve* (constantly nil)]
    (port/reset-cache!)
    (testing "every call answers nil and none throws"
      (is (nil? (port/inc-events! :some/event :info)))
      (is (nil? (port/observe-request-duration! "tool" 0.5)))
      (is (nil? (port/set-lings-active! 3)))
      (is (nil? (port/handle-effect! {:counter :wave_failure}))))))

(deftest the-port-late-binds-to-the-host-namespace-by-symbol
  (let [seen (atom [])]
    (binding [soft/*resolve* (stub-resolver seen)]
      (port/reset-cache!)
      (port/inc-events! :some/event :info)
      (port/observe-request-duration! "event-dispatch-x" 1.5)
      (port/set-lings-active! 7)
      (port/handle-effect! {:counter :wave_failure})
      (is (= '[[inc-events-total! :some/event :info]
               [observe-request-duration! "event-dispatch-x" 1.5]
               [set-lings-active! 7]
               [handle-prometheus-effect! {:counter :wave_failure}]]
             @seen)
          "the kernel's four calls reach the four host fns, arguments unchanged"))))

(deftest an-installed-implementation-wins-over-the-host-namespace
  (let [host (atom [])
        impl (atom [])]
    (binding [soft/*resolve* (stub-resolver host)]
      (port/reset-cache!)
      (port/install! (recording-impl impl))
      (port/set-lings-active! 2)
      (is (= [[:lings 2]] @impl))
      (is (= [] @host) "nothing reached the host namespace while an impl was installed")
      (port/uninstall!)
      (port/set-lings-active! 4)
      (is (= '[[set-lings-active! 4]] @host)
          "uninstall! falls back to the late-bound host fn"))))

(deftest install-clears-the-resolution-cache
  (let [host (atom [])]
    (binding [soft/*resolve* (constantly nil)]
      (port/reset-cache!)
      (port/set-lings-active! 1))
    (binding [soft/*resolve* (stub-resolver host)]
      (testing "without clearing, the cached miss would keep answering noop"
        (port/set-lings-active! 2)
        (is (= [] @host)))
      (port/install! (recording-impl (atom [])))
      (port/uninstall!)
      (port/set-lings-active! 3)
      (is (= '[[set-lings-active! 3]] @host)
          "install!/uninstall! cleared the cache, so the namespace is seen again"))))
