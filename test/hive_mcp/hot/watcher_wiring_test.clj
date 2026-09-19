(ns hive-mcp.hot.watcher-wiring-test
  "Does the watcher actually RECEIVE the protocol interlock?

   hive-mcp.hot.self-test proves the set is derived correctly. That is a
   different claim from the set reaching hive-hot, and the gap between those
   two claims is exactly where this feature was broken for its whole life:
   `init-with-watcher!` accepted `:no-reload` all along and nobody passed it.
   A test of the derivation alone would have stayed green throughout.

   Contained on purpose. It captures the options rather than starting a real
   watcher, so hive-hot's global registry and clj-reload's baseline are left
   alone and this namespace cannot perturb another suite in the same JVM."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-hot.core :as hot]
            [hive-mcp.protocols.dispatch]
            [hive-mcp.server.init :as init]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- captured-watcher-opts
  "Run the real `init-hot-reload-watcher!` with the boundary stubbed, and
   return whatever it handed to `init-with-watcher!`.

   Returns ::never-called when the boundary was not reached at all, which is a
   state the caller MUST check: the function under test wraps its whole body
   in `result/rescue`, so any exception on the way would swallow itself and
   leave a nil-punning assertion looking green."
  [project-config]
  (let [captured (atom ::never-called)]
    (with-redefs [hot/init-with-watcher! (fn [opts] (reset! captured opts) nil)]
      (init/init-hot-reload-watcher! (atom nil) project-config))
    @captured))

(deftest the-watcher-is-handed-the-protocol-interlock
  (let [opts (captured-watcher-opts {:hot-reload true})]
    (testing "vacuity guard: the body is rescue-wrapped, so silence is possible"
      (is (not= ::never-called opts)
          "init-with-watcher! was never reached, so nothing below was actually checked"))
    (let [{:keys [dirs no-reload]} opts]
      (testing "it still watches a source tree"
        (is (seq dirs)))
      (testing "and it now carries the interlock, which is the whole fix"
        (is (some? no-reload)
            "no :no-reload means a reload of a protocol namespace orphans every instance built against it")
        (is (pos? (count no-reload))
            "an empty set is the same as no interlock, and passes a some? check")
        (is (contains? no-reload 'hive-mcp.protocols.dispatch)
            "a real core protocol namespace must be in the protected set")))))

(deftest a-disabled-watcher-starts-nothing
  (testing "the off switch still works, interlock or not"
    (is (= ::never-called (captured-watcher-opts {:hot-reload false}))
        "hot-reload false must not reach init-with-watcher! at all")))
