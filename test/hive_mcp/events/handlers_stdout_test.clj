(ns hive-mcp.events.handlers-stdout-test
  "register-handlers! runs at boot, before the stdio transport owns stdout.
   Its notices go through the hive-spi log port, never onto *out*
   (kanban 20260930224940-31ead2d9). The port is observed with an installed
   capture logger; nothing is redefined."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [hive-mcp.events.handlers :as handlers]
            [hive-mcp.events.registry :as registry]
            [hive-spi.log.ports :as log-ports]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- capture-logger [sink]
  (reify log-ports/ILogger
    (log-event [_ level message] (swap! sink conj [level message]) nil)
    (logger-levels [_] #{:debug :info :warn :error})))

(defn- logged
  "Run f with a capture logger installed. Returns [stdout [[level msg] ...]]."
  [f]
  (let [sink (atom [])]
    (log-ports/set-logger! (capture-logger sink))
    (try
      ;; timbre (what the sibling handler modules log through) is silenced:
      ;; the test JVM keeps its *out* appender, production routes it to stderr
      [(with-out-str (log/with-min-level :report (f))) @sink]
      (finally (log-ports/clear-logger!)))))

(use-fixtures :each (fn [t] (try (t) (finally (log-ports/clear-logger!)))))

(defn- line-with [lines needle]
  (some (fn [[level msg]] (when (str/includes? msg needle) [level msg])) lines))

(deftest verify-handlers-notices-go-through-the-log-port
  (testing "missing and extra handler notices reach the port and leave *out* empty"
    (registry/with-clean-registry
      ;; an empty registry: every expected event is missing, and one
      ;; undeclared event is extra
      (registry/reg-event :q2-test/extra [] (fn [_ _] {}))
      (let [[out lines] (logged handlers/verify-handlers!)
            [missing-level missing-msg] (line-with lines "missing handlers")
            [extra-level extra-msg]     (line-with lines "Extra handlers registered")]
        (is (= "" out))
        (is (= :warn missing-level))
        (is (str/includes? missing-msg ":system/error"))
        (is (= :info extra-level))
        (is (str/includes? extra-msg ":q2-test/extra"))))))

(deftest verify-handlers-is-quiet-when-nothing-differs
  (registry/with-clean-registry
    (doseq [e (handlers/expected-events)]
      (registry/reg-event e [] (fn [_ _] {})))
    (let [[out lines] (logged handlers/verify-handlers!)]
      (is (= "" out))
      (is (nil? (line-with lines "missing handlers")))
      (is (nil? (line-with lines "Extra handlers registered"))))))

(deftest register-handlers-first-registration-notice-goes-through-the-log-port
  (testing "the first-registration notice reaches the port and leaves *out* empty"
    (handlers/reset-registration!)
    (let [[out lines] (logged handlers/register-handlers!)
          [level msg] (line-with lines "[hive-events] Handlers registered:")]
      (is (= "" out))
      (is (= :info level))
      (is (str/includes? msg ":lifecycle/sweep")))))
