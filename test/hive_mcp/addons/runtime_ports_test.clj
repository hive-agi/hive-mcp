(ns hive-mcp.addons.runtime-ports-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.addons.runtime-ports :as runtime-ports]))

(def expected-port-keys
  #{:tools/invoke
    :workflow/engine
    :host/shared-jvm?
    :memory/store
    :embedding/embed-batch
    :embedding/provider
    :embedding/configured?
    :kg/register-schema!
    :kg/infer-scope
    :kg/resolve-project-id
    :kg/query
    :extension/get
    :extension/keys
    :extension/register!
    :extension/contribute-commands!
    :extension/retract-contributions!})

(deftest composition-root-exposes-callable-adapters
  (let [ports (runtime-ports/runtime-ports)]
    (is (= expected-port-keys (set (keys ports))))
    (is (every? fn? (vals ports)))))

(deftest the-host-answers-whether-this-jvm-is-shared-by-every-agent
  (testing "the port answers, whatever this JVM is"
    ;; Not (false? ...): another test in the same suite run may have started a
    ;; system, and then `true` is the correct answer. The injected cases below
    ;; pin the meaning without depending on suite order.
    (is (boolean? ((:host/shared-jvm? (runtime-ports/runtime-ports))))))
  (testing "no system up: a dev, test or worker JVM, so an addon may run code here"
    (is (false? (runtime-ports/shared-jvm? (constantly nil))) "server ns not loaded")
    (is (false? (runtime-ports/shared-jvm? (constantly (atom nil)))) "system halted"))
  (testing "a running system means this process serves every agent"
    (is (true? (runtime-ports/shared-jvm? (constantly (atom {:some/component :up}))))))
  (testing "the probe never loads the server namespace just to answer"
    (let [loaded-before (some? (find-ns 'hive-mcp.server.core))]
      (runtime-ports/shared-jvm?)
      (is (= loaded-before (some? (find-ns 'hive-mcp.server.core)))
          "a test or worker JVM must not load the server (and its stores) to be told it is not the server"))))
