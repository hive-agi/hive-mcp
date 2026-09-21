(ns hive-mcp.tools.consolidated.workflow.readiness-vessel-test
  "Tests for vterm-ready? driving the :swarm/slave-ready? op through the
   :vessel :dispatch capability (stub, per commit b4297275's stub pattern).

   Contract: {:success true :result \"t\"} => ready (true); any other
   envelope (result nil, \"nil\", success false) => not ready (false);
   no vessel registered => the unavailable envelope answers => false."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-spi.editor.services :as svc]
            [hive-mcp.tools.consolidated.workflow.readiness :as readiness]
            [hive-mcp.test.stub.swarm-host :as sh]))

(deftest vterm-ready-dispatches-slave-ready-op
  (testing "vterm-ready? emits {:op :swarm/slave-ready? :slave-id ...} via :vessel :dispatch"
    (sh/with-swarm-host
      [host (sh/answering {:swarm/slave-ready? {:success true :result "t" :timed-out false}})]
      (is (true? (readiness/vterm-ready? "ling-42")))
      (is (= [[{:op :swarm/slave-ready? :slave-id "ling-42"} 2000]]
             (sh/calls-of host :swarm/slave-ready?))
          "exact op map and timeout"))))

(deftest vterm-ready-not-ready-envelopes
  (testing "any envelope other than {:success true :result \"t\"} reads not-ready"
    (doseq [envelope [{:success true :result "nil" :timed-out false}
                      {:success true :result nil :timed-out false}
                      {:success false :result nil :error :boom}
                      {:success false :result nil :timed-out true}]]
      (sh/with-swarm-host
        [_host (sh/answering {:swarm/slave-ready? envelope})]
        (is (false? (readiness/vterm-ready? "ling-42"))
            (str "not ready for envelope " (pr-str envelope)))))))

(deftest vterm-ready-no-vessel-registered
  (testing "with no :vessel capability registered, vterm-ready? is false"
    (let [prior (get (svc/registered) sh/registry-key)]
      (svc/unregister-services! sh/registry-key)
      (try
        (is (false? (readiness/vterm-ready? "ling-42"))
            "unavailable envelope => not ready")
        (finally
          (svc/unregister-services! sh/registry-key)
          (when (seq prior)
            (svc/register-services! sh/registry-key prior)))))))
