(ns hive-mcp.tools.hive-project-vessel-test
  "Tests for get-projectile-info driving the :project/info op through the
   :vessel :dispatch capability (stub, per commit b4297275's stub pattern).

   get-projectile-info is private: invoked via its var.

   Contract: {:success true :result \"<json>\"} => parsed map; {:success false}
   or no vessel registered (unavailable envelope) => nil."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [hive-spi.editor.services :as svc]
            [hive-mcp.tools.hive-project :as hive-project]
            [hive-mcp.test.stub.swarm-host :as sh]))

(defn- project-info-host
  "A vessel stub answering :project/info with ENVELOPE, recording the ops."
  [envelope]
  (sh/answering {:project/info envelope}))

(deftest get-projectile-info-dispatches-project-info-op
  (testing "get-projectile-info emits {:op :project/info :directory dir} via :vessel :dispatch"
    (sh/with-swarm-host
      [host (project-info-host
             {:success true
              :result (json/write-str {:name "hive" :root "/tmp/hive" :type "clojure-cli"})
              :timed-out false})]
      (let [info (#'hive-project/get-projectile-info "/tmp/hive")]
        (is (= {:name "hive" :root "/tmp/hive" :type "clojure-cli"} info))
        (is (= [[{:op :project/info :directory "/tmp/hive"} 5000]]
               (sh/calls-of host :project/info))
            "exact op map (directory bound) and timeout")))))

(deftest get-projectile-info-nil-directory-op
  (testing "no directory => the op carries no :directory key"
    (sh/with-swarm-host
      [host (project-info-host
             {:success true
              :result (json/write-str {:name "hive" :root "/tmp/hive" :type "clojure-cli"})
              :timed-out false})]
      (is (some? (#'hive-project/get-projectile-info nil)))
      (is (= [[{:op :project/info} 5000]] (sh/calls-of host :project/info))
          "op map is exactly {:op :project/info}"))))

(deftest get-projectile-info-failure-envelope-nil
  (testing "{:success false} => nil"
    (sh/with-swarm-host
      [host (project-info-host {:success false :result nil :error :boom :timed-out false})]
      (is (nil? (#'hive-project/get-projectile-info "/tmp/hive")))
      (is (= 1 (count (sh/calls-of host :project/info)))
          "the op was dispatched exactly once"))))

(deftest get-projectile-info-no-vessel-registered
  (testing "with no :vessel capability registered, get-projectile-info is nil"
    (let [prior (get (svc/registered) sh/registry-key)]
      (svc/unregister-services! sh/registry-key)
      (try
        (is (nil? (#'hive-project/get-projectile-info "/tmp/hive"))
            "unavailable envelope => nil")
        (finally
          (svc/unregister-services! sh/registry-key)
          (when (seq prior)
            (svc/register-services! sh/registry-key prior)))))))
