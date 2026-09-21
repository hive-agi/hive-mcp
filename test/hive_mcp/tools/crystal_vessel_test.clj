(ns hive-mcp.tools.crystal-vessel-test
  "Tests for hive-mcp.tools.crystal driving the :crystal/available? and
   :crystal/wrap-gather ops through the :vessel :dispatch capability
   (stub, per commit ea7523a1's stub pattern).

   The relevant fns are private: invoked via their vars.

   Contract: {:success true :result ...} => parsed/kept; {:success false} or
   no vessel registered (unavailable envelope) => nil."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [hive-spi.editor.services :as svc]
            [hive-mcp.tools.crystal :as crystal]
            [hive-mcp.test.stub.swarm-host :as sh]))

(defn- crystal-host
  "A vessel stub for the crystal ops: :crystal/available? always answers
   loaded t, :crystal/wrap-gather answers ENVELOPE, recording the calls."
  [envelope]
  (sh/answering {:crystal/available? {:success true :result "t" :timed-out false}
                 :crystal/wrap-gather envelope}))

(deftest hive-mcp-el-available-dispatches-available-op
  (testing "hive-mcp-el-available? emits {:op :crystal/available?} via :vessel :dispatch at 5000ms"
    (sh/with-swarm-host
      [host (crystal-host {:success true :result "t" :timed-out false})]
      (is (true? (#'crystal/hive-mcp-el-available?)))
      (is (= [[{:op :crystal/available?} 5000]] (sh/calls-of host :crystal/available?))
          "exact op map and timeout"))))

(deftest hive-mcp-el-available-not-loaded
  (testing "envelope :result nil (featurep false) => false, op still dispatched"
    (sh/with-swarm-host
      [host (sh/answering {:crystal/available? {:success true :result "nil" :timed-out false}})]
      (is (false? (#'crystal/hive-mcp-el-available?)))
      (is (= [[{:op :crystal/available?} 5000]] (sh/calls-of host :crystal/available?))
          "exact op map and timeout"))))

(deftest hive-mcp-el-available-failure-envelope
  (testing "{:success false} => false"
    (sh/with-swarm-host
      [host (sh/answering {:crystal/available? {:success false :result nil :error :boom :timed-out false}})]
      (is (false? (#'crystal/hive-mcp-el-available?))))))

(deftest fetch-elisp-data-dispatches-wrap-gather-op
  (testing "fetch-elisp-data emits {:op :crystal/wrap-gather :directory dir} at 5000ms"
    (sh/with-swarm-host
      [host (crystal-host
             {:success true
              :result (json/write-str {:session "s1" :notes ["n"]})
              :timed-out false})]
      (let [data (#'crystal/fetch-elisp-data "/tmp/hive")]
        (is (= {:session "s1" :notes ["n"]} data))
        (is (= [[{:op :crystal/wrap-gather :directory "/tmp/hive"} 5000]]
               (sh/calls-of host :crystal/wrap-gather))
            "exact op map (directory bound) and timeout")))))

(deftest fetch-elisp-data-nil-directory-op
  (testing "no directory => the op carries no :directory key"
    (sh/with-swarm-host
      [host (crystal-host
             {:success true
              :result (json/write-str {:session "s1"})
              :timed-out false})]
      (is (some? (#'crystal/fetch-elisp-data nil)))
      (is (= [[{:op :crystal/wrap-gather} 5000]] (sh/calls-of host :crystal/wrap-gather))
          "op map is exactly {:op :crystal/wrap-gather}"))))

(deftest fetch-elisp-data-failure-envelope-nil
  (testing "{:success false} => nil"
    (sh/with-swarm-host
      [host (crystal-host {:success false :result nil :error :boom :timed-out false})]
      (is (nil? (#'crystal/fetch-elisp-data "/tmp/hive")))
      (is (= 1 (count (sh/calls-of host :crystal/wrap-gather)))
          "the op was dispatched exactly once"))))

(deftest fetch-elisp-data-unavailable-el
  (testing "with the el addon unavailable (:success false on :crystal/available?),
           fetch-elisp-data never dispatches :crystal/wrap-gather"
    (sh/with-swarm-host
      [host (sh/answering {:crystal/available? {:success false :result nil :error :no-el :timed-out false}
                           :crystal/wrap-gather {:success true :result "{}" :timed-out false}})]
      (is (nil? (#'crystal/fetch-elisp-data "/tmp/hive")))
      (is (= 1 (count (sh/calls-of host :crystal/available?))))
      (is (= 0 (count (sh/calls-of host :crystal/wrap-gather)))
          "wrap-gather gated behind availability"))))

(deftest no-vessel-registered
  (testing "with no :vessel capability registered, both sites see the
           unavailable envelope and read as false / nil"
    (let [prior (get (svc/registered) sh/registry-key)]
      (svc/unregister-services! sh/registry-key)
      (try
        (is (false? (#'crystal/hive-mcp-el-available?))
            "unavailable envelope => false")
        (is (nil? (#'crystal/fetch-elisp-data "/tmp/hive"))
            "unavailable envelope => nil")
        (finally
          (svc/unregister-services! sh/registry-key)
          (when (seq prior)
            (svc/register-services! sh/registry-key prior)))))))
