(ns hive-mcp.addons.runtime-ports-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.addons.runtime-ports :as runtime-ports]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]))

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
    :extension/retract-contributions!
    :emacs/lookup-ling-fn
    :emacs/tasks-for-ling-fn
    :emacs/fail-task-fn
    :emacs/release-claims-fn
    :emacs/update-ling-fn})

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

(def ^:private gen-slave
  (gen/let [id (gen/elements ["l1" "l2" "ling-x"])
            status (gen/elements [:idle :working :error])
            pid (gen/elements ["hive" "p"])
            with-status? gen/boolean
            with-pid? gen/boolean]
    (cond-> {:slave/id id :slave/name "n"}
      with-status? (assoc :slave/status status)
      with-pid? (assoc :slave/project-id pid))))

(deftrifecta slave-projects-onto-ling-vocabulary
  runtime-ports/slave->ling
  {:gen gen-slave
   :pred #(and (string? (:ling/id %))
               (every? #{"ling"} (map namespace (keys %))))
   :num-tests 50
   :mutations [["leaks-slave-keys" (fn [s] (assoc s :ling/id (:slave/id s)))]
               ["drops-id" (fn [_] {:ling/status :idle})]]
   :assert (fn []
             (is (nil? (runtime-ports/slave->ling nil)))
             (is (= {:ling/id "a" :ling/status :idle :ling/project-id "p"}
                    (runtime-ports/slave->ling {:slave/id "a" :slave/status :idle
                                                :slave/project-id "p" :slave/name "x"})))
             (is (= {:slave/status :error}
                    (runtime-ports/ling-updates->slave {:ling/status :error :other 1}))))})

(deftest emacs-ling-ports-route-to-the-swarm-datascript
  (let [calls (atom [])
        call-fn (fn [sym & args]
                  (swap! calls conj (into [sym] args))
                  (case (name sym)
                    "get-slave" {:slave/id (first args) :slave/status :working}
                    "get-tasks-for-slave" (list {:task/id "t1"})
                    :ok))
        ports (runtime-ports/emacs-ling-ports call-fn)]
    (is (= {:ling/id "l1" :ling/status :working} ((:emacs/lookup-ling-fn ports) "l1")))
    (is (= [{:task/id "t1"}] ((:emacs/tasks-for-ling-fn ports) "l1" :dispatched)))
    (is (= {:success true} ((:emacs/fail-task-fn ports) "t1" :timeout)))
    ((:emacs/release-claims-fn ports) "l1")
    ((:emacs/update-ling-fn ports) "l1" {:ling/status :error})
    (is (= [['hive-mcp.swarm.datascript.queries/get-slave "l1"]
            ['hive-mcp.swarm.datascript.queries/get-tasks-for-slave "l1" :dispatched]
            ['hive-mcp.swarm.datascript.lings/fail-task! "t1" :timeout]
            ['hive-mcp.swarm.datascript.lings/release-claims-for-slave! "l1"]
            ['hive-mcp.swarm.datascript.lings/update-slave! "l1" {:slave/status :error}]]
           @calls))
    (testing "an unknown ling is nil, not an error"
      (is (nil? ((:emacs/lookup-ling-fn (runtime-ports/emacs-ling-ports (constantly nil))) "zz"))))))
