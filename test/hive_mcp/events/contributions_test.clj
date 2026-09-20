;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.events.contributions-test
  "The kernel runs what was contributed, not a list it holds.

   Three properties matter: a contributor's register fn runs, one that throws
   does not take the others down, and a domain whose namespace is absent is
   skipped rather than fatal. The last one is the state every domain reaches
   once it becomes an addon, so it is bound through `soft/*resolve*` rather
   than simulated.

   One test reads the REAL manifest: every symbol it names must resolve in
   this build, and the events it declares must be the ones its register fn
   installs. That is the drift the old hand-written `expected-events` set
   could not catch."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.events.contributions :as contrib]
            [hive-mcp.events.handlers :as handlers]
            [hive-mcp.events.registry :as registry]
            [hive-mcp.swarm.adapters.soft :as soft]))

(defn- with-clean-registry [f]
  (let [handlers-before (contrib/contributed :handlers)
        effects-before  (contrib/contributed :effects)]
    (contrib/reset!!)
    (try (f)
      (finally
        (contrib/reset!!)
        (doseq [[k e] handlers-before] (contrib/contribute! :handlers k e))
        (doseq [[k e] effects-before]  (contrib/contribute! :effects k e))))))

(use-fixtures :each with-clean-registry)

(deftest a-contribution-registers-and-can-be-forgotten
  (let [ran (atom 0)]
    (contrib/contribute! :handlers ::probe {:install! #(swap! ran inc)
                                            :events #{:probe/one :probe/two}})
    (is (= #{:probe/one :probe/two} (contrib/declared-events)))
    (is (= {:ran [::probe] :failed {}} (contrib/register-all! :handlers)))
    (is (= 1 @ran))
    (contrib/forget! :handlers ::probe)
    (is (= #{} (contrib/declared-events)))
    (is (= {:ran [] :failed {}} (contrib/register-all! :handlers))
        "a forgotten contributor is not called again")))

(deftest one-failing-contributor-does-not-stop-the-others
  (let [ok (atom 0)]
    (contrib/contribute! :handlers ::boom {:install! #(throw (ex-info "boom" {}))})
    (contrib/contribute! :handlers ::fine {:install! #(swap! ok inc)})
    (let [{:keys [ran failed]} (contrib/register-all! :handlers)]
      (is (= 1 @ok) "the healthy contributor still registered")
      (is (= [::fine] ran))
      (is (contains? failed ::boom))
      (is (instance? Throwable (get failed ::boom))))))

(deftest a-domain-that-left-with-its-addon-is-absent-not-fatal
  (binding [soft/*resolve* (constantly nil)]
    (let [{:keys [handlers effects]} (contrib/load-manifest!)]
      (is (empty? (:contributed handlers)))
      (is (seq (:absent handlers)) "every in-core domain reads as absent")
      (is (empty? (:contributed effects)))
      (is (= #{} (contrib/declared-events))
          "nothing is declared, so nothing is expected")))
  (testing "and the kernel still expects exactly its own events"
    (binding [soft/*resolve* (constantly nil)]
      (contrib/reset!!)
      (contrib/load-manifest!)
      (is (= handlers/kernel-events (handlers/expected-events))))))

(deftest the-manifest-names-only-things-this-build-has
  (let [manifest (contrib/read-manifest)]
    (is (seq (:handlers manifest)))
    (doseq [kind [:handlers :effects]
            entry (get manifest kind)]
      (let [{:keys [key] sym :install!} entry]
        (testing (str kind " " key)
          (is (some? (requiring-resolve sym))
              (str sym " does not resolve; the manifest drifted from the build")))))))

(deftest a-declared-event-set-matches-what-the-domain-registers
  (contrib/load-manifest!)
  (contrib/register-all! :handlers)
  (let [registered (registry/registered-events)]
    (doseq [[key {:keys [events]}] (contrib/contributed :handlers)
            event events]
      (testing (str key " declares " event)
        (is (contains? registered event)
            "declared but not registered: the manifest promises a handler nobody installs")))))

(deftest expected-events-is-the-kernel-set-plus-what-contributed
  (contrib/reset!!)
  (contrib/contribute! :handlers ::probe {:install! (constantly true)
                                          :events #{:probe/one}})
  (is (= (conj handlers/kernel-events :probe/one) (handlers/expected-events))))
