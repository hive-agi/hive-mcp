(ns hive-mcp.swarm.delegate-required-test
  "resolve-required: a shim var that a NEWER addon introduced. Resolved
   against namespaces that are always (or never) on the classpath, so the
   three answers are exercised without the addon and without with-redefs."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.swarm.delegate :as delegate]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest present-var-resolves-to-itself
  (is (= #'clojure.core/identity (delegate/resolve-required "clojure.core" 'identity))))

(deftest present-namespace-without-the-var-is-an-outdated-addon
  (let [e (try (delegate/resolve-required "clojure.core" 'no-such-var-in-core) nil
               (catch clojure.lang.ExceptionInfo e e))]
    (is (some? e) "an older addon is refused at resolution, never a nil call")
    (is (true? (:swarm/addon-outdated (ex-data e))))
    (is (= {:addon "clojure" :ns "clojure.core" :var "no-such-var-in-core"}
           (select-keys (ex-data e) [:addon :ns :var])))
    (testing "the message names the missing var and says to upgrade"
      (is (re-find #"clojure\.core/no-such-var-in-core" (ex-message e)))
      (is (re-find #"(?i)upgrade" (ex-message e))))))

(deftest absent-namespace-still-raises-addon-missing-on-call
  (let [v (delegate/resolve-required "no.such.addon.ns" 'f)
        e (try (v 1) nil (catch clojure.lang.ExceptionInfo e e))]
    (is (true? (:swarm/addon-missing (ex-data e))))
    (is (= "no" (:addon (ex-data e))))))
