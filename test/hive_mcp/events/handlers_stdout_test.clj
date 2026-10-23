(ns hive-mcp.events.handlers-stdout-test
  "register-handlers! runs at boot, before the stdio transport owns stdout.
   Its notices must land on stderr, never on *out* (kanban 20260930224940-31ead2d9)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.events.handlers :as handlers]
            [hive-mcp.events.registry :as registry]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- out+err
  "Run f with timbre silenced (production routes it to stderr in
   hive-mcp.server.core; the test JVM keeps timbre's *out* default),
   returning [stdout stderr] as strings."
  [f]
  (let [err (java.io.StringWriter.)
        out (with-out-str
              (binding [*err* err]
                (log/with-min-level :report (f))))]
    [out (str err)]))

(deftest verify-handlers-notices-go-to-stderr
  (testing "missing and extra handler notices leave *out* empty"
    (with-redefs [handlers/expected-events (constantly #{:a/expected})
                  registry/registered-events (constantly #{:z/extra})]
      (let [[out err] (out+err handlers/verify-handlers!)]
        (is (= "" out))
        (is (str/includes? err "missing handlers"))
        (is (str/includes? err "Extra handlers registered"))))))

(deftest register-handlers-first-registration-notice-goes-to-stderr
  (testing "the first-registration notice leaves *out* empty"
    (handlers/reset-registration!)
    (let [[out err] (out+err handlers/register-handlers!)]
      (is (= "" out))
      (is (str/includes? err "[hive-events] Handlers registered:")))))
