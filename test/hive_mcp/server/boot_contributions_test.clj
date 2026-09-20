;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.server.boot-contributions-test
  "The kernel's embedding entry point runs a CONTRIBUTED boot step. Two
   branches must be honest: with the memory domain present the step runs and
   the entry point says true, and with it absent nothing runs and it says
   false rather than reporting wiring that did not happen."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.server.init :as init]
            [hive-mcp.spi.contributions :as contrib]
            [hive-mcp.swarm.adapters.soft :as soft]))

(defn- with-restored-boot [f]
  (try (f)
    (finally
      (contrib/reset-kind! :boot)
      (init/load-boot-contributions!))))

(use-fixtures :each with-restored-boot)

(deftest the-boot-manifest-names-only-things-this-build-has
  (let [{:keys [boot]} (init/load-boot-contributions!)]
    (is (empty? (:absent boot))
        (str "the boot manifest names domains this build does not have: " (:absent boot)))
    (is (= [:embeddings] (:contributed boot)))))

(deftest the-entry-point-runs-the-contributed-step
  (contrib/reset-kind! :boot)
  (let [ran (atom 0)]
    (contrib/contribute! :boot :probe {:install! #(swap! ran inc)})
    (is (true? (init/init-embedding-provider!))
        "a step ran and none failed")
    (is (pos? @ran))))

(deftest a-failing-step-is-reported-not-swallowed-as-success
  (contrib/reset-kind! :boot)
  (contrib/contribute! :boot :boom {:install! #(throw (ex-info "boom" {}))})
  (is (false? (init/init-embedding-provider!))
      "a boot step that threw must not read as wired"))

(deftest with-no-memory-domain-there-is-nothing-to-wire
  (contrib/reset-kind! :boot)
  (binding [soft/*resolve* (constantly nil)]
    (let [{:keys [boot]} (init/load-boot-contributions!)]
      (is (= [:embeddings] (:absent boot)))
      (is (empty? (:contributed boot))))
    (testing "and the entry point says so instead of pretending"
      (is (false? (init/init-embedding-provider!))))))
