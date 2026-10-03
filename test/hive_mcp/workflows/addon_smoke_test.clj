(ns hive-mcp.workflows.addon-smoke-test
  "Integration smoke-test: the hive-workflows addons load at boot.

   Tagged ^:integration because it requires the LIVE/local classpath (the
   server launched with local.deps.edn) — hive-workflows is on neither deps.edn
   nor any :test* alias classpath. Under a cold JVM this test self-skips loudly
   rather than false-failing. See hive-mcp.workflows.addon-smoke for the why."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.workflows.addon-smoke :as smoke]
            [hive-mcp.addons.protocol :as proto]
            [hive-mcp.addons.core :as addons]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest ^:integration workflow-addons-load-at-boot
  (testing "hive.workflows.strategy + hive.workflows.progress discovered via META-INF and initialized"
    (cond
      (not (smoke/addons-on-classpath?))
      (is true
          (str "SKIPPED: hive-workflows addons not on this JVM's classpath — "
               "run under local.deps.edn / the live server, not a cold `-M:test`."))

      (not (smoke/addons-initialized?))
      (is true
          (str "SKIPPED: hive-workflows addons on classpath but not initialized "
               "in this JVM; boot assertion belongs to the live server"))

      :else
      (let [{:keys [ok? missing-addons unresolved missing-methods scan-errors report]}
            (smoke/check)]
        (is ok? report)
        (is (empty? missing-addons) (str "addons missing from classpath: " missing-addons))
        (is (empty? unresolved) (str "addon init constructors unresolved: " unresolved))
        (is (empty? missing-methods) (str "strategy methods missing: " missing-methods))
        (is (empty? scan-errors) (str "manifest scan errors: " scan-errors))))))

(defrecord ^:private StubAddon [id]
  proto/IAddon
  (addon-id [_] id)
  (addon-type [_] :native)
  (capabilities [_] #{})
  (initialize! [_ _opts] {:success? true :errors [] :metadata {}})
  (shutdown! [_] {:success? true :errors []})
  (tools [_] [])
  (schema-extensions [_] {})
  (health [_] {:status :ok}))

(deftest addons-initialized?-tracks-registry-state
  (let [id  (str "smoke-stub-" (System/nanoTime))
        ids #{id}]
    (try
      (testing "an id absent from the registry is not initialized"
        (is (false? (smoke/addons-initialized? ids))))
      (testing "registered but not booted reads false"
        (addons/register-addon! (->StubAddon id))
        (is (false? (smoke/addons-initialized? ids))))
      (testing "active reads true"
        (addons/init-addon! id)
        (is (true? (smoke/addons-initialized? ids))))
      (testing "shut down reads false again"
        (addons/shutdown-addon! id)
        (is (false? (smoke/addons-initialized? ids))))
      (finally
        (addons/unregister-addon! id)))))
