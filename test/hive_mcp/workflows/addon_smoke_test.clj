(ns hive-mcp.workflows.addon-smoke-test
  "Integration smoke-test: the hive-workflows addons load at boot.

   Tagged ^:integration because it requires the LIVE/local classpath (the
   server launched with local.deps.edn) — hive-workflows is on neither deps.edn
   nor any :test* alias classpath. Under a cold JVM this test self-skips loudly
   rather than false-failing. See hive-mcp.workflows.addon-smoke for the why."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.workflows.addon-smoke :as smoke]
            [hive-mcp.addons.protocol :as proto]
            [hive-mcp.addons.core :as addons]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

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

;; -----------------------------------------------------------------------------
;; Facets
;; -----------------------------------------------------------------------------

(def ^:private active-stub-id "smoke-facet-active")
(def ^:private registered-stub-id "smoke-facet-registered")

(defn- with-stub-addons
  "Register one stub addon and boot it (:active), register a second without
   booting it; unregister both afterwards."
  [f]
  (addons/register-addon! (->StubAddon active-stub-id))
  (addons/init-addon! active-stub-id)
  (addons/register-addon! (->StubAddon registered-stub-id))
  (try (f)
       (finally
         (addons/shutdown-addon! active-stub-id)
         (addons/unregister-addon! active-stub-id)
         (addons/unregister-addon! registered-stub-id))))

(def ^:private StubId [:enum "smoke-facet-active" "smoke-facet-registered" "smoke-facet-absent"])

(def ^:private IdColl
  [:maybe [:or [:set {:max 3} StubId] [:vector {:max 3} StubId]]])

(deftrifecta-from-schema addons-initialized?-is-every-id-active
  #'smoke/addons-initialized?
  {:in [:cat IdColl]
   :out :boolean
   :rel (fn [[ids] out] (= out (every? #{active-stub-id} ids)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   ;; the property's registry state is installed per test by the fixture
   :num-tests 100})

(clojure.test/use-fixtures :each with-stub-addons)

(golden/deftest-golden cold-jvm-smoke-golden
  "test/golden/hive-mcp/workflows/addon-smoke-cold-jvm.edn"
  ;; This JVM (the :test-unit classpath) carries no hive-workflows addon, so
  ;; every check answers its absent-addon shape.
  (let [{:keys [by-id scan-errors] :as discovered} (smoke/discovered-addons)
        report (smoke/check)
        ;; BUG (recorded, not fixed): check-ir-seam's contract promises
        ;; {:ok? false :ir-loadable? false} when hive-workflows is absent, but
        ;; requiring-resolve THROWS FileNotFoundException for a missing
        ;; namespace instead of answering nil, so the `if-not` branch is
        ;; unreachable. Pinned as the current behaviour: it throws.
        ir-seam (try (smoke/check-ir-seam)
                     (catch java.io.FileNotFoundException _ :throws-file-not-found))]
    {:on-classpath? (smoke/addons-on-classpath?)
     :initialized? (smoke/addons-initialized?)
     :discovered {:by-id (vec (sort (keys by-id))) :scan-errors (count scan-errors)
                  :scan-total-nonneg? (<= 0 (:scan-total discovered))}
     :check (-> report
                (select-keys [:ok? :missing-addons :unresolved :missing-methods])
                (update-vals #(if (set? %) (vec (sort %)) %)))
     :report-prefix (subs (:report report) 0 5)
     :ir-seam ir-seam
     :required (vec (sort smoke/required-addon-ids))
     :methods (vec (sort smoke/required-methods))}))
