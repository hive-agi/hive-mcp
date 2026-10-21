(ns hive-mcp.system.addon-hot-contract-test
  "Contract facets for hive-mcp.system.addon-hot: schema-driven trifectas over
   the pure strata, and a golden over boot!/start!/stop! with the catalog and
   hot engine injected as stub ports. The last-boot atom is restored."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.system.addon-hot :as ah]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private SpecIn [:map [:addon/id [:enum "hive.a" "hive.b" :hive.c]]])

(deftrifecta-from-schema registered-specs-keeps-exactly-the-registered
  #'ah/registered-specs
  {:in [:cat [:vector SpecIn]
        [:map [:hot/registered [:vector SpecIn]]]]
   :out [:vector ah/MountSpec]
   :rel (fn [[specs reg] out]
          (let [ids (set (map (comp str :addon/id) (:hot/registered reg)))]
            (= out (filterv #(ids (str (:addon/id %))) specs))))
   :golden-path "test/golden/hive-mcp/system/registered-specs.edn"
   :num-tests 100})

(deftrifecta-from-schema init-opts-never-carries-an-addon-dir
  #'ah/init-opts
  {:in [:cat [:map [:hot/no-reload [:set :symbol]] [:hot/dirs [:set :string]]]
        [:maybe [:int {:min 0}]]]
   :out ah/InitOpts
   :rel (fn [[plan since] out]
          (and (= [] (:dirs out))
               (= (set (:hot/no-reload plan)) (:no-reload out))
               (= since (:since out))))
   :num-tests 100})

(deftrifecta-from-schema boot-report-counts-and-sorts
  #'ah/boot-report
  {:in [:cat [:vector SpecIn]
        [:map [:hot/dirs [:set [:enum "/b" "/a" "/c"]]]
         [:hot/skipped [:vector SpecIn]]]
        [:map [:fresh? :boolean]]
        [:map [:ok? :boolean] [:hot/registered [:vector SpecIn]] [:errors [:vector :string]]]
        [:map [:claims [:map-of :string [:vector :string]]] [:errors [:vector :string]]]]
   :out ah/BootReport
   :rel (fn [[specs plan init reg claimed] out]
          (and (= (count specs) (:mounted out))
               (= (vec (sort (:hot/dirs plan))) (:dirs out))
               (= (:fresh? init) (:fresh? out))
               (= (:ok? reg) (:ok? out))
               (= (into (:errors reg) (:errors claimed)) (:errors out))))
   :num-tests 100})

(defn- catalog [specs host]
  (reify ah/IAddonCatalog (mounted-specs [_] specs) (mount-host [_] host)))

(defn- engine [throw-on]
  (reify ah/IHotEngine
    (plan [_ _ specs]
      (when (= throw-on :plan) (throw (ex-info "plan broke" {})))
      {:hot/dirs #{"/a/src"} :hot/no-reload #{'p} :hot/registered specs :hot/skipped []})
    (ensure-init! [_ _] {:fresh? true})
    (extend-dirs! [_ req] {:dirs (:dirs req) :added (:dirs req)})
    (register! [_ _ specs]
      (when (= throw-on :register) (throw (ex-info "register broke" {})))
      {:ok? true :hot/registered specs})
    (unregister! [_ specs] (mapv :addon/id specs))))

(golden/deftest-golden addon-hot-boot-arms-golden
  "test/golden/hive-mcp/system/addon-hot-boot.edn"
  (let [specs [{:addon/id "hive.a"}]
        prior (ah/last-report)]
    (try
      {:ok         (ah/boot! (catalog specs :host) (engine nil) 5)
       :empty      (ah/boot! (catalog [] :host) (engine nil) nil)
       :no-host    (ah/boot! (catalog specs nil) (engine nil) nil)
       :plan-fail  (:error (ah/boot! (catalog specs :host) (engine :plan) nil))
       :reg-fail   (:error (ah/boot-registration (catalog specs :host) (engine :register) nil))
       :registered (:registered (:ok (ah/boot-registration (catalog specs :host) (engine nil) nil)))
       :start      (let [st (ah/start! (catalog specs :host) (engine nil) nil)]
                     {:registered (:registered st)
                      :result-ok? (contains? (:result st) :ok)
                      :last-is-result (= (:result st) (ah/last-report))
                      :stop (ah/stop! st)})
       :stop-empty (ah/stop! {:engine (engine nil) :registered []})}
      (finally (reset! ah/last-boot prior)))))

(deftest addon-hot-boot-results-hit-each-arm
  (let [prior (ah/last-report)]
    (try
      (let [ok (ah/boot! (catalog [{:addon/id "x"}] :host) (engine nil) nil)
            err (ah/boot! (catalog [{:addon/id "x"}] nil) (engine nil) nil)]
        (is (m/validate ah/BootResult ok))
        (is (m/validate [:map [:ok ah/BootReport]] ok) "the full report on the happy arm")
        (is (m/validate ah/BootResult err))
        (is (not (m/validate [:map [:ok :any]] err)))
        (is (m/validate ah/AddonCatalog (ah/live-catalog)))
        (is (m/validate ah/HotEngine (ah/live-engine))))
      (finally (reset! ah/last-boot prior)))))
