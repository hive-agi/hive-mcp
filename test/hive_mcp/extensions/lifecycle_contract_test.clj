(ns hive-mcp.extensions.lifecycle-contract-test
  "Contract facets for the lifecycle host: its pure reports (unmount,
   delegated re-seat), its config reading, and the re-seat decision."
  (:require [clojure.test :refer [deftest is]]
            [hive-addon.lifecycle :as lc]
            [hive-mcp.extensions.lifecycle :as lcm]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema stub-owner-suffixes-the-addon-id
  #'lcm/stub-owner
  {:in [:cat [:enum "hive.carto" "probe.a" :probe/kw]]
   :out [:re #"^.+#dormant$"]
   :rel (fn [[id] out] (= (str id "#dormant") out))
   :golden-path "test/golden/hive-mcp/extensions/stub-owner.edn"
   :num-tests 30})

(def ^:private Td
  [:maybe [:map [:errors {:optional true} [:vector [:enum "td-1" "td-2"]]]
           [:torn-down {:optional true} [:vector [:enum "a" "b"]]]
           [:teardown/data-preserved? {:optional true} :boolean]]])

(def ^:private Un
  [:maybe [:map [:unregistered {:optional true} [:vector [:enum "a" "b"]]]
           [:unsupported {:optional true} [:vector [:enum "c"]]]
           [:errors {:optional true} [:vector [:enum "un-1"]]]]])

(deftrifecta-from-schema unmount-report-joins-teardown-and-plug-out
  #'lcm/unmount-report
  {:in [:cat Td Un]
   :out [:map [:ok? :boolean] [:errors [:vector :string]] [:torn-down [:vector :string]]
         [:unregistered [:vector :string]]
         [:teardown/data-preserved? {:optional true} :boolean]
         [:unsupported {:optional true} [:vector {:min 1} :string]]]
   :rel (fn [[td un] out]
          (let [errs (into (vec (:errors td)) (:errors un))]
            (and (= errs (:errors out))
                 (= (empty? errs) (:ok? out))
                 (= (contains? td :teardown/data-preserved?)
                    (contains? out :teardown/data-preserved?))
                 (= (seq (:unsupported un)) (seq (:unsupported out))))))
   :golden-path "test/golden/hive-mcp/extensions/unmount-report.edn"
   :num-tests 200})

(deftrifecta-from-schema delegated-report-keeps-a-failed-reseat
  #'lcm/delegated-report
  {:in [:cat [:or [:map [:reseated? :boolean] [:errors {:optional true} [:vector [:enum "x"]]]]
              [:enum :done]]
        [:enum "HostA" "HostB"]]
   :out [:map [:via [:= :hive-addon]] [:host :string] [:reseated? :boolean]]
   :rel (fn [[ret host] out]
          (= (merge (if (map? ret) ret {:reseated? true}) {:via :hive-addon :host host}) out))
   :golden-path "test/golden/hive-mcp/extensions/delegated-report.edn"
   :num-tests 100})

(deftrifecta-from-schema lifecycle-config-reads-the-lifecycle-map
  #'lcm/config
  {:in [:cat [:map [:lifecycle [:or [:map [:enabled? :boolean] [:sweep-interval-ms [:int {:min 1, :max 9}]]] [:enum true "yes"]]]]]
   :out [:map [:enabled? {:optional true} :boolean]]
   :rel (fn [[svc] out]
          (and (= (let [c (:lifecycle svc)] (if (map? c) c {})) out)
               (= (true? (:enabled? out)) (lcm/enabled? svc))))
   :golden-path "test/golden/hive-mcp/extensions/lifecycle-config.edn"
   :num-tests 100})

(deftest a-current-host-needs-no-reseat
  (let [h (lcm/host {:resolve-config (constantly {})})]
    (is (= "hive_mcp.extensions.lifecycle.McpLifecycleHost" (.getName (class h))))
    (is (false? (lcm/stale-host? h)))
    (is (false? (lcm/stale-mount-host? h)))
    (is (false? (lcm/needs-reseat? h)))
    (is (false? (lcm/stale-mount-host? {})))
    (is (= (into {} h) (into {} (lcm/rebuild-host h))))
    (is (= {:host :x :k 1} (lcm/reseat-manager {:host :y :k 1} :x)))))

(deftest reseat-host-without-a-manager-does-nothing
  (when (nil? (lc/installed-manager))
    (is (= {:reseated? false :reason :no-manager} (lcm/reseat-host! [])))))

(deftest the-surface-dir-sits-under-the-hive-config
  (is (.endsWith ^String (lcm/surface-dir) "/.config/hive-mcp/data/addon-surfaces")))

(deftest wrap-handler-is-identity-without-a-manager
  (when (nil? (lc/installed-manager))
    (let [h (fn [_] :ok)]
      (is (identical? h (lcm/wrap-handler "m11.probe" h))))))
