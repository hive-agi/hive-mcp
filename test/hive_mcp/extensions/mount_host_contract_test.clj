(ns hive-mcp.extensions.mount-host-contract-test
  "Contract facets for the IMountHost adapter, driven through injected seams
   only: no test here touches the addons.core registry."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.extensions.mount-host :as mh]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private FakeHost
  [:map
   [:registered-fn [:fn {:gen/elements [(constantly nil) (constantly :present)]} ifn?]]
   [:unreg-fn [:fn {:gen/elements [(constantly nil)]} ifn?]]])

(deftrifecta-from-schema unregister-entry-returns-the-host
  #'mh/unregister-entry!
  {:in [:cat FakeHost [:enum "x.one" "x.two"]]
   :out FakeHost
   :rel (fn [[host _] out] (identical? host out))
   :num-tests 50})

(deftrifecta-from-schema unregister-reports-a-host-that-cannot-plug-out
  #'mh/unregister!
  {:in [:cat [:enum :not-a-host] [:vector {:max 4} [:enum "x.one" "x.two" "x.three"]]]
   :out [:map {:closed true} [:unregistered [:= []]] [:unsupported [:vector :string]] [:errors [:= []]]]
   :rel (fn [[_ ids] out] (= (vec ids) (:unsupported out)))
   :golden-path "test/golden/hive-mcp/extensions/unregister-unsupported.edn"
   :num-tests 50})

(deftest unregister-entry-drops-only-a-registered-id
  (let [dropped (atom [])
        host {:registered-fn #{"x.one"} :unreg-fn #(swap! dropped conj %)}]
    (mh/unregister-entry! host "x.one")
    (mh/unregister-entry! host "x.gone")
    (is (= ["x.one"] @dropped))))

(deftest addon-registry-host-carries-the-injected-seams
  (let [seams {:reg-fn identity :init-fn (fn [_ _] nil) :shutdown-fn (constantly {:success? true})
               :unreg-fn identity :registered-fn (constantly nil)}
        host (mh/addon-registry-host seams)]
    (is (= "hive_mcp.extensions.mount_host.AddonRegistryHost" (.getName (class host))))
    (is (= seams (select-keys host (keys seams))))
    (is (boolean? (mh/ensure-unregister!)))
    (is (identical? host (mh/current host)))
    (is (= :not-a-host (mh/current :not-a-host)))))
