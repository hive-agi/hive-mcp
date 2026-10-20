(ns hive-mcp.extensions.runtime-contract-test
  "Contract facets for the client-runtime provisioner host: its config
   reading, its provisioner options and the provisioning of a MountReport."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.extensions.runtime :as rt]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-mcp.extensions.registry :as ext]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private RuntimeCfg [:map [:enabled? {:optional true} [:maybe :boolean]]
                           [:home {:optional true} [:enum "/h/a" "/h/b"]]])

(deftrifecta-from-schema config-reads-the-runtime-map
  #'rt/config
  {:in [:cat [:map [:runtime [:or [:map [:enabled? :boolean] [:home [:enum "/h/a" "/h/b"]]] [:enum "on" :off]]]]]
   :out [:or [:map {:closed true}] RuntimeCfg]
   :rel (fn [[svc] out] (let [c (:runtime svc)] (= (if (map? c) c {}) out)))
   :golden-path "test/golden/hive-mcp/extensions/runtime-config.edn"
   :num-tests 100})

(deftrifecta-from-schema enabled-lets-the-env-flag-win
  #'rt/enabled?
  {:in [:cat [:maybe [:map [:runtime {:optional true} RuntimeCfg]]]
        [:maybe [:enum "1" "true" "0" "false"]]]
   :out :boolean
   :rel (fn [[svc flag] out]
          (= (if (some? flag)
               (contains? #{"1" "true"} flag)
               (not (false? (get-in svc [:runtime :enabled?]))))
             out))
   :golden-path "test/golden/hive-mcp/extensions/runtime-enabled.edn"
   :num-tests 100})

(deftrifecta-from-schema provisioner-opts-default-the-home-and-liveness
  #'rt/provisioner-opts
  {:in [:cat [:maybe [:map [:home {:optional true} [:enum "/h/a" "/h/b"]]
                      [:live? {:optional true} [:maybe :boolean]]
                      [:profiles {:optional true} [:vector :keyword]]]]]
   :out [:map [:home :string] [:eval-fn ifn?] [:live? :boolean]
         [:profiles {:optional true} [:vector :keyword]]]
   :rel (fn [[cfg] out]
          (and (= (or (:home cfg) (System/getProperty "user.home")) (:home out))
               (= (not (false? (:live? cfg))) (:live? out))
               (= (:profiles cfg) (:profiles out))
               (not (contains? out :declarations))))
   :num-tests 100})

(def ^:private AddonId [:enum "a" "b" "c"])

(def ^:private Provision
  [:maybe [:fn {:gen/elements [(fn [spec _instance] {:rt (:addon/id spec)})]} ifn?]])

(deftrifecta-from-schema provision-mounted-attaches-a-runtime-once
  #'rt/provision-mounted
  {:in [:cat
        [:map [:mounted [:vector [:map [:addon/id AddonId] [:success? :boolean]
                                  [:runtime {:optional true} [:= :pre]]]]]]
        [:vector [:map [:addon/id [:enum "a" "b"]]]]
        Provision
        [:map-of [:enum "a" "c"] [:= :instance]]]
   :out [:map [:mounted [:vector [:map [:addon/id AddonId] [:success? :boolean]
                                  [:runtime {:optional true} [:or [:= :pre] [:map [:rt AddonId]]]]]]]]
   :rel (fn [[report specs provision instance-of] out]
          (let [spec-ids (set (map :addon/id specs))]
            (if-not provision
              (= report out)
              (= (mapv (fn [{:keys [success? runtime] id :addon/id :as r}]
                         (if (and success? (nil? runtime) (spec-ids id) (instance-of id))
                           (assoc r :runtime {:rt id})
                           r))
                       (:mounted report))
                 (:mounted out)))))
   :num-tests 200})

(deftest the-shared-provisioner-starts-empty-after-a-reset
  (try
    (rt/reset-provisioner!)
    (is (= {} (rt/installed)))
    (is (nil? (rt/deprovision! "m11.probe")))
    (is (map? (rt/mount-opts)))
    (is (or (nil? (rt/provision-fn)) (ifn? (rt/provision-fn))))
    (finally (rt/reset-provisioner!))))

(deftest registered-eval-is-nil-without-a-client-transport
  (when-not (ext/get-extension rt/client-eval-key)
    (is (nil? (rt/registered-eval :m11-probe-client ["echo 1"])))))
