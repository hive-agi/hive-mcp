(ns hive-mcp.addons.headless-test
  "Golden over the IHeadlessBackend facade. Every alias is driven through
   recording stubs that extend the CANONICAL hive-spi protocols: a bare
   backend, a backend that declares capabilities, and a full addon."
  (:require [clojure.test :refer [deftest is]]
            [hive-addon.protocol :as proto]
            [hive-mcp.addons.headless :as h]
            [hive-spi.addon.headless :as spi]
            [hive-test.golden :refer [deftest-golden-fn]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord StubBackend [id calls]
  spi/IHeadlessBackend
  (headless-id [_] id)
  (headless-spawn! [_ ctx opts] (swap! calls conj [:spawn ctx opts]) {:spawned (:ling-id ctx)})
  (headless-dispatch! [_ ctx task] (swap! calls conj [:dispatch ctx task]) {:dispatched task})
  (headless-status [_ ctx last] (swap! calls conj [:status ctx last]) {:status :running})
  (headless-kill! [_ ctx] (swap! calls conj [:kill ctx]) {:killed true})
  (headless-interrupt! [_ ctx] (swap! calls conj [:interrupt ctx]) {:interrupted true}))

(defrecord CapableBackend [id]
  spi/IHeadlessBackend
  (headless-id [_] id)
  (headless-spawn! [_ _ _] nil)
  (headless-dispatch! [_ _ _] nil)
  (headless-status [_ _ _] nil)
  (headless-kill! [_ _] nil)
  (headless-interrupt! [_ _] nil)
  spi/IHeadlessCapabilities
  (declared-capabilities [_] #{:cap/tools :cap/streaming}))

(defrecord AddonBackend [id]
  spi/IHeadlessBackend
  (headless-id [_] id)
  (headless-spawn! [_ _ _] nil)
  (headless-dispatch! [_ _ _] nil)
  (headless-status [_ _ _] nil)
  (headless-kill! [_ _] nil)
  (headless-interrupt! [_ _] nil)
  proto/IAddon
  (addon-id [_] (name id))
  (addon-type [_] :native)
  (capabilities [_] #{})
  (initialize! [_ _] {:success? true :errors [] :metadata {}})
  (shutdown! [_] {:success? true :errors []})
  (tools [_] [])
  (schema-extensions [_] {})
  (health [_] {:status :ok}))

(defn- scenario []
  (let [calls (atom [])
        bare  (->StubBackend :stub calls)
        cap   (->CapableBackend :capable)
        addon (->AddonBackend :addon)
        ctx   {:ling-id "ling-1"}]
    {:dispatch {:id        (h/headless-id bare)
                :spawn     (h/headless-spawn! bare ctx {:model "m"})
                :dispatch  (h/headless-dispatch! bare ctx "task")
                :status    (h/headless-status bare ctx nil)
                :kill      (h/headless-kill! bare ctx)
                :interrupt (h/headless-interrupt! bare ctx)
                :calls     @calls}
     :backend? {:bare (h/headless-backend? bare) :addon (h/headless-backend? addon)
                :map  (h/headless-backend? {}) :nil (h/headless-backend? nil)}
     :addon?   {:bare (h/headless-addon? bare) :addon (h/headless-addon? addon)
                :map  (h/headless-addon? {})}
     :capabilities {:bare     (vec (sort (h/capabilities bare)))
                    :declared (vec (sort (h/capabilities cap)))
                    :direct   (vec (sort (h/declared-capabilities cap)))}}))

(deftest-golden-fn headless-facade-golden
  "test/golden/addons/headless-facade.edn"
  scenario)

(deftest aliases-name-the-spi-protocols
  ;; A `def` alias holds the protocol map as of load time; extending the
  ;; protocol rebinds the spi var's root, so compare the var it names.
  (is (= #'spi/IHeadlessBackend (:var h/IHeadlessBackend)))
  (is (= #'spi/IHeadlessCapabilities (:var h/IHeadlessCapabilities))))
