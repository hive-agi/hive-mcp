(ns hive-mcp.spi.session-test
  "Facets for hive-mcp.spi.session (cov-facets f07): a recording
   ISessionLifecycle adapter is installed through the facade's install!,
   both calls are driven through it, and the port laws run against it."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.spi.session :as session]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord RecordingSession [seen]
  session/ISessionLifecycle
  (-record-session-start! [_ agent-id] (swap! seen conj [:start agent-id]) agent-id)
  (-register-session-hooks! [_ registry]
    (swap! seen conj [:hooks (:name registry)])
    {:registered [:session-end]}))

(deftest-golden session-facade-golden
  "test/golden/hive-mcp/f07-spi-session-facade.edn"
  (let [seen (atom [])]
    (session/install! (->RecordingSession seen))
    (try
      {:start       [(session/record-session-start! "ling-1")
                     (session/record-session-start! nil)]
       :hooks       (session/register-session-hooks! {:name "test-registry"})
       :reset-cache (session/reset-cache!)
       :calls       @seen}
      (finally (session/uninstall!)))))

(deftest-port-contract recording-session-port-laws
  {:port/methods
   {session/-record-session-start!   {:in [:cat [:maybe [:string {:min 1}]]]
                                      :out [:maybe :string]
                                      :rel (fn [[id] out] (= id out))}
    session/-register-session-hooks! {:in [:cat [:map [:name :string]]]
                                      :out [:map [:registered [:vector :keyword]]]}}}
  (->RecordingSession (atom [])))

(deftest uninstall-restores-the-fallback
  (session/install! (->RecordingSession (atom [])))
  (session/uninstall!)
  (is (not (instance? RecordingSession (session/current)))))
