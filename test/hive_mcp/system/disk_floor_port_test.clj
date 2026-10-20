(ns hive-mcp.system.disk-floor-port-test
  "Boot-port contract: lifecycle tests can intercept disk-pressure abort before init."
  (:require [clojure.test :refer [is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.server.core :as core]))

(defn boot-port-result
  "Exercise the start boot port before any Integrant components initialize."
  [profile]
  (try
    (core/start! :profile profile
                 :hprof-boot! (fn [] (throw (ex-info "boot intercepted" {:code 78}))))
    :init-reached
    (catch clojure.lang.ExceptionInfo e
      (if (= 78 (:code (ex-data e))) :intercepted :other-error))))

(deftrifecta disk-floor-boot-port
  boot-port-result
  {:golden-path "test/golden/system/disk-floor-boot-port.edn"
   :cases {:desktop :desktop :headless :k8s-headless :minimal :k8s-minimal}
   :gen (gen/elements [:desktop :k8s-headless :k8s-minimal])
   :pred #{:intercepted}
   :num-tests 30
   :mutations [["boot bypassed" (constantly :init-reached)]]
   :assert (fn []
             (is (= :intercepted (boot-port-result :desktop))
                 "the boot port intercepts before Integrant starts")
             (is (nil? @core/system) "abort never initializes the system"))})
