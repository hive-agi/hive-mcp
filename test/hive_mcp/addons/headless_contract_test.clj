(ns hive-mcp.addons.headless-contract-test
  "The `(def x spi/x)` facade aliases in `hive-mcp.addons.headless` carry their
   own `m/=>`: they capture the VALUE of the spi fns, so instrumenting the spi
   var never reaches a caller going through the facade. Each is driven here with
   a stub backend that implements the port."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.addons.headless :as headless]
            [malli.core :as m]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private aliases
  '[headless-id headless-spawn! headless-dispatch! headless-status headless-kill!
    headless-interrupt! declared-capabilities headless-backend? capabilities])

(def ^:private stub
  (reify
    headless/IHeadlessBackend
    (headless-id [_] :stub)
    (headless-spawn! [_ ctx _] (str "slave-" (:id ctx)))
    (headless-dispatch! [_ _ opts] {:dispatched? true :opts opts})
    (headless-status [_ _ ds] (merge ds {:alive? true}))
    (headless-kill! [_ ctx] {:killed? true :id (:id ctx)})
    (headless-interrupt! [_ ctx] {:success? true :ling-id (:id ctx)})
    headless/IHeadlessCapabilities
    (declared-capabilities [_] #{:cap/streaming})))

(deftest every-alias-is-contracted-test
  (let [contracted (get (m/function-schemas) 'hive-mcp.addons.headless)]
    (doseq [a aliases]
      (is (contains? contracted a) (str a " carries its own m/=>")))))

(deftest aliases-behave-under-instrumentation-test
  (let [vs (set (map #(ns-resolve 'hive-mcp.addons.headless %) aliases))]
    (mi/instrument! {:filters [(mi/-filter-var vs)]})
    (try
      (is (= :stub (headless/headless-id stub)))
      (is (= "slave-7" (headless/headless-spawn! stub {:id 7} {})))
      (is (= {:dispatched? true :opts {:t 1}} (headless/headless-dispatch! stub {} {:t 1})))
      (is (= {:alive? true :s 1} (headless/headless-status stub {} {:s 1})))
      (is (= {:killed? true :id 7} (headless/headless-kill! stub {:id 7})))
      (is (= {:success? true :ling-id 7} (headless/headless-interrupt! stub {:id 7})))
      (is (= #{:cap/streaming} (headless/declared-capabilities stub)))
      (is (= #{:cap/streaming} (headless/capabilities stub)))
      (is (true? (headless/headless-backend? stub)))
      (is (false? (headless/headless-backend? (Object.))))
      (testing "the id contract refuses a non-backend"
        (is (thrown? clojure.lang.ExceptionInfo (headless/headless-id (Object.)))))
      (finally (mi/unstrument! {:filters [(mi/-filter-var vs)]})))))
