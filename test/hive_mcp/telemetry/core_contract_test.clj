(ns hive-mcp.telemetry.core-contract-test
  "Contract facets for hive-mcp.telemetry.core: the log-shaping fns, observed
   through a timbre capture appender, never redefined."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.telemetry.core :as telemetry]
            [hive-mcp.telemetry.health :as health]
            [hive-mcp.test.stub.log-capture :as cap :refer [with-captured-logs]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- request-event
  "Subject wrapper is NOT what the trifecta targets; it only reads the event
   the contracted log-eval-request emitted."
  [arg]
  (with-captured-logs logs
    (telemetry/log-eval-request arg)
    (cap/event-data logs :eval-request)))

(deftrifecta-from-schema log-eval-request-answers-nil
  #'telemetry/log-eval-request
  {:in [:map [:code [:string {:max 80}]] [:mode [:enum :elisp :cider-silent]]]
   :out :nil
   ;; logging is the effect; the relation checks the logged preview
   :rel (fn [{:keys [code]} _]
          (let [ev (request-event {:code code :mode :elisp})]
            (and (= (count code) (:code-length ev))
                 (= (:code-preview ev)
                    (if (> (count code) 50) (str (subs code 0 50) "...") code)))))
   ;; :out is :nil, which has no mutants to derive
   :mutation false
   :num-tests 50})

(golden/deftest-golden log-eval-shapes-golden
  "test/golden/hive-mcp/telemetry/log-eval-shapes.edn"
  (with-captured-logs logs
    (telemetry/log-eval-request {:code (apply str (repeat 60 "x")) :mode :elisp :metadata {:u "a"}})
    (telemetry/log-eval-request {:code "(+ 1 2)" :mode :cider-silent})
    (telemetry/log-eval-result {:success true :duration-ms 4 :result-length 9 :metadata {:s 1}})
    (telemetry/log-eval-result {:success false :error "boom" :duration-ms 7})
    (telemetry/log-eval-exception {:exception (ex-info "kaput" {}) :operation "eval-x"})
    (mapv (fn [e] [(:level e) (:event e)
                   (dissoc (:data e) :exception-type)])
          (cap/events logs))))

(deftest configure-logging-answers-the-merged-config
  (let [saved log/*config*]
    (try
      (let [cfg (telemetry/configure-logging! {:level :warn
                                               :appenders {:println {:enabled? false}}})]
        (is (map? cfg))
        (is (= :warn (:min-level cfg))))
      (finally (log/set-config! saved)))))

(deftest core-facade-aliases-point-at-health
  (is (= health/severities telemetry/health-severities))
  (is (= health/error-types telemetry/health-error-types))
  (is (= #'health/emit-health-event! telemetry/emit-health-event!)))
