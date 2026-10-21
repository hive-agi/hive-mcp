(ns hive-mcp.swarm.bootstrap.factory-test
  "Facets for hive-mcp.swarm.bootstrap.factory.

   The :emacs bootstrap reads the swarm through the editor services port
   (hive-spi.editor.services, capability :dispatch under :vessel). The suite
   satisfies that port with a stub capability that answers a canned
   {:success ... :result ...}, and restores whatever was registered before.

     golden    load-slaves over a good JSON payload, a payload that does not
               parse, a failed dispatch and no capability at all; source
               resolution from an explicit arg, a string config and a
               throwing config read
     property  any slave list the vessel reports round-trips through
               load-slaves

   The :none / :datahike / unknown sources build a NoopBootstrap, a compat
   shim over hive-agent; without hive-agent on the unit classpath the golden
   records the addon-missing refusal they answer."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.swarm.bootstrap.factory :as factory]
            [hive-spi.editor.services :as svc]
            [hive-test.golden :refer [deftest-golden]]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f]
    (let [before (get (svc/registered) :vessel)]
      (svc/unregister-services! :vessel)
      (try (f)
           (finally (svc/unregister-services! :vessel)
                    (when before (svc/register-services! :vessel before)))))))

(defn- vessel-answers!
  "Register a :vessel :dispatch stub answering ANSWER, recording each op."
  [answer]
  (let [calls (atom [])]
    (svc/register-services! :vessel {:dispatch (fn [req timeout-ms]
                                                 (swap! calls conj [(:op req) timeout-ms])
                                                 answer)})
    calls))

(defn- no-config [& _] nil)

(defn- load-with
  "load-slaves of an :emacs bootstrap whose vessel answers ANSWER (nil: no
   capability registered), plus the ops the vessel saw."
  [answer]
  (svc/unregister-services! :vessel)
  (let [calls (when answer (vessel-answers! answer))
        b     (factory/make-bootstrap {:source :emacs :timeout-ms 1234} no-config)]
    {:slaves (factory/load-slaves b) :calls (some-> calls deref)}))

(defn- refusal [f]
  (try [:built (some? (f))]
       (catch clojure.lang.ExceptionInfo e [:threw (select-keys (ex-data e) [:swarm/addon-missing :ns :var])])))

(deftest-golden emacs-bootstrap-golden
  "test/golden/hive-mcp/swarm/bootstrap/factory-emacs.edn"
  {:good-json   (load-with {:success true
                            :result (json/write-str {:slaves-detail [{:slave-id "a" :status "idle"}
                                                                     {:slave-id "b" :status "working"}]})})
   :bad-json    (load-with {:success true :result "not json {"})
   :failed      (load-with {:success false :error "vessel down"})
   :unavailable (load-with nil)
   :snapshot-is-self (let [b (factory/make-bootstrap {:source :emacs} no-config)]
                       [(identical? b (factory/snapshot-slave! b "a" {}))
                        (identical? b (factory/forget-slave! b "a"))
                        (factory/close! b)])
   :from-string-config (do (vessel-answers! {:success true :result "{\"slaves-detail\":[]}"})
                           (factory/load-slaves
                            (factory/make-bootstrap {} (fn [& _] "emacs"))))
   :throwing-config    (do (vessel-answers! {:success true :result "{\"slaves-detail\":[{\"id\":1}]}"})
                           (factory/load-slaves
                            (factory/make-bootstrap {} (fn [& _] (throw (ex-info "cfg" {}))))))
   :none-source    (refusal #(factory/make-bootstrap {:source :none} no-config))
   :unknown-source (refusal #(factory/make-bootstrap {:source "f00-bogus"} no-config))})

(def ^:private Slaves
  [:vector {:gen/max 5} [:map [:slave-id [:string {:min 1}]] [:status [:enum "idle" "working"]]]])

(defspec reported-slaves-round-trip 100
  (prop/for-all [slaves (mg/generator Slaves)]
    (vessel-answers! {:success true :result (json/write-str {:slaves-detail slaves})})
    (= (map #(update-keys % keyword) slaves)
       (factory/load-slaves (factory/make-bootstrap {:source :emacs} no-config)))))

(deftest default-source-is-emacs-test
  (let [calls (vessel-answers! {:success true :result "{}"})]
    (is (= [] (factory/load-slaves (factory/make-bootstrap {} no-config))))
    (is (= [[:swarm/status 5000]] @calls) "the legacy default, with the default timeout")))
