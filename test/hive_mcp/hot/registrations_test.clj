(ns hive-mcp.hot.registrations-test
  "A core reload re-points every fx / cofx / event registration owned by a
   reloaded namespace at the fresh var value, so :stale-registrations comes
   back empty. The reload itself is simulated: a probe namespace registers
   its handlers by VALUE (the shape a defonce-gated registrar leaves behind),
   then its defns are re-evaluated the way clj-reload re-loads them, and the
   registrar is NOT re-run."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive.events.cofx :as cofx]
            [hive.events.fx :as fx]
            [hive.events.router :as router]
            [hive.events.staleness :as staleness]
            [hive-mcp.hot.core :as core]
            [hive-mcp.hot.registrations :as registrations]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def probe-ns 'hive-mcp.hot.registrations-test.probe)

(defn- isolate-registries [t]
  (let [fx0 (fx/registry-snapshot) cofx0 (cofx/registry-snapshot) ev0 (router/registry-snapshot)]
    (try (t)
         (finally
           (fx/restore-registry! fx0)
           (cofx/restore-registry! cofx0)
           (router/restore-registry! ev0)
           (remove-ns probe-ns)))))

(use-fixtures :each isolate-registries)

(defn- load-probe!
  "Evaluate the probe namespace's handlers; `tag` is what they answer, so a
   second load is observably new code."
  [tag]
  (binding [*ns* (create-ns probe-ns)]
    (refer-clojure)
    (eval `(defn ~'handle-fx [~'v] [~tag ~'v]))
    (eval `(defn ~'handle-cofx [~'c] (assoc ~'c :probe ~tag)))
    (eval `(defn ~'handle-event [~'_cofx ~'_ev] {:probe ~tag}))))

(defn- register-probe!
  "The registrar: by value, run once (the defonce-gated shape)."
  []
  (let [v #(deref (ns-resolve probe-ns %))]
    (fx/reg-fx ::probe-fx (v 'handle-fx))
    (cofx/reg-cofx ::probe-cofx (v 'handle-cofx))
    (router/reg-event-fx ::probe-event [] (v 'handle-event))))

(defn- reload-probe!
  "What a core reload does to a namespace whose registrar is gated: the code
   is re-loaded, the registration is not re-run."
  []
  (load-probe! :new))

(defn- stale-ids []
  (set (map :id (staleness/stale-entries {:namespaces [probe-ns]}))))

(defn- ports
  "Stub hive-hot ports whose pass reloads the probe namespace; the rebind port
   is the real default."
  []
  {:hot/ensure-init!      (fn [_] {:initialized? true})
   :hot/status            (fn [] {:dirs ["/r/src"]})
   :hot/scope-plan        (fn [_] {:want [(io/file "/r/src/hive_mcp/hot/registrations_test/probe.clj")]
                                   :cascade [probe-ns]})
   :hot/repair-preview    (fn [_] {})
   :hot/prepare-pass!     (fn [_ _] {:forced [] :orphans [] :kept {}})
   :hot/reload-scoped!    (fn [_]
                            (reload-probe!)
                            {:success true :loaded [probe-ns] :unloaded [probe-ns]
                             :stale-registrations (seq (staleness/stale-entries {:namespaces [probe-ns]}))})
   :host/rebind-registrations! (:host/rebind-registrations! (core/default-ports))})

(deftest the-simulated-reload-leaves-the-registries-stale
  (load-probe! :old)
  (register-probe!)
  (is (empty? (stale-ids)) "freshly registered: nothing is stale")
  (reload-probe!)
  (is (= #{::probe-fx ::probe-cofx ::probe-event} (stale-ids))
      "the precondition the fix exists for: every registration still runs the old fn")
  (is (= [:old 1] ((fx/get-fx ::probe-fx) 1))))

(deftest a-core-reload-of-a-registering-namespace-reports-no-stale-registrations
  (load-probe! :old)
  (register-probe!)
  (let [report (core/reload! {:ports (ports) :roots ["/r/src"]})]
    (is (true? (:ok? report)))
    (is (empty? (:stale-registrations report)))
    (is (= #{::probe-fx ::probe-cofx ::probe-event}
           (set (map :id (:rebound-registrations report)))))
    (testing "the registries now invoke the reloaded code"
      (is (empty? (stale-ids)))
      (is (= [:new 1] ((fx/get-fx ::probe-fx) 1)))
      (is (= {:probe :new} ((cofx/get-cofx ::probe-cofx) {})))
      (is (= {:probe :new} ((:handler (router/get-event ::probe-event)) {} [::probe-event]))))))

(deftest a-rebind-leaves-an-entry-somebody-re-registered-alone
  (load-probe! :old)
  (register-probe!)
  (reload-probe!)
  (let [mine (fn [v] [:mine v])]
    (fx/unreg-fx ::probe-fx)
    (fx/reg-fx ::probe-fx mine)
    (registrations/rebind-stale! [probe-ns])
    (is (identical? mine (fx/get-fx ::probe-fx)))))

(deftest only-the-loaded-namespaces-are-rebound
  (load-probe! :old)
  (register-probe!)
  (reload-probe!)
  (is (= {:rebound [] :remaining []} (registrations/rebind-stale! ['hive-mcp.elsewhere])))
  (is (= 3 (count (stale-ids))) "the probe namespace was not named, so it is untouched"))

(deftest the-pure-pieces
  (let [old (fn [] :old) new (fn [] :new) other (fn [] :other)]
    (is (identical? new (registrations/fresh-entry old old new)))
    (is (= {:interceptors [] :handler new}
           (registrations/fresh-entry {:interceptors [] :handler old} old new)))
    (is (nil? (registrations/fresh-entry other old new)))
    (is (= [:a] (core/stale-registrations {:stale-registrations [:a]} nil)))
    (is (= [:a] (core/stale-registrations {:stale-registrations [:a]} {:error "boom"})))
    (is (nil? (core/stale-registrations {:stale-registrations [:a]} {:rebound [] :remaining []})))))
