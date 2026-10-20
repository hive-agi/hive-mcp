(ns hive-mcp.spi.ports-contract-test
  "Contract facets for the host-local kernel ports (disc, emacs, metrics,
   session) and the two registries (catchup blocks, contributions,
   op-schemas), plus the contract-coverage gate for spi/.

   Every port test installs a recording stub and uninstalls it after, so no
   late-bound host namespace is reached. Every registry test snapshots the
   process-global registry it drives and restores it, so the goldens do not
   depend on test order and the boot-time contributions survive."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.spi.catchup-registry :as cr]
            [hive-mcp.spi.contributions :as con]
            [hive-mcp.spi.disc :as disc]
            [hive-mcp.spi.emacs :as emacs]
            [hive-mcp.spi.metrics :as metrics]
            [hive-mcp.spi.op-schema-registry :as opreg]
            [hive-mcp.spi.session :as session]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.schema.registry :as reg]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-registries
  "Snapshot every registry and port this namespace drives; restore after."
  [f]
  (let [blocks (cr/registered-blocks)
        contribs @@#'con/registry
        owners (opreg/owned)]
    (try
      (cr/reset-registry!)
      (f)
      (finally
        (cr/reset-registry!)
        (run! cr/register-block! blocks)
        (reset! @#'con/registry contribs)
        (opreg/reset-for-test!)
        (reset! @#'opreg/owners* owners)
        (disc/uninstall!) (emacs/uninstall!) (metrics/uninstall!) (session/uninstall!)))))

(use-fixtures :each own-registries)

;;; ---------------------------------------------------------------------------
;;; Catchup registry
;;; ---------------------------------------------------------------------------

(deftrifecta-from-schema block-shape-predicate
  #'cr/block?
  {:in [:cat [:or cr/BlockValue :map :nil
              [:map [:block/id :string] [:block/fn [:= {} identity]] [:block/order :int]]]]
   :out :boolean
   :rel (fn [[x] out]
          (= out (boolean (and (map? x) (keyword? (:block/id x))
                               (fn? (:block/fn x)) (integer? (:block/order x))))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 200})

(golden/deftest-golden catchup-compose-golden
  "test/golden/hive-mcp/spi/catchup-compose.edn"
  (let [ok   {:block/id :b/ok :block/fn (fn [ctx] {:n (:n ctx)}) :block/order 2}
        boom {:block/id :b/boom :block/fn (fn [_] (throw (ex-info "boom" {}))) :block/order 1}
        nada {:block/id :b/nil :block/fn (constantly nil) :block/order 2}]
    (run! cr/register-block! [ok boom nada])
    (let [composed (cr/compose {:n 3})
          ordered  (mapv :block/id (cr/registered-blocks))
          run-with (cr/compose {:n 3} (fn [b _] (:block/order b)))
          removed  (cr/unregister-block! :b/boom)]
      {:composed composed :ordered ordered :custom-runner run-with
       :removed removed :still? [(cr/registered? :b/ok) (cr/registered? :b/boom)]
       :refused (try (cr/register-block! {:block/id "x"}) :accepted
                     (catch Exception _ :refused))})))

;;; ---------------------------------------------------------------------------
;;; Contributions
;;; ---------------------------------------------------------------------------

(golden/deftest-golden contributions-lifecycle-golden
  "test/golden/hive-mcp/spi/contributions.edn"
  (let [ran (atom [])]
    (con/reset-kind! ::k)
    (con/contribute! ::k :a {:install! #(swap! ran conj :a)})
    (con/contribute! ::k :b {:install! #(throw (ex-info "b fails" {}))})
    (con/contribute! ::k :c {:payload 1})
    (con/contribute! ::k :a {:install! #(swap! ran conj :a2)})
    (let [order   (mapv key (con/ordered ::k))
          report  (update (con/register-all! ::k) :failed #(into {} (map (fn [[k t]] [k (ex-message t)])) %))
          entry-c (dissoc (con/entry ::k :c) :contrib/seq)
          _       (con/forget! ::k :c)
          after   (set (keys (con/contributed ::k)))
          _       (con/reset-kind! ::k)]
      {:order order :report report :ran @ran :entry-c entry-c
       :after-forget after :after-reset (con/contributed ::k)
       :absent-manifest (con/read-manifest "hive-mcp/no-such-manifest.edn")
       :load-absent (con/load-manifest! "hive-mcp/no-such-manifest.edn")})))

(deftest a-real-manifest-loads-with-its-contracted-shape
  (let [report (con/load-manifest! "hive-mcp/tool-contributions.edn" {:resolve-keys #{} :required-key :none})]
    (is (m/validate con/LoadReport report))))

;;; ---------------------------------------------------------------------------
;;; Op-schema registry
;;; ---------------------------------------------------------------------------

(golden/deftest-golden op-schema-ownership-golden
  "test/golden/hive-mcp/spi/op-schema-registry.edn"
  (let [bundle {:test.m20/alpha [:map [:x :int]]}]
    (opreg/reset-for-test!)
    (try
      {:registered (opreg/register-by-key! :addon/a :op-schema/a bundle)
       :malformed  (opreg/register-by-key! :addon/a :op-schema/a [:not-a-map])
       :owned      (opreg/owned)
       :removed    (opreg/deregister-by-owner! :addon/a)
       :again      (opreg/deregister-by-owner! :addon/a)
       :reset      (opreg/reset-for-test!)}
      (finally (reg/deregister-all! (keys bundle))))))

;;; ---------------------------------------------------------------------------
;;; Ports — one recording stub each
;;; ---------------------------------------------------------------------------

(golden/deftest-golden ports-delegate-to-the-installed-impl-golden
  "test/golden/hive-mcp/spi/ports-delegate.edn"
  (let [calls (atom [])
        rec   (fn [tag ret] (fn [& args] (swap! calls conj (into [tag] args)) ret))
        d (reify disc/IDiscKnowledge
            (-staleness-value [_ r] ((rec :disc/value 5.0) r))
            (-propagate-staleness! [_ p v r] ((rec :disc/propagate {:propagated 1}) p v r))
            (-top-stale-files [_ o] ((rec :disc/top []) o)))
        e (reify emacs/IEmacsDaemons
            (-ensure-default-daemon! [_] ((rec :emacs/ensure :d1)))
            (-select-daemon-for-ling [_ l] ((rec :emacs/select {:daemon-id :d1 :reason :test}) l))
            (-bind-ling! [_ d s] ((rec :emacs/bind true) d s))
            (-unbind-ling! [_ d s] ((rec :emacs/unbind true) d s))
            (-get-daemon-for-ling [_ s] ((rec :emacs/get :d1) s))
            (-default-daemon-id [_] ((rec :emacs/default :d1))))
        mt (reify metrics/IMetrics
             (-inc-events! [_ t s] ((rec :metrics/inc nil) t s))
             (-observe-request-duration! [_ t s] ((rec :metrics/observe nil) t s))
             (-set-lings-active! [_ n] ((rec :metrics/lings nil) n))
             (-handle-effect! [_ e] ((rec :metrics/effect nil) e)))
        s (reify session/ISessionLifecycle
            (-record-session-start! [_ a] ((rec :session/start nil) a))
            (-register-session-hooks! [_ r] ((rec :session/hooks nil) r)))
        installed [(identical? d (disc/install! d)) (identical? e (emacs/install! e))
                   (identical? mt (metrics/install! mt)) (identical? s (session/install! s))]
        answers {:disc   [(disc/staleness-value :hash-mismatch)
                          (disc/propagate-staleness! "f.clj" 5.0 :hash-mismatch)
                          (disc/top-stale-files {:n 2})]
                 :emacs  [(emacs/ensure-default-daemon!) (emacs/select-daemon-for-ling "l")
                          (emacs/bind-ling! :d1 "l") (emacs/unbind-ling! :d1 "l")
                          (emacs/get-daemon-for-ling "l") (emacs/default-daemon-id)]
                 :metrics [(metrics/inc-events! :e/x :info)
                           (metrics/observe-request-duration! "t" 0.5)
                           (metrics/set-lings-active! 3)
                           (metrics/handle-effect! {:counter :c})]
                 :session [(session/record-session-start! "a")
                           (session/register-session-hooks! {:hooks []})]}
        current [(identical? d (disc/current)) (identical? e (emacs/current))
                 (identical? mt (metrics/current)) (identical? s (session/current))]
        uninstalled [(disc/uninstall!) (emacs/uninstall!) (metrics/uninstall!) (session/uninstall!)
                     (disc/reset-cache!) (emacs/reset-cache!) (metrics/reset-cache!) (session/reset-cache!)]]
    {:installed installed :answers answers :current current
     :calls @calls :uninstalled uninstalled
     :fallback-is-not-the-stub [(identical? d (disc/current)) (identical? e (emacs/current))]}))

(deftest noops-answer-their-contracted-absences
  (doseq [[port impl] [[disc/install! disc/noop] [emacs/install! emacs/noop]
                       [metrics/install! metrics/noop] [session/install! session/noop]]]
    (port impl))
  (is (nil? (disc/staleness-value :hash-mismatch)))
  (is (= [] (disc/top-stale-files {:n 1})))
  (is (nil? (emacs/select-daemon-for-ling "l")))
  (is (nil? (metrics/inc-events! :e/x :warn)))
  (is (nil? (session/record-session-start! nil))))

;;; ---------------------------------------------------------------------------
;;; Coverage gate for spi/ (spi/kanban has its own)
;;; ---------------------------------------------------------------------------

(def ^:private spi-nss
  '[hive-mcp.spi.catchup-registry hive-mcp.spi.contributions hive-mcp.spi.disc
    hive-mcp.spi.emacs hive-mcp.spi.kanban hive-mcp.spi.metrics
    hive-mcp.spi.op-schema-registry hive-mcp.spi.session])

(run! require (conj spi-nss 'hive-mcp.spi.kanban.registry))

(coverage/deftest-contract-coverage spi-contracts
  {:paths ["src/hive_mcp/spi"]})

(deftest every-spi-contract-generates-both-sides
  (doseq [ns-sym spi-nss
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (testing (str ns-sym "/" fn-sym)
      (is (= 3 (count (mg/sample side {:size 3 :seed 7})))))))
