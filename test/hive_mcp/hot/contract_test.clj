(ns hive-mcp.hot.contract-test
  "Contract facets for src/hive_mcp/hot and its coverage gate.

   The pure strata (claims, reseat plans, keep plans, the core projection) get
   trifectas over their contracts. The boundary fns that would drive clj-reload
   or hive-hot are exercised through what they accept without effect: a
   refused plan (jar roots), empty port maps, a prefix that matches nothing,
   an empty pending queue. Nothing here reloads a namespace.

   The reseat registry is process-global; the tests that touch it save it and
   put it back (own-the-reseat-registry)."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.hot.claims :as claims]
            [hive-mcp.hot.core :as core]
            [hive-mcp.hot.keep :as keep]
            [hive-mcp.hot.registrations :as regs]
            [hive-mcp.hot.reseat :as reseat]
            [hive-mcp.hot.self :as self]
            ;; Loaded so the namespaces the LoadedNs generator names exist in a
            ;; cold image, whatever ran before.
            [hive-mcp.protocols.kg]
            [hive-mcp.protocols.vector]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage hot-contracts-gate
  {:paths ["src/hive_mcp/hot"]
   :exempt {}})

(def ^:private hot-nss
  '[hive-mcp.hot.claims hive-mcp.hot.core hive-mcp.hot.keep hive-mcp.hot.registrations
    hive-mcp.hot.reseat hive-mcp.hot.self])

(deftest every-hot-contract-generates-both-sides
  (doseq [ns-sym hot-nss
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (let [s (m/schema schema)] (if (= :function (m/type s)) (m/children s) [s]))
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

(defn- own-the-reseat-registry [f]
  (let [saved (reseat/registered)]
    (doseq [k (keys saved)] (reseat/unregister-reseater! k))
    (try (f)
         (finally
           (doseq [k (keys (reseat/registered))] (reseat/unregister-reseater! k))
           (doseq [[k v] saved] (reseat/register-reseater! k v))))))

;; ---------------------------------------------------------------------------
;; claims (pure + an injected extend)

(deftrifecta-from-schema base-opts-claims-no-dir
  #'claims/base-opts
  {:in [:cat [:maybe [:sequential :symbol]] [:maybe :int]]
   :out [:map [:dirs [:= []]] [:no-reload [:set :any]]]
   :rel (fn [[nr since] out]
          (and (= (set nr) (:no-reload out)) (= since (:since out))))
   :golden-path "test/golden/hive-mcp/hot/claims-base-opts.edn"})

(deftrifecta-from-schema owner-claims-one-per-sourced-addon
  #'claims/owner-claims
  {:in [:cat claims/HotPlan]
   :out [:vector claims/ExtendRequest]
   :rel (fn [[plan] out]
          (= (count out)
             (count (filter #(get-in % [:hot/source :hot/source-dir]) (:hot/registered plan)))))})

(deftrifecta-from-schema claim-folds-every-answer
  #'claims/claim!
  {:in [:cat claims/Extend [:vector {:gen/max 4} [:map [:owner [:enum :hive.a :hive.b "hive.c"]] [:dirs [:vector {:gen/max 2} [:enum "/a" "/b"]]] [:no-reload [:= #{}]]]]]
   :out claims/ClaimsReport
   :rel (fn [[_ reqs] {:keys [claims errors]}]
          (= (count (distinct (map (comp str :owner) reqs)))
             (count (distinct (concat (keys claims)
                                      (map #(subs % 0 (.indexOf ^String % ": ")) errors))))))})

(deftrifecta-from-schema claims-report-splits-ok-from-err
  #'claims/claims-report
  {:in [:cat [:vector {:gen/max 4} [:map [:owner [:enum :hive.a :hive.b]] [:dirs [:vector {:gen/max 2} [:enum "/a" "/b"]]]]]
        [:vector {:gen/max 4} [:or [:= {} {:added ["/a"]}] [:= {} {}] [:= {} {:error :x :message "gone"}]]]]
   :out claims/ClaimsReport
   :rel (fn [[reqs answers] {:keys [errors]}]
          (= (count errors)
             (count (filter :error (take (count reqs) answers)))))})

;; ---------------------------------------------------------------------------
;; reseat

(deftrifecta-from-schema reseat-plan-keeps-load-order-once
  #'reseat/reseat-plan
  {:in [:cat reseat/Registry reseat/Loaded]
   :out [:vector [:tuple :symbol reseat/GenReseater]]
   :rel (fn [[registry loaded] out]
          (= (map first out)
             (filter #(contains? registry %) (distinct (map symbol loaded)))))})

(deftrifecta-from-schema run-plan-reports-every-entry
  #'reseat/run-plan!
  {:in [:cat [:vector [:tuple [:symbol {:gen/elements '[a b c]}] reseat/GenReseater]] reseat/Loaded]
   :out reseat/ReseatReport
   :rel (fn [[plan] out] (= (map (comp str first) plan) (map :ns out)))})

(deftrifecta-from-schema via-var-follows-the-var
  #'reseat/via-var
  {:in [:cat [:enum 'clojure.core/str 'clojure.core/vector]]
   :out [:fn fn?]
   :rel (fn [[q] out] (= ((resolve q) 1 2) (out 1 2)))
   :num-tests 10})

(deftest registry-lifecycle-owns-its-state
  (own-the-reseat-registry
   (fn []
     (is (= {} (reseat/registered)))
     (is (= 'hive-mcp.x (reseat/register-reseater! 'hive-mcp.x count)))
     (is (= [{:ns "hive-mcp.x" :result 2}] (reseat/reseat! ["hive-mcp.x" "hive-mcp.y"])))
     (is (= [] (reseat/reseat! ["hive-mcp.y"])))
     (is (nil? (reseat/unregister-reseater! 'hive-mcp.x)))
     (is (= {} (reseat/registered))))))

(deftest via-var-names-an-unloaded-var
  (let [f (reseat/via-var 'no.such.namespace/f)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not loaded" (f 1)))))

;; ---------------------------------------------------------------------------
;; registrations

(deftrifecta-from-schema fresh-entry-replaces-only-the-stale-handler
  #'regs/fresh-entry
  {:in [:cat [:or regs/Handler [:map [:handler regs/Handler] [:k :int]]] regs/Handler regs/Handler]
   :out [:maybe regs/Entry]
   :rel (fn [[entry stale fresh] out]
          (cond
            (identical? entry stale) (identical? out fresh)
            (and (map? entry) (identical? (:handler entry) stale)) (= out (assoc entry :handler fresh))
            :else (nil? out)))
   :golden-path "test/golden/hive-mcp/hot/registrations-fresh-entry.edn"})

(deftrifecta-from-schema rebind-entry-of-an-unregistered-id-replaces-nothing
  #'regs/rebind-entry!
  {:in [:cat regs/RegistryKind [:enum ::not-registered] regs/Handler regs/Handler]
   :out :boolean
   :rel (fn [_ out] (false? out))
   :num-tests 20})

(deftest rebind-stale-of-no-namespace-rebinds-nothing
  (is (contains? #{nil {:rebound [] :remaining []}} (regs/rebind-stale! [])))
  (is (contains? #{nil {:rebound [] :remaining []}} (regs/rebind-stale! ['no.such.namespace]))))

;; ---------------------------------------------------------------------------
;; self

(deftrifecta-from-schema defines-protocol-reads-method-metadata
  #'self/defines-protocol?
  {:in [:cat self/LoadedNs]
   :out :boolean
   :rel (fn [[n] out]
          (= out (boolean (some #(:protocol (meta %)) (vals (ns-publics n))))))
   :golden-path "test/golden/hive-mcp/hot/self-defines-protocol.edn"})

(deftrifecta-from-schema protocol-namespaces-are-prefixed-definers
  #'self/protocol-namespaces
  {:in [:cat self/Prefix]
   :out [:set :symbol]
   :rel (fn [[prefix] out]
          (every? #(and (.startsWith (str %) prefix) (self/defines-protocol? %)) out))
   :num-tests 10})

;; ---------------------------------------------------------------------------
;; keep (pure planning, and the boundary on inputs that queue nothing)

(deftrifecta-from-schema keep-plan-keeps-defonces-and-resources
  #'keep/keep-plan
  {:in [:cat [:sequential [:tuple [:enum 'def 'defonce 'defn] [:enum 'a 'b 'c]]]
        [:or [:= {} {'b (atom 1)}] [:= {} {}]] [:enum (constantly true) (constantly false)]]
   :out keep/KeepPlan
   :rel (fn [[defs var-of resource?] out]
          (every? (fn [[tag sym]]
                    (case tag
                      defonce (contains? out sym)
                      defn true
                      def (or (contains? out sym)
                              (not (and (var-of sym) (resource? (var-of sym)))))))
                  defs))
   :golden-path "test/golden/hive-mcp/hot/keep-plan.edn"})

(deftrifecta-from-schema stuck-namespaces-are-loaded-and-not-unloading
  #'keep/stuck-namespaces
  {:in [:cat keep/ReloadState [:set [:enum 'a 'b 'c 'd]]]
   :out [:vector :symbol]
   :rel (fn [[{:keys [to-unload]} loaded] out]
          (every? #(and (contains? loaded %) (not (some #{%} to-unload))) out))})

(deftrifecta-from-schema repair-plan-unloads-every-holder
  #'keep/repair-plan
  {:in [:cat [:map-of [:enum 'h1 'h2] [:sequential [:enum 't1 't2 't3]]] keep/Pred keep/Pred]
   :out [:map [:unload [:vector :symbol]] [:load [:vector :symbol]]]
   :rel (fn [[links] {:keys [unload load]}]
          (and (= (set unload) (set (keys links)))
               (every? (set load) unload)))
   :golden-path "test/golden/hive-mcp/hot/keep-repair-plan.edn"})

(deftrifecta-from-schema dependents-closure-starts-from-the-seeds
  #'keep/dependents-closure
  {:in [:cat keep/ReloadState keep/NsList [:enum (constantly true)] [:maybe [:set [:enum 'a 'c]]]]
   :out [:vector :symbol]
   :rel (fn [[_ nses _ pinned] out]
          (and (= out (distinct out))
               (not-any? (set pinned) out)
               (every? (set out) (remove (set pinned) nses))))})

(deftrifecta-from-schema top-level-defs-reads-tag-and-name
  #'keep/top-level-defs
  {:in [:cat keep/Source]
   :out [:vector keep/TopLevelDef]
   :rel (fn [_ out] (every? (fn [[t s]] (and (symbol? t) (symbol? s))) out))
   :num-tests 20})

(deftrifecta-from-schema live-resource-reads-the-raw-root
  #'keep/live-resource?
  {:in [:cat keep/AVar]
   :out :boolean
   :rel (fn [[v] out] (= out (boolean (and (.hasRoot ^clojure.lang.Var v)
                                           (instance? clojure.lang.IRef (.getRawRoot ^clojure.lang.Var v))))))
   :num-tests 20})

(deftrifecta-from-schema holds-state-of-a-loaded-namespace
  #'keep/holds-state?
  {:in [:cat keep/LoadedNsSym]
   :out :boolean
   :rel (fn [[n] out] (= out (boolean (some keep/live-resource? (vals (ns-interns n))))))
   :num-tests 20})

(deftrifecta-from-schema keeps-for-an-untracked-namespace-is-empty
  #'keep/keeps-for
  {:in [:cat keep/ReloadState keep/NsSym]
   :out keep/KeepPlan
   :rel (fn [_ out] (= {} out))
   :num-tests 20})

(deftrifecta-from-schema dead-links-under-an-unused-prefix-are-none
  #'keep/dead-links
  {:in [:cat [:enum "no.such.prefix" "zz.nothing."]]
   :out keep/DeadLinks
   :rel (fn [_ out] (= {} out))
   :num-tests 5})

(deftrifecta-from-schema repair-preview-under-an-unused-prefix
  #'keep/repair-preview
  {:in [:cat [:enum "no.such.prefix"]]
   :out keep/RepairPreview
   :rel (fn [_ out] (= {} (:dead-links out)))
   :num-tests 5})

(deftrifecta-from-schema inject-keeps-of-untracked-namespaces-keeps-nothing
  #'keep/inject-keeps!
  {:in [:cat [:sequential [:enum 'no.such.namespace 'neither.this]]]
   :out [:map-of :symbol [:vector :symbol]]
   :rel (fn [_ out] (= {} out))
   :num-tests 10})

(deftrifecta-from-schema force-of-nothing-queues-nothing
  #'keep/force-unload!
  {:in [:cat [:= []]]
   :out [:vector :symbol]
   :rel (fn [_ out] (= [] out))
   :num-tests 5})

(deftrifecta-from-schema force-load-of-nothing-queues-nothing
  #'keep/force-load!
  {:in [:cat [:= []]]
   :out [:vector :symbol]
   :rel (fn [_ out] (= [] out))
   :num-tests 5})

(deftest the-clj-reload-boundary-answers-its-shapes
  (testing "release-no-unload! answers nil"
    (is (nil? (keep/release-no-unload!))))
  (testing "pending? reads the queue"
    (is (boolean? (keep/pending?))))
  ;; prepare-pass! and run-pending! are NOT called here: run-pending! drives a
  ;; real clj-reload pass, which unloads every namespace whose file changed
  ;; since the image loaded. Their contracts are checked on the report shapes.
  (testing "the report schemas tell a pass report from a reload answer"
    (let [pass {:forced [] :orphans [] :dead-links {} :unrepairable [] :kept {}}
          answer {:success true :ms 3 :error nil :pending-pass? true :loaded [] :unloaded []}]
      (is (m/validate keep/PassReport pass))
      (is (not (m/validate keep/PassReport answer)))
      (is (m/validate keep/ReloadAnswer answer))
      (is (not (m/validate keep/ReloadAnswer (assoc answer :pending-pass? false)))))))

;; ---------------------------------------------------------------------------
;; core (projection, and the boundary on the refusal and empty-port paths)

(deftrifecta-from-schema interlock-pins-only-protocols
  #'core/interlock
  {:in [:cat core/Classes]
   :out core/Interlock
   :rel (fn [[c] out] (= (set (:protocol c)) (:no-reload out)))
   :golden-path "test/golden/hive-mcp/hot/core-interlock.edn"})

(deftrifecta-from-schema affected-is-the-cascade-minus-the-pinned
  #'core/affected
  {:in [:cat core/ScopePlan core/Classes]
   :out [:vector :any]
   :rel (fn [[sp c] out] (= out (vec (remove (set (:protocol c)) (:cascade sp)))))})

(deftrifecta-from-schema file-ns-maps-under-a-root
  #'core/file->ns
  {:in [:cat core/Roots [:enum "/r/src/hive_mcp/a_b/c.clj" "/o/src/o/b.cljc" "/elsewhere/x.clj"]]
   :out [:maybe :string]
   :rel (fn [[roots f] out]
          (= (some? out) (boolean (some #(.startsWith (.getPath (io/file f)) (str % "/")) roots))))
   :golden-path "test/golden/hive-mcp/hot/core-file-ns.edn"})

(deftrifecta-from-schema plan-report-counts-the-interlock
  #'core/plan-report
  {:in [:cat core/ScopePlan core/Roots core/Classes [:enum (constantly nil)]]
   :out core/PlanReport
   :rel (fn [[sp _ c] out]
          (and (= (count (set (:protocol c))) (get-in out [:interlock :no-reload]))
               (= (empty? (:want sp)) (:unchanged? out))))})

(deftrifecta-from-schema tools-refreshed-prefers-the-tools-port
  #'core/tools-refreshed
  {:in [:cat [:or [:= {} {}] [:= {} {:host/refresh-tools! nil}]] [:enum 42 nil] [:or [:= {} {:server-tools 7}] [:enum nil "x"]]]
   :out [:maybe :int]
   :rel (fn [[ports tools surface] out]
          (= out (if (contains? ports :host/refresh-tools!) tools (when (map? surface) (:server-tools surface)))))
   :golden-path "test/golden/hive-mcp/hot/core-tools-refreshed.edn"})

(deftrifecta-from-schema stale-registrations-trust-the-rescan
  #'core/stale-registrations
  {:in [:cat [:or [:= {} {}] [:= {} {:stale-registrations [:a]}]]
        [:or :nil [:= {} {:remaining []}] [:= {} {:remaining [:b]}] [:= {} {:error "x"}]]]
   :out [:maybe [:vector :keyword]]
   :rel (fn [[res rebind] out]
          (= out (if (and (map? rebind) (contains? rebind :remaining))
                   (not-empty (vec (:remaining rebind)))
                   (:stale-registrations res))))
   :golden-path "test/golden/hive-mcp/hot/core-stale-registrations.edn"})

(deftrifecta-from-schema ns-root-of-a-jar-or-missing-ns-is-nil
  #'core/ns-root
  {:in [:cat [:enum 'malli.core 'no.such.namespace]]
   :out [:maybe :string]
   :rel (fn [_ out] (nil? out))
   :num-tests 5})

(deftrifecta-from-schema core-roots-are-at-most-one
  #'core/core-roots
  {:in [:cat core/NsSym]
   :out [:vector :string]
   :rel (fn [[n] out] (= out (if-let [r (core/ns-root n)] [r] [])))
   :num-tests 10})

(deftrifecta-from-schema defines-record-reads-map-constructors
  #'core/defines-record?
  {:in [:cat [:enum 'hive-mcp.hot.core 'hive-mcp.hot.reseat 'clojure.string]]
   :out :boolean
   :rel (fn [[n] out] (= out (boolean (some #(.startsWith (name %) "map->") (keys (ns-publics n))))))
   :num-tests 10})

(deftrifecta-from-schema classify-splits-the-loaded-namespaces
  #'core/classify
  {:in [:cat core/Prefix]
   :out [:map [:protocol [:set :symbol]] [:state [:set :symbol]] [:record [:set :symbol]]]
   :rel (fn [[prefix] out] (every? #(.startsWith (str %) prefix) (apply concat (vals out))))
   :num-tests 5})

(deftrifecta-from-schema plan-of-jar-roots-is-refused
  #'core/plan
  {:in [:cat [:or [:= {} {:roots []}] [:= {} {:roots [] :ports {}}]]]
   :out core/Refusal
   :rel (fn [_ out] (= :restart-required (:error out)))
   :num-tests 5})

(deftrifecta-from-schema reload-without-hive-hot-is-refused
  #'core/reload!
  {:in [:cat [:= {} {:ports {} :roots ["/r/src"]}]]
   :out [:map [:error :keyword] [:ok? [:= false]]]
   :rel (fn [_ out] (= :hive-hot-absent (:error out)))
   :num-tests 5})

(deftest default-ports-name-every-effect
  (let [ports (core/default-ports)]
    (is (m/validate core/Ports ports))
    (is (contains? ports :host/remount!) "named, though only the hot tool can fill it")
    (is (nil? (:host/remount! ports)))
    (is (ifn? (:host/reseat! ports)))))

(golden/deftest-golden reload-refusals-golden
  "test/golden/hive-mcp/hot/core-refusals.edn"
  {:plan-jar (core/plan {:roots []})
   :reload-jar (core/reload! {:roots []})
   :reload-no-hot (core/reload! {:ports {} :roots ["/r/src"]})})

(deftest report-arms-validate-only-as-themselves
  (is (m/validate core/Refusal {:error :hive-hot-absent :message "m"}))
  (is (not (m/validate core/ReloadReport {:error :hive-hot-absent :message "m" :ok? false})))
  (is (not (m/validate core/Refusal {:ok? true :pass :none}))))
