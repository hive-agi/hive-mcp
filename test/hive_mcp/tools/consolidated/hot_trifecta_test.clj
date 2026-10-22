(ns hive-mcp.tools.consolidated.hot-trifecta-test
  "Schema-derived trifectas over the PURE helpers of the `hot` tool. The
   bridge-driven handlers are pinned by hot-test with injected HostPorts; here
   every subject is a value-in, value-out fn, so each gets a property, a
   golden and the schema-derived mutants."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.consolidated.hot :as hot]
            [hive-mcp.tools.core :as tc]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def PlugOutParams
  [:map
   [:cascade {:optional true} [:maybe [:or :boolean [:enum "true"]]]]
   [:force {:optional true} [:maybe :boolean]]
   [:drain_ms {:optional true} [:maybe [:or [:int {:min -5 :max 5000}] [:enum "100"]]]]])

(deftrifecta-from-schema plug-out-opts-contract
  hive-mcp.tools.consolidated.hot/plug-out-opts
  {:in          PlugOutParams
   :out         hot/PlugOutOpts
   :rel         (fn [{:keys [cascade force drain_ms]} out]
                  (and (= (true? cascade) (:cascade? out))
                       (= (true? force) (:force? out))
                       (= (when (pos-int? drain_ms) drain_ms) (:drain-ms out))))
   :golden-path "test/golden/tools/consolidated/hot-plug-out-opts.edn"
   :num-tests   200})

(def Pins [:maybe [:vector {:max 4} [:or :symbol [:enum "a.ns" "b.ns"]]]])

(deftrifecta-from-schema no-reload-split-contract
  hive-mcp.tools.consolidated.hot/no-reload-split
  {:in          [:cat Pins Pins]
   :out         hot/NoReloadSplit
   :rel         (fn [[core addon] out]
                  (and (= (:effective out) (vec (sort (set (map str (concat core addon))))))
                       (= (:core out) (vec (sort (set (map str core)))))
                       (= (count (:effective out)) (get-in out [:counts :effective]))))
   :golden-path "test/golden/tools/consolidated/hot-no-reload-split.edn"
   :num-tests   200})

(def EjectParams
  [:map
   [:addon {:optional true} [:maybe [:enum "hive.rss" "hive.carto"]]]
   [:path {:optional true} [:maybe [:enum "/opt/addon" "/tmp/x.jar"]]]])

(deftrifecta-from-schema eject-target-contract
  hive-mcp.tools.consolidated.hot/eject-target
  {:in          EjectParams
   :out         [:maybe :string]
   :rel         (fn [{:keys [addon path]} out] (= out (or addon path)))
   :golden-path "test/golden/tools/consolidated/hot-eject-target.edn"
   :num-tests   100})

(defn- shape-pin-params
  "Bend a drawn `params` map to the chosen classification `arm`, keeping the
   drawn values wherever they already fit it. The bad-policy arm also drops a
   non-positive idle_ms, so the policy refusal is the one that answers."
  [[params arm valid bad-idle good-idle]]
  (let [ok-policy? (contains? #{"eager" "lazy" "pinned" nil} (:policy params))
        fix-policy #(cond-> % (not ok-policy?) (assoc :policy valid))]
    (case arm
      :bad-policy    (cond-> (assoc params :policy "bogus")
                       (not (pos-int? (:idle_ms params))) (dissoc :idle_ms))
      :bad-idle      (-> params fix-policy (assoc :idle_ms bad-idle))
      :explicit-idle (-> params fix-policy (assoc :idle_ms good-idle))
      :baseline-idle (-> params fix-policy (dissoc :idle_ms)))))

(def PinParams
  "idle_ms leans to non-positive values so the bad-idle refusal is reached as
   often as the accepting arms.

   A uniform draw still leaves :bad-idle (a VALID policy beside a present,
   non-positive idle_ms) near a tenth of the draws, close enough to the floor
   that some seeds starve it. A classification arm drawn beside the params
   bends each draw into its class deliberately, so all four classes sit near
   a quarter of the samples."
  [:map {:gen/schema [:tuple
                      [:map
                       [:policy {:optional true} [:maybe [:enum "eager" "lazy" "pinned" "bogus" ""]]]
                       [:idle_ms {:optional true} [:maybe [:or [:int {:min -10 :max 0}] [:int {:min 1 :max 100000}]]]]]
                      [:enum :bad-policy :bad-idle :explicit-idle :baseline-idle]
                      [:enum "eager" "lazy" "pinned"]
                      [:int {:min -10 :max 0}]
                      [:int {:min 1 :max 100000}]]
         :gen/fmap shape-pin-params}
   [:policy {:optional true} [:maybe [:enum "eager" "lazy" "pinned" "bogus" ""]]]
   [:idle_ms {:optional true} [:maybe [:or [:int {:min -10 :max 0}] [:int {:min 1 :max 100000}]]]]])

(defn pin-decl-of
  "Unary adapter: pin-decl over the fallback policy enum and a 1000 ms baseline."
  [params]
  (hot/pin-decl params [:enum :eager :lazy :pinned] 1000))

(deftrifecta-from-schema pin-decl-contract
  hive-mcp.tools.consolidated.hot-trifecta-test/pin-decl-of
  {:in          PinParams
   :out         hot/PinDecl
   :rel         (fn [{:keys [policy idle_ms]} out]
                  (let [p (or policy "pinned")]
                    (cond
                      (not (#{"eager" "lazy" "pinned"} p)) (str/includes? (str (:error out)) "rejected")
                      (and (some? idle_ms) (not (pos-int? idle_ms))) (str/includes? (str (:error out)) "idle_ms")
                      :else (= out {:policy (keyword p) :idle-ms (or idle_ms 1000)}))))
   :classify    (fn [{:keys [policy idle_ms]} out]
                  (cond (str/includes? (str (:error out)) "rejected") :bad-policy
                        (:error out) :bad-idle
                        (some? idle_ms) :explicit-idle
                        :else :baseline-idle))
   :classify-domain #{:bad-policy :bad-idle :explicit-idle :baseline-idle}
   :golden-path "test/golden/tools/consolidated/hot-pin-decl.edn"
   :num-tests   300})

(def ReloadBase [:map [:mount-opts {:optional true} [:map [:x :int]]]])

(deftrifecta-from-schema reload-call-opts-contract
  hive-mcp.tools.consolidated.hot/reload-call-opts
  {:in          [:cat ReloadBase [:maybe [:or :boolean [:enum "true" "false" "yes"]]]]
   :out         :map
   :rel         (fn [[base force] out]
                  (if (contains? #{true "true"} force)
                    (= out (assoc base :force? true :ns-reloaded? false))
                    (= out base)))
   :golden-path "test/golden/tools/consolidated/hot-reload-call-opts.edn"
   :num-tests   200})

(def EvictionReport
  [:or
   [:map [:refused? {:optional true} :boolean]
    [:evicted? {:optional true} :boolean]
    [:reason {:optional true} [:maybe [:enum "pinned" "busy"]]]]
   :nil
   :string])

(deftrifecta-from-schema eviction-outcome-contract
  hive-mcp.tools.consolidated.hot/eviction-outcome
  {:in          EvictionReport
   :out         :any
   :rel         (fn [r out]
                  (if (and (map? r) (or (:refused? r)
                                        (and (false? (:evicted? r)) (some? (:reason r)))))
                    (= out (assoc r :ok? false :refused? true))
                    (= out r)))
   :mutation    false ;; :out is :any (a non-map report passes through), so
                      ;; malli derives no output mutants; the golden and :rel
                      ;; carry the teeth.
   :golden-path "test/golden/tools/consolidated/hot-eviction-outcome.edn"
   :num-tests   300})

(def PlugOutResult
  [:or
   [:map [:ok [:map [:hot/ejected [:vector :string]]]]]
   [:map [:error [:enum :hot/eject-refused :hot/eject-unknown :hot/eject-failed]]
    [:message {:optional true} [:maybe :string]]]
   [:map [:hot/ejected [:vector :string]] [:ok? :boolean]]])

(deftrifecta-from-schema unmount-outcome-contract
  hive-mcp.tools.consolidated.hot/unmount-outcome
  {:in          PlugOutResult
   :out         hot/UnmountOutcome
   :rel         (fn [res out]
                  (cond
                    (contains? res :ok) (= out (:ok res))
                    (keyword? (:error res))
                    (and (false? (:ok? out))
                         (= (:reason out) (subs (str (:error res)) 1))
                         (= (= :hot/eject-refused (:error res)) (true? (:hot/refused? out)))
                         (some? (:message out)))
                    :else (= out res)))
   :classify    (fn [res _] (cond (contains? res :ok) :ok
                                  (:error res) :err
                                  :else :bare))
   :classify-domain #{:ok :err :bare}
   :mutation    false ;; UnmountOutcome ends in a pass-through :any arm, so malli
                      ;; derives no output mutants; :rel and the golden, which
                      ;; cover all three arms, carry the teeth.
   :golden-path "test/golden/tools/consolidated/hot-unmount-outcome.edn"
   :num-tests   300})

(def SkippedSource
  [:map [:addon/id [:enum "a" "b" "c"]] [:hot/source-kind [:enum :jar :absent :weird]]])

(deftrifecta-from-schema reload-all-seeds-contract
  hive-mcp.tools.consolidated.hot/reload-all-seeds
  {:in          [:map
                 [:hot/registered [:vector {:max 3} [:map [:addon/id [:enum "x" "y" "z"]]]]]
                 [:hot/skipped [:vector {:max 3} SkippedSource]]]
   :out         hot/ReloadAllSeeds
   :rel         (fn [plan out]
                  (and (= (:seeds out) (set (map :addon/id (:hot/registered plan))))
                       (= (map :addon/id (:skipped out)) (map :addon/id (:hot/skipped plan)))
                       (every? #(seq (:reason %)) (:skipped out))))
   :golden-path "test/golden/tools/consolidated/hot-reload-all-seeds.edn"
   :num-tests   200})

(deftest unsupported-report-names-the-bridge
  (let [r (hot/unsupported-report 'hive-addon.hot/m27-absent!)]
    (is (= "hive-addon.hot/m27-absent!" (:bridge r)))
    (is (false? (:ok? r)))))

(deftest accepts-arity-reads-arglists
  (is (true? (hot/accepts-arity? #'hot/pin-decl 3)))
  (is (false? (hot/accepts-arity? #'hot/pin-decl 2))))

(deftest report-with-surface-never-claims-a-fresh-surface
  (is (true? (:surface-stale? (hot/report-with-surface {:ok? true} nil))))
  (is (false? (:ok? (hot/report-with-surface {:ok? true} {:server-tools {:error "x"}}))))
  (let [r (hot/report-with-surface {:ok? true} {:server-tools {:ok {:surfaces [:tools]}}
                                                :agent-tools {:ok? true}})]
    (is (true? (:ok? r)))
    (is (nil? (:surface-stale? r)))))

(deftest run-host-verb-over-stub-ports
  (let [ports {:host (constantly ::host) :specs (constantly [])
               :reload-opts (constantly {}) :refresh! (constantly {:stub true})}
        verb  {:bridge 'hive-addon.hot.inject/m27-never-defined!
               :target :addon :report-keys [:ok?]}]
    (is (true? (:isError (hot/run-host-verb verb ports {}))) "no target is an error")
    (let [r (hot/run-host-verb verb ports {:addon "hive.rss"})]
      (is (m/validate tc/McpResponse r))
      (is (str/includes? (:text r) "unsupported") "an absent bridge answers unsupported-report"))))

(deftest command-enum-ends-with-help
  (let [e (hot/command-enum)]
    (is (= "help" (peek e)))
    (is (= (butlast e) (sort (butlast e))))))
