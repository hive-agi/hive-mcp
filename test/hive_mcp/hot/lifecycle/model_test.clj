(ns hive-mcp.hot.lifecycle.model-test
  "Pins the reloadable-core lifecycle contract with hive-test's trifecta.

   The subject is the runner driving a host through the IHotHost port; the
   host is the model stub. Golden scripts fix what a client sees for each
   named scenario, the property facet runs random scripts against every
   registered invariant, and the mutation facet proves the golden cases catch
   a host that breaks the contract. Mutant hosts are DECORATORS over the port
   (no with-redefs): each breaks exactly one promise."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.hot.lifecycle.domain :as d]
            [hive-mcp.hot.lifecycle.model :as model]
            [hive-mcp.hot.lifecycle.model-host :refer [model-host]]
            [hive-mcp.hot.lifecycle.port :as port]
            [hive-mcp.hot.lifecycle.runner :as runner]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

;; ── fixture: a host with every kind of addon the contract distinguishes ──

(def spec
  {:spec/core-tools  #{"code" "hot" "memory"}
   :spec/addon-tools {"a.tools" #{"alpha"}
                      "b.pinned" #{"beta"}
                      "c.hooks" #{}
                      "e.base" #{"epsilon"}}
   :spec/hook-only   #{"c.hooks"}
   :spec/policy      {"a.tools" :eager "b.pinned" :pinned "c.hooks" :eager "e.base" :eager}
   :spec/injectable  {"f.extra" #{"phi"}}
   :spec/deps        {"a.tools" #{"e.base"}}})

(defn- op
  ([kind] {:op/kind kind})
  ([kind addon] {:op/kind kind :op/addon addon})
  ([kind addon more] (merge (op kind addon) more)))

(defn trace-of
  "Subject: run SCRIPT through the port against HOST-FN's host for `spec`."
  ([script] (trace-of model-host script))
  ([host-fn script] (runner/run-script (host-fn spec) script)))

;; ── mutant hosts: decorators over the port, one broken promise each ─────

(defn- decorate
  "A host that delegates to INNER except where OVERRIDES says otherwise.
   OVERRIDES: {:apply-op! (fn [inner op] ...) :observe (fn [inner] ...)}."
  [inner {:keys [apply-op! observe]}]
  (reify port/IHotHost
    (apply-op! [_ o] (if apply-op! (apply-op! inner o) (port/apply-op! inner o)))
    (observe [_] (if observe (observe inner) (port/observe inner)))))

(defn- drops-dormant-stubs
  "Evicted addons stop advertising their tools."
  [s]
  (let [inner (model-host s)]
    (decorate inner
              {:observe (fn [h]
                          (let [{:obs/keys [phases] :as o} (port/observe h)
                                gone (into #{} (comp (filter (fn [[_ p]] (= :dormant p)))
                                                     (mapcat (fn [[id _]] (get-in s [:spec/addon-tools id]))))
                                           phases)]
                            (update o :obs/tools #(vec (remove gone %)))))})))

(defn- core-reload-remounts-everything
  "A core reload silently re-activates dormant addons."
  [s]
  (let [state (atom (model/initial-state s))
        inner (model-host s)]
    (decorate inner
              {:apply-op! (fn [h o]
                            (let [outcome (port/apply-op! h o)]
                              (when (= :core-reload (:op/kind o))
                                (reset! (:state h) (model/initial-state s)))
                              outcome))})))

(defn- ignores-pins
  "Pinned addons are evicted like any other."
  [s]
  (model-host (assoc s :spec/policy (update-vals (:spec/policy s) (constantly :eager)))))

(defn- evicts-hook-only
  "Hook-only addons are evicted, losing their hooks with no stub to bring them back."
  [s]
  (model-host (assoc s :spec/hook-only #{})))

(defn- duplicates-a-tool
  "The tool table advertises one addon tool twice."
  [s]
  (decorate (model-host s)
            {:observe (fn [h] (update (port/observe h) :obs/tools conj "alpha"))}))

(defn- eject-leaves-tools
  "An ejected addon keeps advertising its tools."
  [s]
  (decorate (model-host s)
            {:observe (fn [h]
                        (let [{:obs/keys [phases] :as o} (port/observe h)
                              ejected (remove #(contains? phases %) (keys (:spec/addon-tools s)))
                              stale   (mapcat #(get-in s [:spec/addon-tools %]) ejected)]
                          (update o :obs/tools #(vec (sort (into (set %) stale))))))}))

(defn- inject-duplicates
  "An injected addon's tools are registered a second time next to the old
   entries, so the table advertises them twice."
  [s]
  (let [injected (atom #{})]
    (decorate (model-host s)
              {:apply-op! (fn [h o]
                            (let [outcome (port/apply-op! h o)]
                              (when (and (= :inject (:op/kind o)) (= :applied outcome))
                                (swap! injected conj (:op/addon o)))
                              outcome))
               :observe (fn [h]
                          (let [{:obs/keys [phases] :as o} (port/observe h)
                                twice (mapcat #(get (model/catalog s) %)
                                              (filter #(contains? phases %) @injected))]
                            (update o :obs/tools into twice)))})))

(defn- pin-ignores-no-surface
  "Every pin is applied as if forced, so a hook-only addon goes :lazy."
  [s]
  (decorate (model-host s)
            {:apply-op! (fn [h o]
                          (port/apply-op! h (cond-> o (= :pin (:op/kind o)) (assoc :op/force? true))))}))

;; ── the trifecta ──────────────────────────────────────────────────────────

(def addon-ids
  "Every addon an op may name: the fixture's catalog plus one it never heard of."
  (conj (vec (sort (keys (model/catalog spec)))) "z.unknown"))

(defmulti gen-of
  "The generator of ops of KIND. OPEN: a new op kind joins the property facet
   as one `defmethod`; `gen-op` draws from every registered kind."
  identity)

(defmethod gen-of :core-reload [k] (gen/return (op k)))

(defn- gen-addon-op [k] (gen/fmap #(op k %) (gen/elements addon-ids)))

(defmethod gen-of :addon-reload [k] (gen-addon-op k))
(defmethod gen-of :evict [k] (gen-addon-op k))
(defmethod gen-of :activate [k] (gen-addon-op k))
(defmethod gen-of :call [k] (gen-addon-op k))
(defmethod gen-of :inject [k] (gen-addon-op k))

(defmethod gen-of :pin [k]
  (gen/fmap (fn [[a p f]] (op k a {:op/policy p :op/force? f}))
            (gen/tuple (gen/elements addon-ids) (gen/elements [:eager :lazy :pinned]) gen/boolean)))

(defmethod gen-of :eject [k]
  (gen/fmap (fn [[a c]] (op k a {:op/cascade? c}))
            (gen/tuple (gen/elements addon-ids) gen/boolean)))

(defmethod gen-of :unmount [k]
  (gen/fmap (fn [[a c]] (op k a {:op/cascade? c}))
            (gen/tuple (gen/elements addon-ids) gen/boolean)))

(def gen-op
  (gen/one-of (mapv gen-of (sort (keys (methods gen-of))))))

(def gen-script (gen/vector gen-op 0 12))

(def golden-cases
  "Named scripts whose traces are pinned in the golden file."
  {:core-reload-with-addons-mounted [(op :core-reload) (op :core-reload)]
   :evict-then-call-remounts        [(op :evict "a.tools") (op :call "a.tools")]
   :evict-then-activate-roundtrip   [(op :evict "a.tools") (op :activate "a.tools") (op :activate "a.tools")]
   :pinned-refuses-eviction         [(op :evict "b.pinned")]
   :hook-only-refuses-eviction      [(op :evict "c.hooks")]
   :reload-needs-a-mounted-addon    [(op :evict "a.tools") (op :addon-reload "a.tools")]
   :core-reload-while-dormant       [(op :evict "a.tools") (op :core-reload) (op :activate "a.tools")]
   :eject-then-call-refused         [(op :eject "a.tools") (op :call "a.tools") (op :evict "a.tools")]
   :eject-then-inject-restores      [(op :eject "a.tools") (op :inject "a.tools") (op :inject "a.tools")]
   :eject-an-unknown-addon-refused  [(op :eject "z.unknown") (op :eject "a.tools") (op :eject "a.tools")]
   :inject-an-injectable            [(op :inject "f.extra") (op :evict "f.extra") (op :inject "z.unknown")]
   :eject-with-dependent-refused    [(op :evict "a.tools")
                                     (op :eject "e.base")
                                     (op :eject "e.base" {:op/cascade? true})
                                     (op :eject "a.tools")]
   :pin-lazy-on-hook-only           [(op :pin "c.hooks" {:op/policy :lazy})
                                     (op :pin "c.hooks" {:op/policy :lazy :op/force? true})]
   :pin-changes-what-evict-allows   [(op :pin "a.tools" {:op/policy :pinned})
                                     (op :evict "a.tools")
                                     (op :pin "b.pinned" {:op/policy :eager})
                                     (op :evict "b.pinned")]
   :eject-forgets-a-runtime-pin     [(op :pin "a.tools" {:op/policy :pinned})
                                     (op :eject "a.tools")
                                     (op :inject "a.tools")
                                     (op :evict "a.tools")]})

(def mutants
  "[label host-fn]: each a decorator over the port breaking one promise."
  [["drops-dormant-stubs"             drops-dormant-stubs]
   ["core-reload-remounts-everything" core-reload-remounts-everything]
   ["ignores-pins"                    ignores-pins]
   ["evicts-hook-only"                evicts-hook-only]
   ["duplicates-a-tool"               duplicates-a-tool]
   ["eject-leaves-tools"              eject-leaves-tools]
   ["inject-duplicates"               inject-duplicates]
   ["pin-ignores-no-surface"          pin-ignores-no-surface]])

(deftrifecta lifecycle-contract
  hive-mcp.hot.lifecycle.model-test/trace-of
  {:golden-path "test/golden/hot/lifecycle-contract.edn"
   :cases golden-cases
   :gen gen-script
   :pred (fn [trace] (and (d/valid-trace? trace) (empty? (model/violations spec trace))))
   :num-tests 200
   :mutations (mapv (fn [[label host-fn]] [label #(trace-of host-fn %)]) mutants)})

;; ── LSP: the runner over the model host is the pure fold ─────────────────

(deftest runner-over-model-host-is-the-pure-fold
  (doseq [script (gen/sample gen-script 60)]
    (is (= (model/run spec script) (trace-of script)) (pr-str script))))

(deftest every-mutant-breaks-an-invariant-or-the-contract
  (testing "the invariants alone catch the table-shape mutants"
    (let [script [(op :evict "a.tools")]]
      (is (seq (model/violations spec (trace-of drops-dormant-stubs script))))
      (is (seq (model/violations spec (trace-of duplicates-a-tool script)))))
    (let [script [(op :eject "a.tools")]]
      (is (= #{:ejected-addon-tools-never-advertised}
             (set (map :invariant (model/violations spec (trace-of eject-leaves-tools script)))))))
    (let [script [(op :eject "a.tools") (op :inject "a.tools")]]
      (is (= #{:no-duplicate-tools}
             (set (map :invariant (model/violations spec (trace-of inject-duplicates script)))))))))

(deftest every-mutant-is-killed-by-a-golden-case
  (doseq [[label host-fn] mutants]
    (testing label
      (is (seq (for [[k script] golden-cases
                     :when (not= (trace-of script) (trace-of host-fn script))]
                 k))
          (str label " survives every golden case")))))

(deftest every-step-kind-has-an-op-variant-and-a-generator
  (let [kinds (set (remove #{:default} (keys (methods model/step))))]
    (is (= kinds (set (remove #{:default} (keys (methods d/op-variant))))))
    (is (= kinds (set (keys (methods gen-of)))))))

(deftest an-op-missing-its-kind-fields-is-not-an-op
  (is (not (m/validate (d/Op) (op :pin "a.tools"))) "pin needs :op/policy")
  (is (not (m/validate (d/Op) (op :eject "a.tools" {:op/cascade? "yes"}))))
  (is (m/validate (d/Op) (op :eject "a.tools" {:op/cascade? true})))
  (is (not (m/validate (d/Op) (op :evict))) "evict names an addon"))

(deftest an-unknown-op-kind-asks-for-a-registration
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"register a step method"
                        (trace-of [(op :teleport "a.tools")]))))

(deftest the-fixture-spec-is-a-host-spec
  (is (d/valid-spec? spec)))
