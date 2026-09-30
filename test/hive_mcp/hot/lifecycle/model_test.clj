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
            [hive-test.trifecta :refer [deftrifecta]]))

;; ── fixture: a host with every kind of addon the contract distinguishes ──

(def spec
  {:spec/core-tools  #{"code" "hot" "memory"}
   :spec/addon-tools {"a.tools" #{"alpha"}
                      "b.pinned" #{"beta"}
                      "c.hooks" #{}}
   :spec/hook-only   #{"c.hooks"}
   :spec/policy      {"a.tools" :eager "b.pinned" :pinned "c.hooks" :eager}})

(defn- op
  ([kind] {:op/kind kind})
  ([kind addon] {:op/kind kind :op/addon addon}))

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

;; ── the trifecta ──────────────────────────────────────────────────────────

(def gen-op
  (gen/one-of
   [(gen/return (op :core-reload))
    (gen/fmap (fn [[k a]] (op k a))
              (gen/tuple (gen/elements [:addon-reload :evict :activate :call])
                         (gen/elements (sort (keys (:spec/addon-tools spec))))))]))

(def gen-script (gen/vector gen-op 0 12))

(deftrifecta lifecycle-contract
  hive-mcp.hot.lifecycle.model-test/trace-of
  {:golden-path "test/golden/hot/lifecycle-contract.edn"
   :cases {:core-reload-with-addons-mounted [(op :core-reload) (op :core-reload)]
           :evict-then-call-remounts        [(op :evict "a.tools") (op :call "a.tools")]
           :evict-then-activate-roundtrip   [(op :evict "a.tools") (op :activate "a.tools") (op :activate "a.tools")]
           :pinned-refuses-eviction         [(op :evict "b.pinned")]
           :hook-only-refuses-eviction      [(op :evict "c.hooks")]
           :reload-needs-a-mounted-addon    [(op :evict "a.tools") (op :addon-reload "a.tools")]
           :core-reload-while-dormant       [(op :evict "a.tools") (op :core-reload) (op :activate "a.tools")]}
   :gen gen-script
   :pred (fn [trace] (and (d/valid-trace? trace) (empty? (model/violations spec trace))))
   :num-tests 200
   :mutations [["drops-dormant-stubs"             #(trace-of drops-dormant-stubs %)]
               ["core-reload-remounts-everything" #(trace-of core-reload-remounts-everything %)]
               ["ignores-pins"                    #(trace-of ignores-pins %)]
               ["evicts-hook-only"                #(trace-of evicts-hook-only %)]
               ["duplicates-a-tool"               #(trace-of duplicates-a-tool %)]]})

;; ── LSP: the runner over the model host is the pure fold ─────────────────

(deftest runner-over-model-host-is-the-pure-fold
  (doseq [script (gen/sample gen-script 60)]
    (is (= (model/run spec script) (trace-of script)) (pr-str script))))

(deftest every-mutant-breaks-an-invariant-or-the-contract
  (testing "the invariants alone catch the table-shape mutants"
    (let [script [(op :evict "a.tools")]]
      (is (seq (model/violations spec (trace-of drops-dormant-stubs script))))
      (is (seq (model/violations spec (trace-of duplicates-a-tool script)))))))

(deftest an-unknown-op-kind-asks-for-a-registration
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"register a step method"
                        (trace-of [(op :teleport "a.tools")]))))

(deftest the-fixture-spec-is-a-host-spec
  (is (d/valid-spec? spec)))
