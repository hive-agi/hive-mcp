(ns hive-mcp.dsl.contract-trifecta-test
  "Schema-driven trifectas (property + golden + mutation) for the pure batch-DSL
   and config-merge functions, plus the contract-coverage gate over the group:
   dsl.param-domain, dsl.response, dsl.verbs, context.budget,
   context.reconstruction, config.merge, multi.param-coerce.

   The property and mutation facets are synthesized from the malli schemas by
   hive-schemas.test; the :rel oracles are independent restatements of each
   function's documented law, not a copy of its body."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.config.merge :as merge]
            [hive-mcp.dsl.response :as response]
            [hive-mcp.dsl.verbs :as verbs]
            [hive-schemas.coverage :as cov]
            [hive-schemas.instrument :as inst]
            [hive-schemas.test :as hst]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; dsl.response/format-results-compact
;; =============================================================================

(def ^:private Summary
  "The summary shape batch execution really emits."
  [:map
   [:total :int]
   [:success :int]
   [:failed :int]
   [:waves :int]])

(def ^:private Envelope
  "BatchEnvelope narrowed to the summary the executor emits."
  [:map
   [:success :boolean]
   [:summary Summary]
   [:waves [:map-of :string [:vector response/BatchOpResult]]]
   [:dry_run :boolean]
   [:plan :map]
   [:errors [:vector :string]]])

(defn- compact-law
  "The documented law of the compact envelope."
  [in out]
  (let [{:keys [success summary waves dry_run errors]} in
        {:keys [total success failed] :as _s} summary
        ok-count success]
    (and (= (:success in) (:ok out))
         (= {:t total :ok ok-count :f failed :w (:waves summary)} (:s out))
         (= (boolean (and waves (not dry_run))) (contains? out :r))
         (or (not (contains? out :r))
             (= (reduce + (map count (vals waves))) (count (:r out))))
         (= (boolean (and dry_run (:plan in))) (contains? out :dr))
         (= (boolean errors) (contains? out :E)))))

(hst/deftrifecta-from-schema compact-envelope-law
  hive-mcp.dsl.response/format-results-compact
  {:in Envelope
   :out response/CompactEnvelope
   :rel compact-law
   :golden-path "test/golden/dsl/contract/format-results-compact.edn"
   :num-tests 80
   :seed 0
   :n-cases 4})

;; =============================================================================
;; dsl.response/resolve-compress-mode
;; =============================================================================

(def ^:private CompressArgs
  [:map
   [:compact {:optional true}
    [:or :boolean
     [:enum "compact" "minimal" "full" "turbo" :compact :minimal :full]]]
   [:_caller_id {:optional true} [:enum "swarm-1" "ling-2" "coordinator"]]])

(defn- expected-mode
  [{:keys [compact _caller_id] :as args}]
  (cond
    (false? compact) nil
    (true? compact) :compact
    (contains? args :compact)
    (let [kw (keyword (name compact))]
      (#{:compact :minimal :full} kw))
    (and _caller_id
         (or (str/starts-with? _caller_id "swarm-")
             (str/starts-with? _caller_id "ling-"))) :compact
    :else nil))

(hst/deftrifecta-from-schema resolve-compress-mode-priority
  hive-mcp.dsl.response/resolve-compress-mode
  {:in CompressArgs
   :out [:maybe response/CompressMode]
   :rel (fn [in out] (= (expected-mode in) out))
   :golden-path "test/golden/dsl/contract/resolve-compress-mode.edn"
   :num-tests 120
   :seed 0
   :n-cases 6})

;; =============================================================================
;; dsl.verbs/ref-target and expand-param-key
;; =============================================================================

(def ^:private RefText
  [:enum "$ref:$0.data.id" "$ref:op-1" "$ref:$2" "plain" "" "ref:no-dollar"
   "$refx:bad"])

(hst/deftrifecta-from-schema ref-target-extracts-the-op-id
  hive-mcp.dsl.verbs/ref-target
  {:in RefText
   :out [:maybe :string]
   :rel (fn [in out]
          (if (str/starts-with? in "$ref:")
            (and (string? out)
                 (str/starts-with? (subs in 5) out)
                 (not (str/includes? out ".")))
            (nil? out)))
   :golden-path "test/golden/dsl/contract/ref-target.edn"
   :num-tests 60
   :seed 0
   :n-cases 7})

(def ^:private ParamKey
  [:enum "c" "t" "#" "id" "rel" "idk" "foo" "relation" :c :# :relation :from])

(hst/deftrifecta-from-schema expand-param-key-resolves-aliases
  hive-mcp.dsl.verbs/expand-param-key
  {:in ParamKey
   :out :keyword
   :rel (fn [in out]
          (let [n (name in)]
            (= out (get verbs/param-aliases n (if (keyword? in) in (keyword n))))))
   :golden-path "test/golden/dsl/contract/expand-param-key.edn"
   :num-tests 60
   :seed 0
   :n-cases 8})

;; =============================================================================
;; config.merge
;; =============================================================================

(def ^:private Layer
  [:map-of [:enum :a :b :c]
   [:or :int [:map-of [:enum :x :y] :int]]])

(defn- merge-law
  [[defaults user] out]
  (and (= (set (concat (keys defaults) (keys user))) (set (keys out)))
       (every? (fn [k]
                 (let [dv (get defaults k) uv (get user k)]
                   (cond
                     (not (contains? user k)) (= dv (get out k))
                     (and (map? dv) (map? uv)) (= (merge dv uv) (get out k))
                     :else (= uv (get out k)))))
               (keys out))))

(hst/deftrifecta-from-schema deep-merge-lets-the-user-win
  hive-mcp.config.merge/deep-merge
  {:in [:cat Layer Layer]
   :out merge/ConfigMap
   :rel merge-law
   :golden-path "test/golden/dsl/contract/deep-merge.edn"
   :num-tests 100
   :seed 0
   :n-cases 5})

(hst/deftrifecta-from-schema parse-key-path-round-trips
  hive-mcp.config.merge/parse-key-path
  {:in [:enum "" "  " "kg" "kg.backend" "a.b.c" "services.nrepl.port"]
   :out [:maybe merge/KeyPath]
   :rel (fn [in out]
          (if (str/blank? in)
            (nil? out)
            (= in (str/join "." (map name out)))))
   :golden-path "test/golden/dsl/contract/parse-key-path.edn"
   :num-tests 60
   :seed 0
   :n-cases 6})

;; =============================================================================
;; Contract-coverage gate over the group
;; =============================================================================

(def ^:private gated-paths
  ["src/hive_mcp/dsl/param_domain.clj"
   "src/hive_mcp/dsl/response.clj"
   "src/hive_mcp/dsl/verbs.clj"
   "src/hive_mcp/context/budget.clj"
   "src/hive_mcp/context/reconstruction.clj"
   "src/hive_mcp/config/merge.clj"
   "src/hive_mcp/multi/param_coerce.clj"])

(def ^:private extension-delegate
  (str "Thin delegate to an optional extension through delegate-or-noop: the "
       "extension owns the value domain, so the argument and result are :any "
       "and a contract narrower than :any would be a claim this ns cannot keep."))

(def ^:private exempt
  {'hive-mcp.dsl.response/omit-defaults          extension-delegate
    'hive-mcp.dsl.response/strip-verbose          extension-delegate
   'hive-mcp.dsl.response/abbreviate-keys        extension-delegate
   'hive-mcp.dsl.response/minimal-entry          extension-delegate
   'hive-mcp.dsl.response/compact-response       extension-delegate
   'hive-mcp.dsl.response/minimal-response       extension-delegate
   'hive-mcp.dsl.response/compress               extension-delegate
   'hive-mcp.dsl.response/compress-mcp-text      extension-delegate
   'hive-mcp.dsl.response/compress-content       extension-delegate
   'hive-mcp.dsl.response/compress-batch-op      extension-delegate
   'hive-mcp.dsl.response/compress-batch-envelope extension-delegate
   'hive-mcp.context.reconstruction/gather-kg-context extension-delegate
   'hive-mcp.context.reconstruction/compress-kg-subgraph extension-delegate})

(cov/deftest-contract-coverage every-pure-dsl-fn-carries-a-contract
  {:paths gated-paths
   :exempt exempt})

(deftest the-gate-is-not-vacuous
  (let [report (cov/coverage {:paths gated-paths :exempt exempt})]
    (is (<= 30 (count (:universe report)))
        "the universe collapsed - a gate over nothing passes for the wrong reason")
    (is (<= 25 (count (:covered report))))))

(deftest every-contract-survives-instrumentation
  (let [report  (cov/coverage {:paths gated-paths :exempt exempt})
        nss     (into [] (comp (map namespace) (map symbol) (distinct))
                      (:covered report))
        wrapped (inst/instrument! {:ns nss})]
    (try
      (is (= (count wrapped) (count (:covered report)))
          "every covered function should have been wrappable")
      (is (= [{:tool "memory" :command "add" :content "hi"}]
             (verbs/parse-dsl [["m+" {"c" "hi"}]]))
          "a real call passes through the instrumented contracts")
      (finally (inst/unstrument! {:ns nss})))))
