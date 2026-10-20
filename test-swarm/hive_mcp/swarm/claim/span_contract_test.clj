(ns hive-mcp.swarm.claim.span-contract-test
  "Trifectas for the `hive-mcp.swarm.claim.span` compat shim, driven by its own
   contracts. The shim delegates to hive-agent, so these run in the swarm
   suite (test-swarm/), where hive-agent is on the classpath. Every subject
   here is pure: the call graph is an injected map, the context is nil."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.swarm.addon-gate :as gate]
            [hive-mcp.swarm.claim.span :as span]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private File [:enum "src/a.clj" "src/b.clj"])
(def ^:private Qn [:enum "a/f" "a/g" "a/h"])

(def ^:private SpanIn
  "Spans over two files and three forms, plus bare paths: every overlap arm
   (file blanket, same form, signature versus caller, disjoint) is reachable."
  [:or File
   [:map [:span/file File] [:span/mode [:enum :file]] [:span/qn :nil]]
   [:map [:span/file File] [:span/mode [:enum :body :signature]] [:span/qn Qn]]])

(def ^:private OneFileSpan
  "SpanIn over one file, so the form-level arms are not starved by disjoint files."
  [:or [:enum "src/a.clj"]
   [:map [:span/file [:enum "src/a.clj"]] [:span/mode [:enum :file]] [:span/qn :nil]]
   [:map [:span/file [:enum "src/a.clj"]] [:span/mode [:enum :body :signature]] [:span/qn Qn]]
   [:map [:span/file [:enum "src/a.clj"]] [:span/mode [:enum :body :signature]] [:span/qn Qn]]])

;; a/f is called by a/g; nobody calls a/g or a/h
(def ^:private graph {"a/f" ["a/g"]})

(defn- norm [s] (if (string? s) {:span/file s :span/mode :file :span/qn nil} s))

(defn- expected-reason
  "Oracle for overlap's :reason, transcribed from the docstrings."
  [cf a b]
  (let [a (norm a) b (norm b)]
    (when (= (:span/file a) (:span/file b))
      (cond
        (or (= :file (:span/mode a)) (= :file (:span/mode b))) :file-claim
        (= (:span/qn a) (:span/qn b)) :same-form
        (and cf (= :signature (:span/mode a)) (some #{(:span/qn b)} (cf (:span/qn a)))) :signature-vs-caller
        (and cf (= :signature (:span/mode b)) (some #{(:span/qn a)} (cf (:span/qn b)))) :signature-vs-caller
        :else nil))))

(hst/deftrifecta-from-schema overlap-names-the-reason
  hive-mcp.swarm.claim.span/overlap
  {:in [:cat [:enum nil graph] [:or SpanIn OneFileSpan] OneFileSpan]
   :out [:maybe span/Overlap]
   :rel (fn [[cf a b] out] (= (:reason out) (expected-reason cf a b)))
   ;; signature-vs-caller is rare under this generator; it is driven
   ;; directly in signature-vs-caller-arm-test
   :classify (fn [_ out] (case (:reason out) nil :none :file-claim :file-claim :form-level))
   :classify-domain #{:file-claim :form-level :none}
   :classify-floor 10
   :num-tests 300
   :golden-path "test/golden/hive-mcp/swarm/claim/overlap.edn"})

(hst/deftrifecta-from-schema conflicts?-is-a-blocking-overlap
  hive-mcp.swarm.claim.span/conflicts?
  {:in [:cat [:enum nil graph] SpanIn OneFileSpan]
   :out :boolean
   :rel (fn [[cf a b] out] (= out (some? (expected-reason cf a b))))
   :num-tests 200})

(hst/deftrifecta-from-schema key-of-is-path-or-path-hash-qn
  hive-mcp.swarm.claim.span/key-of
  {:in [:cat SpanIn]
   :out :string
   :rel (fn [[s] out]
          (let [s (norm s)]
            (= out (if (= :file (:span/mode s))
                     (:span/file s)
                     (str (:span/file s) "#" (:span/qn s))))))
   :golden-path "test/golden/hive-mcp/swarm/claim/key-of.edn"})

(hst/deftrifecta-from-schema widen-keeps-the-wider-mode
  hive-mcp.swarm.claim.span/widen
  {:in [:cat [:enum :file :signature :body :bogus nil] [:enum :file :signature :body :bogus nil]]
   :out [:maybe :keyword]
   :rel (fn [[a b] out]
          (let [r #(get {:body 0 :signature 1 :file 2} % -1)]
            (= out (if (>= (r a) (r b)) a b))))
   :golden-path "test/golden/hive-mcp/swarm/claim/widen.edn"
   :n-cases 12})

(hst/deftrifecta-from-schema form-span-normalizes-mode-and-qn
  hive-mcp.swarm.claim.span/form-span
  {:in [:cat File [:enum "a/f" "  " nil] [:enum :body :signature :file :bogus]]
   :out span/SpanShape
   :rel (fn [[f qn mode] out]
          (and (= f (:span/file out))
               (= (when-not (or (nil? qn) (= "  " qn)) qn) (:span/qn out))
               (= (if (#{:body :signature :file} mode) mode :body) (:span/mode out))))
   :golden-path "test/golden/hive-mcp/swarm/claim/form-span.edn"
   :n-cases 12})

(hst/deftrifecta-from-schema file-span-is-a-whole-file-claim
  hive-mcp.swarm.claim.span/file-span
  {:in [:cat [:or File :symbol]]
   :out span/SpanShape
   :rel (fn [[f] out] (= out {:span/file (str f) :span/mode :file :span/qn nil}))})

(hst/deftrifecta-from-schema span-coerces-loose-input
  hive-mcp.swarm.claim.span/span
  {:in [:cat [:or SpanIn :int :keyword
              [:map [:span/file File] [:span/mode [:enum :body]] [:span/qn [:enum nil "  "]]]]]
   :out span/SpanShape
   :rel (fn [[x] out]
          (cond
            (and (map? x) (some? (:span/qn x)) (not= "  " (:span/qn x))) (= (:span/qn x) (:span/qn out))
            (map? x) (= :file (:span/mode out))
            :else (= {:span/file (if (string? x) x (str x)) :span/mode :file :span/qn nil} out)))
   :golden-path "test/golden/hive-mcp/swarm/claim/span.edn"
   :n-cases 10})

(hst/deftrifecta-from-schema explain-renders-one-line
  hive-mcp.swarm.claim.span/explain
  {:in [:map
        [:file File]
        [:qn [:maybe Qn]]
        [:held-by [:enum "ling-a" "ling-b"]]
        [:reason [:enum :file-claim :same-form :signature-vs-caller :cross-worktree :other]]
        [:caller {:optional true} Qn]]
   :out [:string {:min 1}]
   :rel (fn [c out] (str/includes? out (:held-by c)))
   :golden-path "test/golden/hive-mcp/swarm/claim/explain.edn"
   :n-cases 10})

(def ^:private HeldIn
  [:vector {:max 4}
   [:map [:span/file File] [:span/mode [:enum :file :body :signature]] [:span/qn Qn]
    [:claim/slave [:enum "ling-a" "ling-b"]]]])

(hst/deftrifecta-from-schema conflicts-skip-the-requesters-own
  hive-mcp.swarm.claim.span/conflicts
  {:in [:cat [:enum nil graph] HeldIn SpanIn [:enum "ling-a" "ling-b"]]
   :out [:vector span/Conflict]
   :rel (fn [[cf held wanted me] out]
          (and (every? #(not= me (:held-by %)) out)
               (= (count out)
                  (count (filter #(and (not= me (:claim/slave %))
                                       (expected-reason cf (span/span %) wanted))
                                 held)))))
   :num-tests 200})

(hst/deftrifecta-from-schema assess-without-ctx-has-no-warnings
  hive-mcp.swarm.claim.span/assess
  {:in [:cat [:enum nil graph] HeldIn SpanIn [:enum "ling-a" "ling-b"] :nil]
   :out span/Assessment
   :rel (fn [[cf held wanted me _] out]
          (and (= [] (:warnings out))
               (= (:conflicts out) (span/conflicts cf held wanted me))))
   :num-tests 100})

(deftest signature-vs-caller-arm-test
  (let [sig    (span/form-span "src/a.clj" "a/f" :signature)
        caller (span/form-span "src/a.clj" "a/g")
        why    (span/overlap graph sig caller)]
    (is (= {:reason :signature-vs-caller :qn "a/f" :caller "a/g"} why))
    (is (m/validate span/Overlap why))
    (testing "no graph, no rule"
      (is (nil? (span/overlap nil sig caller))))
    (testing "symmetric"
      (is (= why (span/overlap graph caller sig))))))

(deftest context-for-anchors-at-cwd-test
  (let [ctx (span/context-for "/tmp")]
    (is (m/validate [:maybe span/Ctx] ctx))
    (when ctx (is (= "/tmp" (:cwd ctx))))))

(deftest addon-values-resolve-test
  (if (gate/present?)
    (testing "the load-time re-exports are the addon's values"
      (is (= [:file :signature :body] span/modes))
      (is (= {:body 0 :signature 1 :file 2} span/mode-rank))
      (is (some? span/Span)))
    (testing "without the addon a load-time value is nil and a call raises :swarm/addon-missing"
      (is (nil? span/modes))
      (is (nil? span/mode-rank))
      (is (gate/addon-missing? #(span/key-of "src/a.clj")))
      (is (gate/addon-missing? #(span/overlap nil "src/a.clj" "src/a.clj")))
      (is (gate/addon-missing? #(span/conflicts nil [] "src/a.clj" "ling-a"))))))

(def ^:private offline?
  "The tests that never reach hive-agent."
  (gate/offline #{"context-for-anchors-at-cwd-test" "addon-values-resolve-test"
                  "span-schemas-generate-test"}
                []))

(defn test-ns-hook [] (gate/run 'hive-mcp.swarm.claim.span-contract-test offline?))

(deftest span-schemas-generate-test
  (doseq [s [span/Mode span/SpanShape span/SpanLike span/Held span/CallersFn span/Ctx
             span/Reason span/Overlap span/Conflict span/Assessment]]
    (is (seq (mg/sample s {:size 4})) (pr-str (m/form s)))))
