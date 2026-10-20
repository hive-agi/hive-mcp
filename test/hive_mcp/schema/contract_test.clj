(ns hive-mcp.schema.contract-test
  "Contract facets for src/hive_mcp/schema: the type-token gate, the memory
   validators, the tool-param helpers, the elisp emitters and the EDN schema
   loader. Each trifecta targets the contracted fn; its oracle is written from
   the docstring. The EDN loader reads test/resources/schema/covtest.edn and
   every test that touches its caches clears them first (reload!)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.schema.edn :as sedn]
            [hive-mcp.schema.elisp :as el]
            [hive-mcp.schema.memory :as mem]
            [hive-mcp.schema.tools :as tools]
            [hive-mcp.schema.type-token :as tt]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.generator :as mg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [t] (sedn/reload!) (try (t) (finally (sedn/reload!)))))

;; =============================================================================
;; Gate
;; =============================================================================

(coverage/deftest-contract-coverage schema-contracts
  {:paths ["src/hive_mcp/schema"]
   :exempt {}})

(deftest the-gate-is-not-vacuous
  (is (= 26 (count (:universe (coverage/coverage {:paths ["src/hive_mcp/schema"]}))))))

(deftest every-schema-contract-generates-both-sides
  (doseq [ns-sym '[hive-mcp.schema.edn hive-mcp.schema.elisp hive-mcp.schema.memory
                   hive-mcp.schema.tools hive-mcp.schema.type-token]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

;; =============================================================================
;; type-token
;; =============================================================================

(def ^:private RawType
  "Strings and keywords of every flavour (padded, upper case, unsafe chars,
   blank), plus values that are neither."
  [:or
   [:enum "Decision" "  note  " "my-type_2" "9lives" "a b" "x\"y" "" "   " "ok/no"
    :Snippet :plan "convention" (apply str "a" (repeat 70 "b"))]
   [:enum "plan" "Axiom" " decision" :note "my_type-3"]
   :keyword :int :nil [:vector :string]])

(deftrifecta-from-schema sanitize-type-trims-and-lowercases-strings-and-keywords
  #'tt/sanitize-type
  {:in [:cat RawType]
   :out [:maybe tt/Token]
   :rel (fn [[t] out]
          (let [s (cond (keyword? t) (name t) (string? t) t)]
            (if (and s (not (str/blank? s)))
              (= out (str/lower-case (str/trim s)))
              (nil? out))))
   :classify (fn [_ out] (if out :token :nil))
   :classify-domain #{:token :nil}
   :golden-path "test/golden/hive-mcp/schema/sanitize-type.edn"
   :num-tests 200})

(deftrifecta-from-schema safe-type-accepts-only-the-bounded-charset
  #'tt/safe-type?
  {:in [:cat RawType]
   :out :boolean
   :rel (fn [[t] out]
          (let [s (tt/sanitize-type t)]
            (= out (boolean (and s (<= (count s) tt/max-type-length)
                                 (re-matches #"[a-z][a-z0-9_-]*" s))))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :golden-path "test/golden/hive-mcp/schema/safe-type.edn"
   :num-tests 200})

(deftrifecta-from-schema requested-type-tag-marks-only-safe-types
  #'tt/requested-type-tag
  {:in [:cat RawType]
   :out [:maybe [:string {:min 16}]]
   :rel (fn [[t] out]
          (if (tt/safe-type? t)
            (= out (str tt/requested-type-tag-prefix (tt/sanitize-type t)))
            (nil? out)))
   :classify (fn [_ out] (if out :tagged :refused))
   :classify-domain #{:tagged :refused}
   :golden-path "test/golden/hive-mcp/schema/requested-type-tag.edn"
   :num-tests 200})

(deftrifecta-from-schema requested-type-of-reads-the-first-marker
  #'tt/requested-type-of
  {:in [:cat [:maybe [:vector {:max 4}
                      [:enum "kanban" "requested-type:axiom" "requested-type:plan"
                       :scope "requested-type:" "x"]]]]
   :out [:maybe :string]
   :rel (fn [[tags] out]
          (= out (some #(let [s (str %)]
                          (when (str/starts-with? s "requested-type:") (subs s 15)))
                       tags)))
   :classify (fn [_ out] (if out :marked :unmarked))
   :classify-domain #{:marked :unmarked}
   :golden-path "test/golden/hive-mcp/schema/requested-type-of.edn"
   :num-tests 200})

;; =============================================================================
;; memory validators
;; =============================================================================

(def ^:private DurationIn [:or mem/MemoryDuration [:enum "forever" "" "LONG"] :nil :keyword])

(deftrifecta-from-schema valid-duration-is-the-enum
  #'mem/valid-duration?
  {:in [:cat DurationIn]
   :out :boolean
   :rel (fn [[d] out] (= out (contains? #{"ephemeral" "short" "medium" "long" "permanent"} d)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :golden-path "test/golden/hive-mcp/schema/valid-duration.edn"
   :num-tests 100})

(deftrifecta-from-schema valid-type-is-the-safe-token-gate
  #'mem/valid-type?
  {:in [:cat RawType]
   :out :boolean
   :rel (fn [[t] out] (= out (tt/safe-type? t)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :golden-path "test/golden/hive-mcp/schema/valid-type.edn"
   :num-tests 100})

(def ^:private EntryIn
  [:or mem/MemoryEntry
   [:map [:id [:enum "" "e1"]] [:type [:enum "BAD TYPE" "note"]] [:content [:maybe :string]]]
   :nil])

(deftrifecta-from-schema valid-entry-agrees-with-explain-entry
  #'mem/valid-entry?
  {:in [:cat EntryIn]
   :out :boolean
   :rel (fn [[e] out] (= out (nil? (mem/explain-entry e))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :golden-path "test/golden/hive-mcp/schema/valid-entry.edn"
   :num-tests 100})

(deftrifecta-from-schema explain-entry-names-the-entry
  #'mem/explain-entry
  {:in [:cat EntryIn]
   :out [:maybe mem/Explanation]
   :rel (fn [[e] out] (if out (and (= e (:value out)) (seq (:errors out))) (m/validate mem/MemoryEntry e)))
   :classify (fn [_ out] (if out :explained :valid))
   :classify-domain #{:explained :valid}
   :mutation false ; the explanation embeds malli schema objects, so the golden
                   ; and the mutation snapshot cannot be printed back; the
                   ; :rel above pins :value and :errors instead.
   :num-tests 100})

(def ^:private AddIn
  [:or mem/MemoryEntryMinimal
   [:map [:type [:enum "Bad" "note"]] [:content [:enum "" "c"]]]
   [:map [:content :string]]])

(deftrifecta-from-schema validate-add-request-reports-each-failing-path
  #'mem/validate-add-request
  {:in [:cat AddIn]
   :out [:maybe mem/AddRequestErrors]
   :rel (fn [[e] out]
          (if (m/validate mem/MemoryEntryMinimal e)
            (nil? out)
            (= (set (map :path (:errors out)))
               (set (map :path (:errors (m/explain mem/MemoryEntryMinimal e)))))))
   :classify (fn [_ out] (if out :rejected :accepted))
   :classify-domain #{:rejected :accepted}
   :golden-path "test/golden/hive-mcp/schema/validate-add-request.edn"
   :num-tests 100})

;; =============================================================================
;; tool params
;; =============================================================================

(def ^:private ParamSchema [:enum [:map [:a :int]] [:map [:name [:string {:min 1}]]] :int [:vector :keyword]])
(def ^:private ParamValue [:or [:map [:a :int]] [:map [:a :string]] [:map [:name :string]] :int [:vector :keyword] :nil])

(deftrifecta-from-schema validate-params-answers-valid-or-explains
  #'tools/validate-params
  {:in [:cat ParamSchema ParamValue]
   :out tools/ValidationResult
   :rel (fn [[s p] out] (= (:valid out) (m/validate s p)))
   :classify (fn [_ out] (:valid out))
   :classify-domain #{true false}
   ;; about one generated pair in six validates; 10 of 200 is a firm floor
   :classify-floor 10
   :mutation false ; the :errors arm holds malli schema objects (unprintable in
                   ; a golden); the arm split is pinned by the test below.
   :num-tests 200})

(deftrifecta-from-schema coerce-and-validate-is-validate-params
  #'tools/coerce-and-validate
  {:in [:cat ParamSchema ParamValue]
   :out tools/ValidationResult
   :rel (fn [[s p] out] (= (:valid out) (:valid (tools/validate-params s p))))
   :classify (fn [_ out] (:valid out))
   :classify-domain #{true false}
   :classify-floor 10
   :mutation false ; same unprintable :errors arm as validate-params
   :num-tests 200})

(deftest validation-result-arms-are-disjoint
  (let [ok (tools/validate-params [:map [:a :int]] {:a 1})
        bad (tools/validate-params [:map [:a :int]] {:a "x"})
        [ok-arm bad-arm] (m/children (m/schema tools/ValidationResult))]
    (is (= {:valid true} ok))
    (is (m/validate ok-arm ok))
    (is (not (m/validate bad-arm ok)))
    (is (m/validate bad-arm bad))
    (is (not (m/validate ok-arm bad)))
    (is (= [[:a]] (mapv :path (:errors (:errors bad)))))))

(deftrifecta-from-schema json-schema-string-keys-properties-and-required
  #'tools/json-schema
  {:in [:cat [:enum [:map [:a :int] [:b {:optional true} :string]]
              [:map [:name [:string {:min 1}]]]
              [:map [:inner [:map [:k :boolean]]]]
              :int [:vector :string]]]
   :out [:map-of :keyword :any]
   :rel (fn [_ out] (and (every? string? (keys (:properties out {})))
                         (every? string? (:required out []))))
   :classify (fn [_ out] (if (:properties out) :object :scalar))
   :classify-domain #{:object :scalar}
   :golden-path "test/golden/hive-mcp/schema/json-schema.edn"
   :num-tests 50})

(deftest-golden param-coercer-decodes-and-names-the-field
  "test/golden/hive-mcp/schema/param-coercer.edn"
  (let [c (tools/param-coercer "p" [:map [:a :int] [:b {:optional true} :string]])
        thrown (fn [x] (try (c x) :no-throw (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    {:decoded (c {"a" "12" "b" "x"})
     :bad-field (thrown {"a" "nope"})
     :two-bad (thrown {"a" "nope" "b" 3})
     :decoded-spawn ((tools/param-coercer "s" tools/SpawnLoopParams)
                     {"llm_retries" "2" "chat_run_id" "  "})}))

(deftest kg-relation-type-is-the-sorted-enum-or-open
  (let [s (tools/KGRelationType)]
    (is (m/validate [:or [:cat [:= :enum] [:+ :string]] [:tuple [:= :string] [:map [:min [:= 1]]]]] s))
    (if (= :enum (first s))
      (is (= (rest s) (sort (rest s))))
      (is (= [:string {:min 1}] s)))))

;; =============================================================================
;; elisp emitters
;; =============================================================================

(def ^:private Schemas
  [:enum
   [:map [:id :string] [:type [:enum :note :snippet]] [:tags [:vector :string]]]
   [:map [:enabled :boolean] [:port {:optional true} [:maybe :int]]]
   [:map [:x [:or :string :int]] [:ratio :double] [:kind :keyword] [:any :any]]])

(def ^:private Opts
  [:map [:name [:enum "hive-a" "hive-b"]]
   [:constructor {:optional true} [:enum "make-it"]]
   [:include {:optional true} [:enum "base"]]
   [:defaults {:optional true} [:map-of [:enum :enabled :port :id] [:or :boolean :int :string :nil]]]
   [:var {:optional true} [:enum "e" "row"]]])

(defn- field-names [s] (map (comp name first) (m/children (m/schema s))))

(defn- checked-field-names
  "Fields whose type the validator checks: an :any field emits a bare t."
  [s]
  (keep (fn [[k _ fs]] (when-not (= :any (m/type fs)) (name k))) (m/children (m/schema s))))

(deftrifecta-from-schema emit-struct-names-every-field
  #'el/emit-struct
  {:in [:cat Schemas Opts]
   :out [:string {:min 1}]
   :rel (fn [[s o] out]
          (and (str/starts-with? out (str "(cl-defstruct (" (:name o) " (:constructor "
                                          (or (:constructor o) (str (:name o) "-create")) ")"))
               (str/ends-with? out (str "\n  " (str/join " " (field-names s)) ")"))
               (= (boolean (:include o)) (str/includes? out "(:include "))))
   :golden-path "test/golden/hive-mcp/schema/emit-struct.edn"
   :num-tests 100})

(deftrifecta-from-schema emit-struct-with-defaults-renders-default-slots
  #'el/emit-struct-with-defaults
  {:in [:cat Schemas Opts]
   :out [:string {:min 1}]
   :rel (fn [[s o] out]
          (every? (fn [f] (if (contains? (:defaults o) (keyword f))
                            (str/includes? out (str "(" f " "))
                            (str/includes? out f)))
                  (field-names s)))
   :golden-path "test/golden/hive-mcp/schema/emit-struct-with-defaults.edn"
   :num-tests 100})

(deftrifecta-from-schema emit-from-plist-reads-every-key
  #'el/emit-from-plist
  {:in [:cat Schemas Opts]
   :out [:string {:min 1}]
   :rel (fn [[s o] out]
          (and (str/starts-with? out (str "(defun " (:name o) "-from-plist (plist)"))
               (every? #(str/includes? out (str "(plist-get plist :" % ")")) (field-names s))))
   :golden-path "test/golden/hive-mcp/schema/emit-from-plist.edn"
   :num-tests 100})

(deftrifecta-from-schema emit-validator-checks-every-field
  #'el/emit-validator
  {:in [:cat Schemas Opts]
   :out [:string {:min 1}]
   :rel (fn [[s o] out]
          (let [v (or (:var o) "entry")]
            (and (str/starts-with? out (str "(defun " (:name o) "-valid-p (" (str/upper-case v) ")"))
                 (every? #(str/includes? out (str "(" (:name o) "-" % " " v ")")) (checked-field-names s)))))
   :golden-path "test/golden/hive-mcp/schema/emit-validator.edn"
   :num-tests 100})

(def ^:private Pairs [:vector {:max 3} [:tuple Schemas Opts]])

(deftrifecta-from-schema emit-structs-joins-each-struct
  #'el/emit-structs
  {:in [:cat Pairs]
   :out el/ElispSource
   :rel (fn [[ps] out] (= out (str/join "\n\n" (map #(apply el/emit-struct %) ps))))
   :golden-path "test/golden/hive-mcp/schema/emit-structs.edn"
   :num-tests 50})

(deftrifecta-from-schema emit-from-plist-all-joins-each-fn
  #'el/emit-from-plist-all
  {:in [:cat Pairs]
   :out el/ElispSource
   :rel (fn [[ps] out] (= out (str/join "\n\n" (map #(apply el/emit-from-plist %) ps))))
   :golden-path "test/golden/hive-mcp/schema/emit-from-plist-all.edn"
   :num-tests 50})

(deftrifecta-from-schema emit-validators-joins-each-predicate
  #'el/emit-validators
  {:in [:cat Pairs]
   :out el/ElispSource
   :rel (fn [[ps] out] (= out (str/join "\n\n" (map #(apply el/emit-validator %) ps))))
   :golden-path "test/golden/hive-mcp/schema/emit-validators.edn"
   :num-tests 50})

(deftest emitters-refuse-a-missing-name-or-a-non-map
  (doseq [f [el/emit-struct el/emit-struct-with-defaults el/emit-from-plist el/emit-validator]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"requires :name" (f [:map [:a :int]] {})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"requires a :map schema" (f [:vector :int] {:name "x"})))))

;; =============================================================================
;; EDN schema loader
;; =============================================================================

(deftest-golden edn-loader-reads-validates-and-caches
  "test/golden/hive-mcp/schema/edn-loader.edn"
  (do (sedn/reload!)
      {:file (sedn/load-schema-file "covtest")
       :missing-file (sedn/load-schema-file "no-such-schema-file")
       :point-form (m/form (sedn/schema-for :covtest/point))
       :unknown-key (sedn/schema-for :covtest/nope)
       :valid (mapv #(sedn/valid? :covtest/point %) [{:x 1 :y 2} {:x "1"} nil])
       :valid-unknown (sedn/valid? :covtest/nope {})
       :validate-ok (sedn/validate :covtest/name "n")
       :validate-bad (try (sedn/validate :covtest/name "") (catch clojure.lang.ExceptionInfo e
                                                             (select-keys (ex-data e) [:type :schema])))
       :validate-missing (try (sedn/validate :covtest/nope 1) (catch clojure.lang.ExceptionInfo e
                                                               [(ex-message e) (ex-data e)]))
       :reload (sedn/reload!)}))

(deftest schema-for-caches-the-compiled-schema
  (sedn/reload!)
  (let [a (sedn/schema-for :covtest/mode)]
    (is (identical? a (sedn/schema-for :covtest/mode)))
    (sedn/reload!)
    (is (not (identical? a (sedn/schema-for :covtest/mode))))
    (is (m/validate (m/schema [:enum "a" "b"]) "a"))
    (testing "an unqualified key names no file"
      (is (nil? (sedn/schema-for :plain))))))
