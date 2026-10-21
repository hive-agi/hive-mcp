(ns hive-mcp.schema.edn-test
  "Facets for the EDN schema loader, over the classpath fixture
   test/resources/schema/covtest.edn (shared with
   hive-mcp.schema.contract-test). Every test clears the caches first."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.schema.edn :as sedn]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [t] (sedn/reload!) (try (t) (finally (sedn/reload!)))))

(def ^:private fixture
  "covtest.edn as data: what load-schema-file must answer for \"covtest\"."
  {:covtest/point [:map [:x :int] [:y :int]]
   :covtest/name  [:string {:min 1}]
   :covtest/mode  [:enum "a" "b"]})

(def ^:private Key
  "schema-for's SchemaKey narrowed to keys that reach every branch: present in
   the fixture, absent from it, a file that does not exist, and unqualified."
  [:enum :covtest/point :covtest/name :covtest/mode :covtest/nope :no-file/x :plain])

(deftrifecta-from-schema load-schema-file-reads-the-classpath-file
  hive-mcp.schema.edn/load-schema-file
  ;; "covtest" is its own :or branch so about half the runs reach the found
  ;; arm; under a flat [:maybe [:enum ...]] it was ~1 in 9 and starved the
  ;; classification floor on unlucky seeds
  {:in [:or [:enum "covtest"] [:maybe [:enum "no-such-schema-file" ""]]]
   :out [:maybe sedn/SchemaFile]
   :rel (fn [ns-name out] (= out (when (= "covtest" ns-name) fixture)))
   :classify (fn [_ out] (some? out))
   :classify-domain #{true false}
   :num-tests 100
   :golden-path "test/golden/hive-mcp/schema/edn/load-schema-file.edn"})

(deftrifecta-from-schema schema-for-compiles-the-fixture-form
  hive-mcp.schema.edn/schema-for
  {:in Key
   :out [:maybe sedn/SchemaObject]
   :rel (fn [k out]
          (if-let [raw (get fixture k)]
            (= raw (m/form out))
            (nil? out)))
   :classify (fn [_ out] (some? out))
   :classify-domain #{true false}
   ;; the answer is a compiled malli schema object: an EDN golden cannot
   ;; print it back, nor can the mutation facet's snapshot; the golden below
   ;; pins (m/form ...) of the same answers
   :mutation false
   :num-tests 100})

(def ^:private KeyAndValue
  [:cat Key [:or [:map [:x :int] [:y :int]] [:enum "a" "b" "" "n" 3 nil {:x "1"}]]])

(deftrifecta-from-schema valid-is-validate-against-the-fixture
  hive-mcp.schema.edn/valid?
  {:in KeyAndValue
   :out [:maybe :boolean]
   :rel (fn [[k v] out]
          (if-let [raw (get fixture k)]
            (= out (m/validate raw v))
            (nil? out)))
   :classify (fn [_ out] (case out true :valid false :invalid nil :unknown))
   :classify-domain #{:valid :invalid :unknown}
   :classify-floor 5
   :num-tests 300
   :golden-path "test/golden/hive-mcp/schema/edn/valid.edn"})

(deftest validate-throws-what-valid-refuses
  (doseq [k [:covtest/point :covtest/name :covtest/mode :covtest/nope]
          v [{:x 1 :y 2} "a" "" nil]]
    (let [verdict (sedn/valid? k v)
          thrown (try (sedn/validate k v) nil (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (case verdict
        true (is (nil? thrown) (pr-str [k v]))
        false (is (= {:type :validation :schema k} (select-keys thrown [:type :schema])) (pr-str [k v]))
        nil (is (= {:key k} thrown) (pr-str [k v]))))))

(deftest-golden schema-for-forms-and-cache-golden
  "test/golden/hive-mcp/schema/edn/schema-for.edn"
  (let [forms (into (sorted-map) (for [k [:covtest/point :covtest/name :covtest/mode :covtest/nope :plain]]
                                   [(str k) (some-> (sedn/schema-for k) m/form)]))
        cached (identical? (sedn/schema-for :covtest/mode) (sedn/schema-for :covtest/mode))
        before (sedn/schema-for :covtest/mode)
        cleared (sedn/reload!)]
    {:forms forms
     :cached-identical cached
     :reload-answers cleared
     :fresh-after-reload (not (identical? before (sedn/schema-for :covtest/mode)))}))
