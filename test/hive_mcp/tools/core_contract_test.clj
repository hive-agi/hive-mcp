(ns hive-mcp.tools.core-contract-test
  "Contract facets for the MCP response constructors and the Elm-style
   parameter coercers. No :agent-id is passed, so the process-global
   instruction queue is never drained or written."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.core :as core]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private numeric-strings
  "The numeric strings RawValue draws, with the long each one parses to."
  {"42" 42 "-7" -7})

(def ^:private RawValue
  "What a caller can hand a coercer: ints, numeric and junk strings, JSON
   arrays, sequences, nil and other scalars, so every branch is reached."
  [:or :int [:enum "42" "-7" "abc" "" "[\"a\",\"b\"]" "[1" "{\"a\":1}" "plain, list"]
   :nil :boolean :keyword [:vector :int]])

(defn- refused
  "Run f; a throw (malli :invalid-input or the fn's own ex-info) is :refused."
  [f]
  (try (f) (catch Throwable _ :refused)))

(deftrifecta-from-schema coerce-int-follows-the-branch-table
  #'core/coerce-int
  {:in [:cat RawValue [:enum :limit :offset] [:maybe [:enum 0 20]]]
   :out core/CoercedInt
   :rel (fn [[v _ d] out]
          (cond
            (nil? v)     (if (some? d) (= {:ok d} out) (string? (:error out)))
            (integer? v) (= {:ok v} out)
            (string? v)  (if-let [n (numeric-strings v)]
                           (= {:ok n} out)
                           (string? (:error out)))
            :else        (string? (:error out))))
   :classify (fn [_ out] (if (contains? out :ok) :ok :error))
   :classify-domain #{:ok :error}
   :golden-path "test/golden/hive-mcp/tools/core-coerce-int.edn"
   :num-tests 200})

(deftrifecta-from-schema coerce-vec-follows-the-branch-table
  #'core/coerce-vec
  {:in [:cat RawValue [:enum :tags :ids] [:maybe [:enum [] ["d"]]]]
   :out core/CoercedVec
   :rel (fn [[v _ d] out]
          (cond
            (nil? v)        (= {:ok (if (some? d) d [])} out)
            (sequential? v) (= {:ok (vec v)} out)
            (= v "[\"a\",\"b\"]") (= {:ok ["a" "b"]} out)
            :else           (string? (:error out))))
   :classify (fn [_ out] (if (contains? out :ok) :ok :error))
   :classify-domain #{:ok :error}
   :golden-path "test/golden/hive-mcp/tools/core-coerce-vec.edn"
   :num-tests 200})

(golden/deftest-golden response-constructors-golden
  "test/golden/hive-mcp/tools/core-responses.edn"
  {:success-string (core/mcp-success "done")
   :success-data   (core/mcp-success {:a 1})
   :error          (core/mcp-error "nope")
   :error-nil      (core/mcp-error nil)
   :json           (core/mcp-json {:a [1 2]})
   :hm-no-agent    (core/mcp-success+hm "t")
   :timeout        [(core/emacs-timeout-ms {}) (core/emacs-timeout-ms {:timeout_ms "60000"})
                    (refused #(core/emacs-timeout-ms {:timeout_ms "soon"}))]
   :bang           [(core/coerce-int! "5" :n) (refused #(core/coerce-int! "x" :n))
                    (core/coerce-vec! nil :v ["d"]) (refused #(core/coerce-vec! 3 :v))]})

(deftest responses-validate-against-their-own-arm
  (testing "an error response is not a JSON success, and the reverse"
    (is (m/validate core/McpErrorResponse (core/mcp-error "x")))
    (is (not (m/validate core/McpJsonResponse (core/mcp-error nil))))
    (is (m/validate core/McpJsonResponse (core/mcp-json [1])))
    (is (not (m/validate core/McpErrorResponse (core/mcp-json [1]))))
    (is (= [1] (json/read-str (:text (core/mcp-json [1])))))))
