(ns hive-mcp.server.routes.wire-coerce-test
  "WIRE-NUMBERS (kanban 20261002211406-17100247): a declared integer / number /
   boolean param that arrives as its text is decoded against the tool's own
   inputSchema before dispatch; anything that does not parse is left alone so
   the handler's own validation still reports it."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.server.routes.wire-coerce :as wc]
            [hive-mcp.channel.piggyback :as piggyback]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private schema
  {:type "object"
   :properties {"n"     {:type "integer" :description "n"}
                "f"     {:type "number" :description "f"}
                "b"     {:type "boolean" :description "b"}
                "s"     {:type "string" :description "s"}
                "multi" {:type ["integer" "string"] :description "either"}
                "any"   {:anyOf [{:type "integer"} {:type "string"}]}}})

;; -----------------------------------------------------------------------------
;; Pure decoder
;; -----------------------------------------------------------------------------

(deftest declared-scalars-are-decoded
  (is (= {"n" 8 "f" 0.5 "b" false "s" "8"}
         (wc/coerce-wire-params schema {"n" "8" "f" "0.5" "b" "false" "s" "8"}))))

(deftest keyword-keys-and-keyword-schema-keys-work-too
  (is (= {:n 8 :b true}
         (wc/coerce-wire-params {:properties {:n {:type "integer"} :b {:type "boolean"}}}
                                {:n "8" :b "true"}))))

(deftest unparseable-values-are-left-for-the-handler
  (is (= {"n" "eight" "f" "fast" "b" "maybe"}
         (wc/coerce-wire-params schema {"n" "eight" "f" "fast" "b" "maybe"})))
  (testing "a fractional integer is not truncated"
    (is (= {"n" "8.5"} (wc/coerce-wire-params schema {"n" "8.5"}))))
  (testing "NaN / Infinity are not JSON numbers"
    (is (= {"f" "NaN" "g" 1} (wc/coerce-wire-params schema {"f" "NaN" "g" 1})))
    (is (= {"f" "Infinity"} (wc/coerce-wire-params schema {"f" "Infinity"})))))

(deftest undeclared-ambiguous-and-typed-values-pass-through
  (is (= {"multi" "8" "any" "8" "other" "8" "n" 3 "b" false}
         (wc/coerce-wire-params schema {"multi" "8" "any" "8" "other" "8" "n" 3 "b" false}))))

(deftest nil-schema-is-a-no-op
  (is (= {"n" "8"} (wc/coerce-wire-params nil {"n" "8"})))
  (is (nil? (wc/coerce-wire-params schema nil))))

;; -----------------------------------------------------------------------------
;; Properties
;; -----------------------------------------------------------------------------

(defspec any-integer-text-decodes-to-that-integer 200
  (prop/for-all [n gen/large-integer]
    (= {"n" n} (wc/coerce-wire-params schema {"n" (str n)}))))

(defspec any-finite-number-text-decodes-to-that-number 200
  (prop/for-all [x (gen/double* {:infinite? false :NaN? false})]
    (= {"f" x} (wc/coerce-wire-params schema {"f" (str x)}))))

(defspec any-boolean-text-decodes-to-that-boolean 50
  (prop/for-all [b gen/boolean]
    (= {"b" b} (wc/coerce-wire-params schema {"b" (str b)}))))

(defspec decoding-never-loses-or-invents-keys-and-is-idempotent 200
  (prop/for-all [params (gen/map (gen/elements ["n" "f" "b" "s" "multi" "any" "x"])
                                 (gen/one-of [gen/string-alphanumeric
                                              (gen/fmap str gen/large-integer)
                                              (gen/elements ["true" "false" "1.5"])
                                              gen/large-integer gen/boolean]))]
    (let [once (wc/coerce-wire-params schema params)]
      (and (= (set (keys params)) (set (keys once)))
           (= once (wc/coerce-wire-params schema once))
           ;; only declared scalar slots may change, and only from a string
           (every? (fn [[k v]]
                     (or (= v (get once k))
                         (and (#{"n" "f" "b"} k) (string? v))))
                   params)))))

(defspec text-that-does-not-parse-is-untouched 200
  (prop/for-all [s (gen/such-that #(and (nil? (parse-long %)) (nil? (parse-double %))
                                        (not (#{"true" "false"} %)))
                                  gen/string-ascii 100)]
    (= {"n" s "f" s "b" s} (wc/coerce-wire-params schema {"n" s "f" s "b" s}))))

;; -----------------------------------------------------------------------------
;; Through the real tool wrapper (the MCP entry point)
;; -----------------------------------------------------------------------------

(deftest make-tool-decodes-wire-text-before-the-handler
  (with-redefs [piggyback/get-messages (fn [& _] nil)]
    (let [seen (atom nil)
          t    (routes/make-tool {:name "wire-numbers-probe" :description "d"
                                  :inputSchema schema
                                  :handler (fn [args] (reset! seen args) "ok")})]
      ((:handler t) {"n" "8" "f" "0.25" "b" "false" "s" "8" "_caller_id" "wire-test"})
      (is (= 8 (:n @seen)))
      (is (= 0.25 (:f @seen)))
      (is (false? (:b @seen)) "the text \"false\" must not reach the handler as truthy")
      (is (= "8" (:s @seen)) "a declared string stays a string")
      (testing "unparseable text still reaches the handler for its own validation"
        ((:handler t) {"n" "eight" "_caller_id" "wire-test"})
        (is (= "eight" (:n @seen)))))))

(deftest an-aliased-param-is-decoded-under-its-advertised-name
  (testing "a `dry-run?` param is advertised as `dry-run_`; the client sends
            that alias, and its text is decoded too"
    (with-redefs [piggyback/get-messages (fn [& _] nil)]
      (let [seen (atom nil)
            t    (routes/make-tool {:name "wire-numbers-alias-probe" :description "d"
                                    :inputSchema {:type "object"
                                                  :properties {"dry-run?" {:type "boolean" :description "b"}}}
                                    :handler (fn [args] (reset! seen args) "ok")})]
        (is (contains? (get-in t [:inputSchema :properties]) "dry-run_"))
        ((:handler t) {"dry-run_" "false" "_caller_id" "wire-test"})
        (is (false? (:dry-run_ @seen)))))))
