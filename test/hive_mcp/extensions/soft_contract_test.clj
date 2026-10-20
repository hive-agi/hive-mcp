(ns hive-mcp.extensions.soft-contract-test
  "Contract facets for late host binding: a symbol whose namespace loads
   resolves and runs; any other answers nil or the fallback."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.extensions.soft :as soft]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private present #{'clojure.core/+ 'clojure.string/upper-case})

(def ^:private Sym
  [:enum 'clojure.core/+ 'clojure.string/upper-case 'hive-mcp.m11-absent/f 'hive-mcp.m11-unqualified])

(deftrifecta-from-schema resolve-soft-answers-nil-for-an-absent-namespace
  #'soft/resolve-soft
  {:in [:cat Sym]
   :out [:maybe [:fn {:gen/elements [#'clojure.core/+]} var?]]
   :rel (fn [[sym] out] (= (contains? present sym) (some? out)))
   :num-tests 50})

(def ^:private Fallback
  [:fn {:gen/elements [(constantly :fallback)]} ifn?])

(deftrifecta-from-schema host-or-calls-the-host-or-the-fallback
  #'soft/host-or
  {:in [:cat [:enum 'clojure.core/+ 'hive-mcp.m11-absent/f] Fallback [:* [:int {:min -50 :max 50}]]]
   :out [:or :int [:= :fallback]]
   :rel (fn [[sym _ & args] out]
          (if (= 'clojure.core/+ sym) (= (apply + args) out) (= :fallback out)))
   :num-tests 100})

(deftest a-host-throw-propagates
  (is (thrown? ClassCastException
               (soft/host-or 'clojure.core/+ (constantly :fallback) "not-a-number" 1))))
