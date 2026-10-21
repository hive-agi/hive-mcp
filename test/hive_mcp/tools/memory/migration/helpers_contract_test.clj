(ns hive-mcp.tools.memory.migration.helpers-contract-test
  "Contract facets for the scope-tag helpers of memory migration: hash-scope
   detection, scope-id extraction, orphan detection and tag rewriting."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.memory.migration.helpers :as h]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private ScopeId
  "Scope ids on both sides of the 12-char hash threshold."
  [:enum "abcdef0123456" "0123456789abcdef" "abc123" "hive-mcp" "ABCDEF0123456" ""])

(def ^:private Tag
  [:or
   [:enum nil :kw "note" "scope:global"]
   [:fn {:gen/schema ScopeId :gen/fmap #(str "scope:project:" %)} string?]])

(deftrifecta-from-schema hash-scope-is-a-long-lowercase-hex-id
  #'h/hash-scope?
  {:in [:cat [:or ScopeId [:enum nil 42]]]
   :out :boolean
   :rel (fn [[s] out]
          (= out (boolean (and (string? s) (> (count s) 12)
                               (every? (set "0123456789abcdef") s)))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 200})

(deftrifecta-from-schema extract-scope-id-strips-the-project-prefix
  #'h/extract-scope-id
  {:in [:cat Tag]
   :out [:maybe :string]
   :rel (fn [[t] out]
          (if (and (string? t) (str/starts-with? t "scope:project:"))
            (= out (subs t (count "scope:project:")))
            (nil? out)))
   :classify (fn [_ out] (if out :scoped :other))
   :classify-domain #{:scoped :other}
   :num-tests 200})

(deftrifecta-from-schema orphaned-scope-tag-is-a-hashed-project-tag
  #'h/orphaned-scope-tag?
  {:in [:cat Tag]
   :out [:maybe :boolean]
   :rel (fn [[t] out] (= (boolean out) (boolean (some-> (h/extract-scope-id t) h/hash-scope?))))
   :num-tests 200})

(deftrifecta-from-schema update-scope-tag-rewrites-only-the-old-tag
  #'h/update-scope-tag
  {:in [:cat [:vector {:max 4} Tag] ScopeId ScopeId]
   :out [:vector :any]
   :rel (fn [[tags old new] out]
          (= out (mapv #(if (= % (str "scope:project:" old)) (str "scope:project:" new) %) tags)))
   :num-tests 200})

(golden/deftest-golden scope-tag-helpers-golden
  "test/golden/hive-mcp/tools/memory/migration-helpers.edn"
  (sorted-map
   :hash-long    (h/hash-scope? "0123456789abcdef")
   :hash-short   (h/hash-scope? "abc123")
   :hash-named   (h/hash-scope? "hive-mcp-legacy")
   :extract      (h/extract-scope-id "scope:project:hive-mcp")
   :extract-none (h/extract-scope-id "scope:global")
   :orphan       (h/orphaned-scope-tag? "scope:project:0123456789abcdef")
   :rewrite      (h/update-scope-tag ["note" "scope:project:old"] "old" "new")
   :edn-missing  (h/read-hive-project-edn "/nonexistent-m29-dir")))

(deftest edn-update-on-a-missing-dir-is-an-error-map
  (is (string? (:error (h/update-hive-project-edn! "/nonexistent-m29-dir/x" "a" "b")))))
