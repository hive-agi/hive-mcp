(ns hive-mcp.schema.type-token-test
  "Trifectas over the type-token security gate, driven by the contracts.

   The contracts' inputs are :any (permissive in: the gate must answer for
   anything a caller hands it). :any rarely generates a padded, upper-case or
   reader-hostile string, so each :in below is the contract's domain narrowed
   to values that REACH the branches: strings with whitespace and case, the
   charset and length boundaries, keywords, and non-strings."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.schema.type-token :as tt]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private RawType
  "What a caller may hand the gate: strings that need trimming and folding,
   the charset and length edges, keywords, and values that are not types."
  [:or
   [:enum "note" " Note " "DECISION" "a-b_c" "1abc" "a b" "x/y" "q\"" "" "   "
    (apply str (repeat 64 "a")) (apply str (repeat 65 "a"))]
   [:enum :Snippet :axiom :a/b]
   :string
   [:enum 42 nil [] {}]])

(defn- token-text [t] (if (keyword? t) (name t) t))

(deftrifecta-from-schema sanitize-type-folds-to-a-token
  hive-mcp.schema.type-token/sanitize-type
  {:in RawType
   :out [:maybe tt/Token]
   :rel (fn [t out]
          (if (and (or (string? t) (keyword? t))
                   (not (str/blank? (token-text t))))
            (= out (str/lower-case (str/trim (token-text t))))
            (nil? out)))
   :idempotent? true
   :classify (fn [_ out] (some? out))
   :classify-domain #{true false}
   :num-tests 300
   :golden-path "test/golden/hive-mcp/schema/type-token/sanitize-type.edn"})

(deftrifecta-from-schema safe-type-is-the-charset-and-length-gate
  hive-mcp.schema.type-token/safe-type?
  {:in RawType
   :out :boolean
   :rel (fn [t out]
          (= out (boolean
                  (when-let [s (tt/sanitize-type t)]
                    (and (<= (count s) tt/max-type-length)
                         (Character/isLetter (.charAt ^String s 0))
                         (<= (int \a) (int (.charAt ^String s 0)) (int \z))
                         (every? #(or (<= (int \a) (int %) (int \z))
                                      (<= (int \0) (int %) (int \9))
                                      (#{\_ \-} %))
                                 s))))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 300
   :golden-path "test/golden/hive-mcp/schema/type-token/safe-type.edn"})

(deftrifecta-from-schema requested-type-tag-is-built-only-from-safe-tokens
  hive-mcp.schema.type-token/requested-type-tag
  {:in RawType
   :out [:maybe [:string {:min 16}]]
   :rel (fn [t out]
          (if (tt/safe-type? t)
            (and (= out (str tt/requested-type-tag-prefix (tt/sanitize-type t)))
                 (= (tt/sanitize-type t) (tt/requested-type-of [out])))
            (nil? out)))
   :classify (fn [_ out] (some? out))
   :classify-domain #{true false}
   :num-tests 300
   :golden-path "test/golden/hive-mcp/schema/type-token/requested-type-tag.edn"})

(def ^:private Tag
  [:or [:enum "requested-type:note" "requested-type:decision" "requested-type:"
        "kanban" "requested:note" :requested-type:axiom]
   :string])

(deftrifecta-from-schema requested-type-of-reads-the-first-marker
  hive-mcp.schema.type-token/requested-type-of
  {:in [:maybe [:or [:vector Tag] [:set Tag]]]
   :out [:maybe :string]
   :rel (fn [tags out]
          (= out (some #(let [s (str %)]
                          (when (str/starts-with? s tt/requested-type-tag-prefix)
                            (subs s (count tt/requested-type-tag-prefix))))
                       tags)))
   :classify (fn [_ out] (some? out))
   :classify-domain #{true false}
   :num-tests 300
   :golden-path "test/golden/hive-mcp/schema/type-token/requested-type-of.edn"})

(deftest marker-round-trips-and-refuses-hostile-text
  (is (= "requested-type:axiom" (tt/requested-type-tag " Axiom ")))
  (is (= "axiom" (tt/requested-type-of ["x" (tt/requested-type-tag :axiom)])))
  (is (nil? (tt/requested-type-tag "a\" OR 1=1")) "filter-expression text never becomes a tag")
  (is (nil? (tt/requested-type-tag (apply str (repeat 65 "a")))) "oversized tokens are refused"))
