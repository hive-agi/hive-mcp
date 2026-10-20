(ns hive-mcp.tools.magit-contract-test
  "Contract facets for the magit tools' pure helpers: the `files` parameter
   normalizer and the staged-path scope check. The handlers reach the editor
   vessel or git and are covered by the existing magit suites."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.magit :as magit]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Files
  "Every spelling of `files`, one top-level branch per outcome (all, nothing
   usable, paths) so each arm is reached about a third of the time."
  [:or
   [:enum :all 'all "all" ["all"] [" all "] #{"all"}]
   [:enum nil :other 'x "" "  " [] ["" "  "] #{}]
   [:or [:enum "a.clj" " a.clj  b.clj " "all b"]
    [:vector {:min 1 :max 3} [:enum "a" " b " :k]]
    [:set {:min 1 :max 2} [:enum "a" "b"]]]])

(defn- paths
  "Oracle: the non-blank trimmed path strings `files` names."
  [files]
  (cond
    (string? files) (vec (remove str/blank? (str/split (str/trim files) #"\s+")))
    (coll? files)   (->> files (map str) (map str/trim) (remove str/blank?) vec)
    :else           nil))

(deftrifecta-from-schema normalize-files-reads-every-spelling
  #'magit/normalize-files
  {:in [:cat Files]
   :out [:maybe [:or [:= :all] [:vector {:min 1} :string]]]
   :rel (fn [[f] out]
          (cond
            (#{:all 'all} f)            (= :all out)
            (or (keyword? f) (symbol? f) (nil? f)) (nil? out)
            :else (let [ps (paths f)]
                    (cond (empty? ps)      (nil? out)
                          (= ["all"] ps)   (= :all out)
                          :else            (= ps out)))))
   :classify (fn [_ out] (cond (nil? out) :none (= :all out) :all :else :paths))
   :classify-domain #{:none :all :paths}
   :num-tests 200})

;; The seeded cases over Files vary between runs (it mixes sets into enums),
;; so the golden facet is pinned to explicit inputs instead.
(golden/deftest-golden normalize-files-golden
  "test/golden/hive-mcp/tools/magit-normalize-files.edn"
  (into (sorted-map)
        (map (fn [[k v]] [k (magit/normalize-files v)]))
        {:nil nil :kw-all :all :sym-all 'all :str-all "all" :vec-all [" all "]
         :blank "  " :empty-vec [] :other :other :one "a.clj"
         :spaced " a.clj  b.clj " :all-b "all b" :mixed ["a" " b " "" :k]}))

(deftrifecta-from-schema staged-outside-files-keeps-only-unselected
  #'magit/staged-outside-files
  {:in [:cat [:vector {:max 3} [:enum "a.clj" "./a.clj" "src/" "src"]]
        [:vector {:max 4} [:enum "a.clj" "b.clj" "src/x.clj" "srcx.clj"]]]
   :out [:vector :string]
   :rel (fn [[files staged] out]
          (let [sel (map #(if (str/starts-with? % "./") (subs % 2) %) files)
                covered? (fn [p] (some #(or (= p %) (and (str/ends-with? % "/") (str/starts-with? p %))) sel))]
            (= out (vec (remove covered? staged)))))
   :num-tests 200})

(deftest blank-directory-falls-through-to-a-real-one
  (is (not (str/blank? (magit/resolve-directory "   "))))
  (is (= "/tmp/m24" (magit/resolve-directory "/tmp/m24"))))
