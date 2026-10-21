(ns hive-mcp.tools.catchup.caller-contract-test
  "Schema-derived trifectas for catchup.caller: the caller-id split rule and
   the per-caller extension lookup, both pure."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.catchup.caller :as caller]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- digits? [s] (and (seq s) (every? #(Character/isDigit ^char %) s)))

(def ^:private CallerIdIn
  "Raw caller ids that reach both arms: a prefix, an optional ':' and a
   suffix that is digits, mixed or empty. nil is pinned in a deftest below."
  [:string {:gen/schema [:tuple [:enum "" "ling-1" "a:b" "coordinator"]
                         [:enum "" ":" ":"]
                         [:enum "" "7" "1234" "12x"]]
            :gen/fmap   (fn [[p sep s]] (str p sep s))}])

(deftrifecta-from-schema caller-id-candidates-split-rule
  hive-mcp.tools.catchup.caller/caller-id-candidates
  {:in              [:cat CallerIdIn]
   :out             [:vector {:min 1 :max 2} :string]
   :rel             (fn [[raw] out]
                      (and (= raw (first out))
                           (if-let [prefix (second out)]
                             (let [suffix (subs raw (inc (count prefix)))]
                               (and (str/starts-with? raw (str prefix ":"))
                                    (pos? (count prefix))
                                    (digits? suffix)))
                             (not (and (str/includes? raw ":")
                                       (pos? (str/last-index-of raw ":"))
                                       (digits? (subs raw (inc (str/last-index-of raw ":")))))))))
   :classify        (fn [_ out] (if (= 2 (count out)) :split :whole))
   :classify-domain #{:split :whole}
   :golden-path     "test/golden/catchup/caller-id-candidates.edn"
   :num-tests       200})

(def ^:private Provider
  "Per-caller extensions: one answers only for the bare agent id, one for
   every caller, one for nobody."
  [:fn {:gen/elements [(fn [c _] (when (= c "ling-1") :ling-1-hit))
                       (fn [c p] [c p])
                       (constantly nil)]}
   ifn?])

(deftrifecta-from-schema resolve-for-caller-first-truthy
  hive-mcp.tools.catchup.caller/resolve-for-caller
  {:in  [:cat Provider [:maybe CallerIdIn] [:maybe [:enum "p" "global"]]]
   ;; the providers above answer a keyword or a [candidate project-id] pair
   :out [:maybe [:or [:= :ling-1-hit] [:tuple :string [:maybe :string]]]]
   :rel (fn [[f raw pid] out]
          (= out (some #(f % pid) (caller/caller-id-candidates raw))))
   :num-tests 200})

(deftest nil-caller-is-the-coordinator
  (is (= ["coordinator"] (caller/caller-id-candidates nil))))

(deftest session-suffix-falls-back-to-the-agent-id
  (is (= :ling-1-hit (caller/resolve-for-caller (fn [c _] (when (= c "ling-1") :ling-1-hit))
                                                "ling-1:1234" "p")))
  (is (nil? (caller/resolve-for-caller (fn [c _] (when (= c "ling-1") :hit)) "ling-1:12x" "p"))
      "a non-numeric suffix is part of the id"))
