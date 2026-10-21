(ns hive-mcp.tools.catchup.caps-contract-test
  "catchup.caps/resolve-caps as a schema-derived trifecta: per-bucket
   precedence profile > provider > literal default, out-of-range values
   ignored. The provider is a map {project-id caps}: a map is callable, so it
   satisfies the provider port and stays printable for the golden."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.catchup.caps :as caps]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private CapsMap
  [:map-of {:gen/max 4} [:enum :axioms :sessions :decisions :snippets :bogus] [:int {:min -5 :max 1005}]])

(defn- valid-cap? [n] (and (integer? n) (<= 0 n 1000)))

(deftrifecta-from-schema resolve-caps-precedence
  hive-mcp.tools.catchup.caps/resolve-caps
  {:in          [:cat [:maybe [:map-of [:enum "p" "q"] CapsMap]]
                 [:enum "p"]
                 [:maybe [:map [:caps CapsMap]]]]
   :out         caps/Caps
   :rel         (fn [[provider pid profile] out]
                  (let [provided (when provider (provider pid))]
                    (every? (fn [[k fallback]]
                              (= (get out k)
                                 (let [pc (get-in profile [:caps k]) prc (get provided k)]
                                   (cond (valid-cap? pc) pc (valid-cap? prc) prc :else fallback))))
                            caps/default-caps)))
   :golden-path "test/golden/catchup/caps-resolve.edn"
   :num-tests   200})

(deftest throwing-provider-falls-back
  (is (= caps/default-caps (caps/resolve-caps (fn [_] (throw (ex-info "down" {}))) "p" nil))))
