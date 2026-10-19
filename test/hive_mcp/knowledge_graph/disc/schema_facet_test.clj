(ns hive-mcp.knowledge-graph.disc.schema-facet-test
  "The disc value objects: every one generates, and a golden of which probe
   values each accepts."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.knowledge-graph.disc.schema :as ds]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest every-disc-schema-generates
  (doseq [[sym v] (ns-publics 'hive-mcp.knowledge-graph.disc.schema)]
    (testing (str sym)
      (let [xs (mg/sample @v {:size 10 :seed 14})]
        (is (= 10 (count xs)))
        (is (every? #(m/validate @v %) xs))))))

(def ^:private probes
  {:date (java.util.Date. 0) :instant (java.time.Instant/ofEpochMilli 0)
   :double 0.5 :zero 0 :int 3 :big-int 5000000 :inf Double/POSITIVE_INFINITY
   :string "a" :blank "" :nil nil :hex (apply str (repeat 64 "a"))
   :hash-ok {:hash (apply str (repeat 64 "0")) :exists? true} :hash-absent {:exists? false}
   :hash-bad {:hash "zz" :exists? true}})

(golden/deftest-golden disc-schema-acceptance-golden
  "test/golden/hive-mcp/m14-disc-schema.edn"
  (into (sorted-map)
        (for [[sname s] {:Date ds/Date :Instant ds/Instant :PositiveNumber ds/PositiveNumber
                         :Score ds/Score :Path ds/Path :Sha256Hex ds/Sha256Hex
                         :HashResult ds/HashResult :StalenessSource ds/StalenessSource}]
          [sname (into (sorted-map) (for [[p v] probes] [p (m/validate s v)]))])))
