(ns hive-mcp.transport.olympus.snapshots-test
  "Facets for hive-mcp.transport.olympus.snapshots (cov-facets f07).

   serialize-date is pure: a trifecta over Dates, strings, keywords and nil.
   The snapshot builders read the swarm DataScript and the memory tools
   through concretions (no injected port), so they are not golden-pinned on
   data; instead each builder's output is checked against its contract
   schema, which holds whatever the JVM happens to hold (the builders rescue
   to an empty snapshot)."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.transport.olympus.snapshots :as snap]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private DateIn
  [:or
   [:fn {:gen/schema [:int {:min 0 :max 4102444800000}]
         :gen/fmap #(java.util.Date. (long %))}
    #(instance? java.util.Date %)]
   [:string {:max 8}]
   [:enum :kw 'sym]
   :nil])

(deftrifecta-from-schema serialize-date-contract snap/serialize-date
  {:in  [:cat DateIn]
   :out [:maybe snap/IsoString]
   :rel (fn [[d] out]
          (cond (nil? d) (nil? out)
                (instance? java.util.Date d) (= (.toInstant ^java.util.Date d)
                                                (java.time.Instant/parse out))
                :else (= out (str d))))
   :num-tests 200})

(deftest-golden serialize-date-golden
  "test/golden/hive-mcp/f07-olympus-serialize-date.edn"
  {:epoch    (snap/serialize-date (java.util.Date. 0))
   :y2026    (snap/serialize-date (java.util.Date. 1792540800000))
   :string   (snap/serialize-date "2026-10-21")
   :keyword  (snap/serialize-date :kw)
   :nil      (snap/serialize-date nil)})

(deftest snapshot-builders-conform-to-their-contracts
  (is (m/validate [:vector snap/AgentRow] (snap/build-agents-snapshot)))
  (is (m/validate snap/KgSnapshot (snap/build-kg-snapshot)))
  (is (m/validate snap/ProjectTreeSnapshot (snap/build-project-tree-snapshot)))
  (let [full (snap/build-full-snapshot)]
    (is (m/validate snap/FullSnapshot full))
    (is (= :init-snapshot (:type full)))))
