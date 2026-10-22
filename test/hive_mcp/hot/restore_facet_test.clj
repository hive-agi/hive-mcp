(ns hive-mcp.hot.restore-facet-test
  "Facets for hive-mcp.hot.restore: `absent` keeps exactly the unloaded names
   the image lacks, in load order, once each (property); `restore!` reports a
   name that cannot load as still unloaded and never throws (golden). Only
   namespaces that do not exist on the classpath are 'restored', so the image
   is never touched."
  (:require [hive-mcp.hot.restore :as restore]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Name
  "Loaded namespaces and names no image has, as symbols or strings."
  [:enum 'clojure.core 'clojure.set "clojure.string" 'hive-mcp.hot.restore
   'covg.never.loaded-a "covg.never.loaded-b" 'covg.never.loaded-c])

(deftrifecta-from-schema absent hive-mcp.hot.restore/absent
  {:in  [:vector {:gen/max 8} Name]
   :out [:vector :symbol]
   :rel (fn [in out]
          (= out (vec (distinct (remove find-ns (map symbol (reverse in)))))))
   :classify (fn [_ out] (if (seq out) :some-absent :all-loaded))
   :classify-domain #{:some-absent :all-loaded}
   :num-tests 200})

(defn- shape
  "restore!'s answer with the error text cut to whether it names the file."
  [{:keys [restored still-unloaded]}]
  {:restored restored
   :still-unloaded (mapv (fn [{:keys [ns error]}]
                           {:ns ns :error-names-classpath? (boolean (re-find #"classpath" error))})
                         still-unloaded)})

(golden/deftest-golden restore-golden
  "test/golden/hive-mcp/hot/restore-missing-namespaces.edn"
  (into (sorted-map)
        (for [[label unloaded] {"none"          []
                                "all-present"   ['clojure.set "clojure.string"]
                                "missing"       ['covg.never.loaded-a "covg.never.loaded-b"]
                                "mixed-repeats" ['covg.never.loaded-a 'clojure.set 'covg.never.loaded-a]}]
          [label (shape (restore/restore! unloaded))])))
