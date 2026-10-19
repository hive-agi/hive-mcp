(ns hive-mcp.extensions.loader-contract-test
  "Contract facets for the extension loader's pure boot-health projection and
   its mount event logger. The probe ids are never mounted in this JVM."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.extensions.loader :as loader]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:enum "m11.probe.a" "m11.probe.b" "m11.probe.c"])
(def ^:private InitNs [:enum "m11.a.init" "m11.b.init"])

(deftrifecta-from-schema roster-facts-splits-mounted-from-failed
  #'loader/roster-facts
  {:in [:cat
        [:vector [:map [:addon/id Id] [:addon/init-ns InitNs]]]
        [:maybe [:map
                 [:report [:map [:mounted [:vector [:map [:addon/id Id] [:success? :boolean]]]]]]
                 [:lifecycle {:optional true} [:map [:dormant [:vector Id]]]]]]
        [:set [:enum 'm11.a.init 'm11.b.init]]]
   :out [:map {:closed true} [:discovered [:vector :string]] [:mounted [:vector :string]]
         [:failed [:vector :string]]]
   :rel (fn [[ordered mount-result successful-ns] out]
          (let [discovered (mapv :addon/id ordered)
                up (into #{} (concat (keep #(when (:success? %) (:addon/id %))
                                           (get-in mount-result [:report :mounted]))
                                     (get-in mount-result [:lifecycle :dormant])
                                     (keep #(when (successful-ns (symbol (:addon/init-ns %)))
                                              (:addon/id %))
                                           ordered)))
                mounted (into #{} (filter up) discovered)]
            (= {:discovered discovered
                :mounted (vec (sort mounted))
                :failed (vec (sort (remove mounted discovered)))}
               out)))
   :golden-path "test/golden/hive-mcp/extensions/roster-facts.edn"
   :num-tests 200})

(deftrifecta-from-schema mount-event-logger-writes-and-answers-nil
  #'loader/mount-event-logger
  {:in [:cat [:map [:level [:enum :debug :warn :error :info :trace]]
              [:event [:enum :mount/started :mount/done]]
              [:addon/id Id]]]
   :out :nil
   :rel (fn [_ out] (nil? out))
   :num-tests 30})
