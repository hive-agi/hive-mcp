(ns hive-mcp.crystal.persist-facets-test
  "Facets for per-scope wrap persistence. pid->project-id is pure and gets a
   trifecta; persist-wraps! gets a golden with its IMemoryStore port
   satisfied by a recording stub registered through the store registry
   (no with-redefs). The stub answers an id for every entry except one
   whose :project-id is \"refuse\", and throws for \"explode\", so the
   success, non-id and throw arms are all reached."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.crystal.harvest.by-scope :as bs]
            [hive-mcp.crystal.persist :as persist]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden-fn]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema pid->project-id-maps-umbrella
  #'persist/pid->project-id
  {:in [:cat [:or [:= :umbrella] [:enum "hive" "funeraria" ""] :nil]]
   :out :string
   :rel (fn [[pid] out] (= out (if (= pid :umbrella) "multi-project" (str pid))))
   :classify (fn [[pid] _] (if (= pid :umbrella) :umbrella :pid))
   :classify-domain #{:umbrella :pid}
   :golden-path "test/golden/crystal/persist-pid-project-id.edn"
   :num-tests 200})

(defn- recording-store [calls]
  (reify mem-proto/IMemoryStore
    (add-entry! [_ entry]
      (swap! calls conj (select-keys entry [:project-id :type :duration :tags :content]))
      (case (:project-id entry)
        "refuse"  nil
        "explode" (throw (ex-info "store down" {}))
        (str "mem-" (count @calls))))))

(def ^:private wraps
  [{:pid "hive" :entry {:type :note :content "h" :tags ["scope:project:hive"] :duration :short}}
   {:pid "refuse" :entry {:content "r"}}
   {:pid "explode" :entry {:type "decision" :duration "long" :content "e"}}
   {:pid "" :entry {:content "blank pid" :tags nil}}
   {:pid bs/umbrella-sentinel :entry {:type :note :content "u" :tags ["scope:multi-project"]}}])

(defn- with-store [store f]
  (let [before (mem-proto/registered-stores)]
    (mem-proto/reset-registry!)
    (try
      (when store (mem-proto/register-store! :default store))
      (f)
      (finally
        (mem-proto/reset-registry!)
        (doseq [[k s] before] (mem-proto/register-store! k s))))))

(defn- scenario []
  (let [calls (atom [])]
    {:no-store (with-store nil #(persist/persist-wraps! wraps))
     :stubbed  (with-store (recording-store calls) #(persist/persist-wraps! wraps))
     :written  @calls}))

(deftest-golden-fn persist-wraps-golden
  "test/golden/crystal/persist-wraps.edn"
  scenario)

(deftest expires-is-stamped
  (let [calls (atom [])
        store (reify mem-proto/IMemoryStore
                (add-entry! [_ e] (swap! calls conj e) "id-1"))]
    (with-store store #(persist/persist-wraps! [{:pid "hive" :entry {:content "x"}}]))
    (is (= "short" (:duration (first @calls))))
    (is (contains? (first @calls) :expires))))
