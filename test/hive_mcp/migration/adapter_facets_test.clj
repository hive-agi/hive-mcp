(ns hive-mcp.migration.adapter-facets-test
  "Facets for migration adapters. IAdapter gets a port law suite over both
   built-in adapters; transform-export/-import get a golden through the
   registry, including the unknown-adapter throw."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.migration.adapter :as a]
            [hive-test.golden :refer [deftest-golden]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private gen-internal
  "Internal data the JSON adapter round-trips: maps of namespaced keyword
   keys to namespaced keywords, strings without a slash, and ints."
  (let [kw (gen/elements [:memory/note :kg/edge :backup/scope])]
    (gen/map kw (gen/one-of [kw gen/small-integer (gen/elements ["plain" "text"])]) {:max-elements 4})))

(defspec json-adapter-round-trips-namespaced-keywords 200
  (prop/for-all [data gen-internal]
    (= data (a/transform-import :json (a/transform-export :json data)))))

(defspec edn-adapter-is-identity 100
  (prop/for-all [data gen-internal]
    (and (= data (a/transform-export :edn data))
         (= data (a/transform-import :edn data {:any :opt})))))

(deftest-golden transforms-golden
  "test/golden/migration/adapter-transforms.edn"
  {:json-export   (a/transform-export :json {:memory/type :note/decision :tags ["a"] :plain :k})
   :json-import   (a/transform-import :json {"memory/type" "note/decision" "x" "a/b/c"})
   :edn-export    (a/transform-export :edn {:a 1} {:opt true})
   :validate      {:edn-map  (a/validate-external (a/get-adapter :edn) {:a 1})
                   :edn-vec  (a/validate-external (a/get-adapter :edn) [1])
                   :json-vec (a/validate-external (a/get-adapter :json) [1])
                   :json-str (a/validate-external (a/get-adapter :json) "x")}
   :unknown       (try (a/transform-export :nope {}) (catch clojure.lang.ExceptionInfo e
                                                       {:msg (ex-message e) :adapter (:adapter (ex-data e))}))
   :unknown-imp   (try (a/transform-import :nope {}) (catch clojure.lang.ExceptionInfo e (ex-message e)))
   :listed-ids    (vec (sort (filter #{:edn :json} (map :id (a/list-adapters)))))
   :missing       (a/get-adapter :never-registered)})

(deftest adapter-port-laws
  (doseq [ad [(a/->IdentityAdapter) (a/->JsonAdapter)]]
    (testing (str (a/adapter-id ad))
      (is (= (a/adapter-id ad) (:id (a/adapter-info ad))) "info names the same id")
      (is (contains? (:formats (a/adapter-info ad)) (a/adapter-id ad)))
      (is (identical? ad (a/register-adapter! ad)) "register answers the adapter")
      (is (= ad (a/get-adapter (a/adapter-id ad))))
      (is (true? (:valid? (a/validate-external ad {})))))))
