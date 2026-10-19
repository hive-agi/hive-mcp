(ns hive-mcp.events.handlers.memory-read-contract-test
  "Contract facets for the memory read-path handlers. Retrieval of an id no
   store holds answers the tool's not-found response, which needs no entry and
   writes nothing."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.memory-read :as memory-read]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private AbsentId
  [:string {:gen/fmap #(str "m11-absent-" %), :min 1}])

(def ^:private TextResponse
  [:map [:mcp-response [:map [:type [:= "text"]] [:text :string]]]])

(deftrifecta-from-schema memory-get-answers-not-found-for-an-absent-id
  #'memory-read/handle-memory-get
  {:in [:cat [:map {:closed true}] [:tuple [:= :memory/get] [:map [:id AbsentId]]]]
   :out TextResponse
   :rel (fn [[_ [_ {:keys [id]}]] out]
          (let [text (get-in out [:mcp-response :text])]
            (and (str/includes? text "not found") (str/includes? text id))))
   :golden-path "test/golden/hive-mcp/events/handlers/memory-get-absent.edn"
   :num-tests 20})

(deftrifecta-from-schema memory-get-metadata-answers-not-found-for-an-absent-id
  #'memory-read/handle-memory-get-metadata
  {:in [:cat [:map {:closed true}] [:tuple [:= :memory/get-metadata] [:map [:id AbsentId]]]]
   :out [:map [:mcp-response [:map [:type [:= "text"]] [:text :string] [:isError [:= true]]]]]
   :rel (fn [[_ [_ {:keys [id]}]] out]
          (= (str "Entry not found: " id) (get-in out [:mcp-response :text])))
   :golden-path "test/golden/hive-mcp/events/handlers/memory-get-metadata-absent.edn"
   :num-tests 20})

(deftest register-handlers-installs-the-read-path-events
  (ev/with-clean-registry
    (let [events [:memory/query :memory/search :memory/get :memory/get-metadata]
          reg (memory-read/register-handlers!)]
      (is (every? #(contains? reg %) events))
      (is (every? ev/handler-registered? events)))))
