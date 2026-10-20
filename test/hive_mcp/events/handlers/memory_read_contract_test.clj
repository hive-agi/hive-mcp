(ns hive-mcp.events.handlers.memory-read-contract-test
  "Contract facets for the memory read-path handlers. Retrieval of an id no
   store holds answers the tool's not-found response, which needs no entry and
   writes nothing."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.memory-read :as memory-read]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-mcp.protocols.memory :as mem-proto]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord EmptyStore []
  mem-proto/IMemoryStore
  (connect! [_ _] nil)
  (disconnect! [_] nil)
  (connected? [_] true)
  (health-check [_] {:healthy? true})
  (add-entry! [_ _] nil)
  (get-entry [_ _] nil)
  (update-entry! [_ _ _] nil)
  (delete-entry! [_ _] nil)
  (query-entries [_ _] nil)
  (search-similar [_ _ _] nil)
  (supports-semantic-search? [_] false)
  (cleanup-expired! [_] nil)
  (entries-expiring-soon [_ _ _] nil)
  (find-duplicate [_ _ _ _] nil)
  (store-status [_] {:configured? true})
  (reset-store! [_] nil))

(defn- with-empty-store
  "Run the namespace against a store that holds no entry, restoring whatever
   :default store was registered before."
  [f]
  (let [prior (get (mem-proto/registered-stores) :default)]
    (try
      (mem-proto/register-store! :default (->EmptyStore))
      (f)
      (finally
        (if prior
          (mem-proto/register-store! :default prior)
          (mem-proto/unregister-store! :default))))))

(use-fixtures :once with-empty-store)

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
