(ns hive-mcp.tools.presets-write-outcome-test
  "preset add/delete answer the store's outcome, not the input id."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.tools.presets :as tools]
            [hive-mcp.presets.core :as presets]
            [hive-mcp.protocols.vector :as vp]
            [hive-mcp.vectordb.memory-store :as mem-store]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.test-fixtures :as fixtures]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord RefusingWrites [inner]
  vp/IVectorCollectionStore
  (-configure [_ opts] (vp/-configure inner opts))
  (-get-collection [_ n] (vp/-get-collection inner n))
  (-create-collection [_ n opts] (vp/-create-collection inner n opts))
  (-delete-collection [_ c] (vp/-delete-collection inner c))
  (-add [_ _ _ _] {:error :store/full :message "disk full"})
  (-get [_ c opts] (vp/-get inner c opts))
  (-query [_ c e opts] (vp/-query inner c e opts))
  (-delete [_ _ _] {:error :store/readonly :message "read-only"})
  (-update [_ c rs] (vp/-update inner c rs)))

(def ^:dynamic *store* nil)

(defn- with-store [f]
  (let [prior-provider (active/get-embedding-provider)
        prior-store    (vp/get-store)]
    (active/set-embedding-provider! (fixtures/->MockEmbedder 8))
    (try
      (f)
      (finally
        (presets/reset-collection-cache!)
        (active/set-embedding-provider! prior-provider)
        (if prior-store (vp/set-store! prior-store) (vp/clear-store!))))))

(use-fixtures :each with-store)

(defn- install! [store]
  (vp/set-store! store)
  (presets/reset-collection-cache!)
  store)

(defn- body [resp]
  (json/read-str (:text resp) :key-fn keyword))

(defn- add! [name]
  (tools/handle-preset-add {:name name :content "# x\nbody"}))

(defn- delete! [name]
  (tools/handle-preset-delete {:name name}))

(deftest add-and-delete-succeed-against-a-healthy-store
  (install! (mem-store/in-memory-store))
  (let [added (add! "alpha")]
    (is (not (:isError added)))
    (is (= "alpha" (:id (body added)))))
  (is (some? (presets/get-preset "alpha")))
  (let [deleted (delete! "alpha")]
    (is (not (:isError deleted)))
    (is (true? (:success (body deleted)))))
  (is (nil? (presets/get-preset "alpha"))))

(deftest delete-of-a-missing-preset-is-a-failure
  (install! (mem-store/in-memory-store))
  (let [resp (delete! "never-existed")]
    (is (:isError resp))
    (is (re-find #"No such preset" (:text resp)))))

(deftest add-reported-failed-by-the-store-is-a-failure
  (install! (->RefusingWrites (mem-store/in-memory-store)))
  (let [resp (add! "beta")]
    (is (:isError resp))
    (is (re-find #"disk full" (:text resp)))))

(deftest delete-reported-failed-by-the-store-is-a-failure
  (let [inner (mem-store/in-memory-store)]
    (install! inner)
    (is (not (:isError (add! "gamma"))))
    (install! (->RefusingWrites inner))
    (let [resp (delete! "gamma")]
      (is (:isError resp))
      (is (re-find #"read-only" (:text resp))))))

(deftest write-outcome-classification
  (testing "nil and plain values are success, an :error map is failure"
    (is (= {:ok :id} (vp/write-outcome->result nil :id)))
    (is (= {:ok :id} (vp/write-outcome->result {:count 1} :id)))
    (is (= :store/full (:error (vp/write-outcome->result {:error :store/full} :id))))))
