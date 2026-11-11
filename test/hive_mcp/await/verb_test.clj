(ns hive-mcp.await.verb-test
  "`swarm await create|list|cancel|fire` over a temp registry, and the pure
   create-request parse."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.registry :as registry]
            [hive-mcp.tools.consolidated.await :as await-tool]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private woken (atom []))

(defmethod ports/open-resumer :verb-stub
  [_owner]
  (fn [req] (swap! woken conj (get-in req [:awaiter :id])) :resumed))

(defn- body [resp] (json/read-str (:text resp) :key-fn keyword))

(defn- with-temp-registry [f]
  (let [d   (.toFile (java.nio.file.Files/createTempDirectory
                      "await-verb" (make-array java.nio.file.attribute.FileAttribute 0)))
        reg (registry/make-registry (str (io/file d "registry.edn")))]
    (registry/init! reg)
    (reset! woken [])
    (try (with-redefs [await-tool/current-registry (constantly reg)] (f reg))
         (finally (registry/shutdown! reg)))))

(deftest create-request-parse
  (testing "flat owner params and string keys become the registry request"
    (is (= {:owner {:kind :hive-agent-run :run-id "r" :cwd "/w"}
            :subscription {:type :manual :filter {:sha "a"} :dedup-key :sha}
            :prompt "p" :timeout-ms 500}
           (:ok (await-tool/create-request {:owner_kind "hive-agent-run" :run_id "r" :cwd "/w"
                                            :filter {"sha" "a"} :dedup_key "sha"
                                            :prompt "p" :timeout_ms 500})))))
  (testing "no owner is a bad request"
    (is (r/err? (await-tool/create-request {:prompt "p"}))))
  (testing "a non-positive timeout is a bad request"
    (is (r/err? (await-tool/create-request {:owner_kind "x" :timeout_ms 0})))))

(deftest verbs-create-list-fire-cancel
  (with-temp-registry
    (fn [_reg]
      (let [c (await-tool/handle-create {:id "v1" :owner {"kind" "verb-stub"}
                                         :filter {"ci" "green"} :prompt "next"})]
        (is (not (:isError c)))
        (is (= "armed" (get-in (body c) [:awaiter :status]))))
      (await-tool/handle-create {:id "v2" :owner_kind "verb-stub"})
      (is (= 2 (:count (body (await-tool/handle-list {:status "armed"})))))
      (testing "fire wakes only the awaiter whose filter matches"
        (let [f (body (await-tool/handle-fire {:id "v1" :data {"ci" "green"}}))]
          (is (= ["v1"] (:offered f)))
          (is (= ["v1"] (mapv :id (:woke f)))))
        (is (= ["v1"] @woken)))
      (testing "cancel stops v2 from ever waking"
        (is (= "cancelled" (get-in (body (await-tool/handle-cancel {:id "v2"})) [:awaiter :status])))
        (await-tool/handle-fire {:data {}})
        (is (= ["v1"] @woken)))
      (testing "refusals are MCP errors naming the reason"
        (is (:isError (await-tool/handle-cancel {:id "nope"})))
        (is (:isError (await-tool/handle-create {:owner_kind "no-such-kind"})))
        (is (:isError (await-tool/handle-create {:owner_kind "hive-agent-run"})))))))
