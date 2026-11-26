(ns hive-mcp.await.verb-test
  "`swarm await create|arm|list|cancel|fire` over a temp registry, and the
   pure create-request parse (including the successor-inheritance keys)."
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

(defn- wait-until [ms pred]
  (let [end (+ (System/currentTimeMillis) ms)]
    (loop []
      (let [v (pred)]
        (if (or v (> (System/currentTimeMillis) end)) v (do (Thread/sleep 10) (recur)))))))

(deftest create-request-parse
  (testing "flat owner params and string keys become the registry request"
    (is (= {:owner {:kind :hive-agent-run :run-id "r" :cwd "/w"}
            :subscription {:type :manual :filter {:sha "a"} :dedup-key :sha}
            :prompt "p" :timeout-ms 500}
           (:ok (await-tool/create-request {:owner_kind "hive-agent-run" :run_id "r" :cwd "/w"
                                            :filter {"sha" "a"} :dedup_key "sha"
                                            :prompt "p" :timeout_ms 500})))))
  (testing "the successor-inheritance keys travel into the owner (review B5)"
    (let [ok (:ok (await-tool/create-request
                   {:owner_kind "hive-agent-run" :cwd "/w" :ling_id "L1"
                    :parent "coord-1" :spawned_by "coord-1" :project_id "hive"
                    :kanban_task_id "20261109131615-774a3498" :sandbox true
                    :presets ["ling"] :token_budget 40000 :time_budget_ms 900000
                    :max_budget_usd 2.5
                    :grant {"tools" ["bash"]} :turn_budget {"initial" 50}}))]
      (is (= {:kind :hive-agent-run :cwd "/w" :ling-id "L1" :parent "coord-1"
              :spawned-by "coord-1" :project-id "hive"
              :kanban-task-id "20261109131615-774a3498" :sandbox true
              :presets ["ling"] :token-budget 40000 :time-budget-ms 900000
              :max-budget-usd 2.5 :grant {:tools ["bash"]}
              :turn-budget {:initial 50}}
             (:owner ok)))))
  (testing "a hive-agent-run owner needs no run_id at create: it binds at arm"
    (let [ok (:ok (await-tool/create-request {:owner_kind "hive-agent-run" :cwd "/w"}))]
      (is (= :hive-agent-run (get-in ok [:owner :kind])))
      (is (not (contains? (:owner ok) :run-id)))))
  (testing "no owner is a bad request"
    (is (r/err? (await-tool/create-request {:prompt "p"}))))
  (testing "a non-positive timeout is a bad request"
    (is (r/err? (await-tool/create-request {:owner_kind "x" :timeout_ms 0})))))

(deftest verbs-create-arm-list-fire-cancel
  (with-temp-registry
    (fn [_reg]
      (let [c (await-tool/handle-create {:id "v1" :owner {"kind" "verb-stub"}
                                         :filter {"ci" "green"} :prompt "next"})]
        (is (not (:isError c)))
        (is (= "pending" (get-in (body c) [:awaiter :status]))
            "create leaves the awaiter pending, not armed"))
      (await-tool/handle-create {:id "v2" :owner_kind "verb-stub"})
      (is (= 2 (:count (body (await-tool/handle-list {:status "pending"})))))
      (testing "a fire while pending is recorded and resumes nothing"
        (let [f (body (await-tool/handle-fire {:id "v1" :data {"ci" "green"}}))]
          (is (= ["v1"] (:offered f)))
          (is (= [] (:woke f)))
          (is (= ["v1"] (mapv :id (:pending f))))
          (is (= [] @woken))))
      (testing "arm fires the recorded wake"
        (let [a (body (await-tool/handle-arm {:id "v1" :run_id "run-v1"}))]
          (is (true? (:fired? a)))
          (is (= "armed" (get-in a [:awaiter :status])))
          (is (= "run-v1" (get-in a [:awaiter :owner :run-id]))))
        (is (wait-until 2000 #(= ["v1"] @woken)))
        ;; The resumer runs before resume! marks the row :woken, so wait for
        ;; the status too instead of reading it right after the stub fired.
        (is (= "woken" (wait-until 2000
                                   #(:status (get-in (body (await-tool/handle-list {:status "woken"}))
                                                     [:awaiters 0]))))))
      (testing "cancel stops v2 for good"
        (is (= "cancelled" (get-in (body (await-tool/handle-cancel {:id "v2"})) [:awaiter :status])))
        (await-tool/handle-fire {:data {}})
        (is (= ["v1"] @woken)))
      (testing "refusals are MCP errors naming the reason"
        (is (:isError (await-tool/handle-cancel {:id "nope"})))
        (is (:isError (await-tool/handle-arm {:id "nope"})))
        (is (:isError (await-tool/handle-arm {})))
        (is (:isError (await-tool/handle-create {:owner_kind "no-such-kind"})))))))
