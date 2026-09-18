(ns hive-mcp.tools.swarm.collect-headless-test
  "Collect of a task Emacs does not own waits for the journal instead of
   answering \"Task not found\"."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.tools.swarm.collect :as collect]
            [hive-mcp.tools.swarm.channel :as channel]
            [hive-mcp.test.stub.elisp :as elisp-stub]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- emacs-answering
  "Elisp transport: the swarm addon is loaded, and every collect answers
   COLLECT-JSON after running ON-COLLECT."
  [collect-json on-collect]
  (fn [code]
    (cond
      (str/includes? code "featurep") {:success true :result "t"}
      (str/includes? code "swarm-api-collect") (do (on-collect) {:success true :result collect-json})
      :else {:success false :error (str "unexpected elisp: " code)})))

(defn- body [resp] (json/read-str (:text resp) :key-fn keyword))

(deftest a-task-emacs-does-not-own-is-collected-from-the-journal
  (let [task-id (str "headless-" (random-uuid))
        not-found "{\"status\":\"error\",\"error\":\"Task not found\"}"
        resp (elisp-stub/with-eval-elisp
               (emacs-answering not-found
                                #(channel/record-task-result!
                                  task-id {:status "completed" :result "done" :slave-id "w"}))
               #(collect/handle-swarm-collect {:task_id task-id :timeout_ms 5000}))]
    (is (not (:isError resp)))
    (is (= "completed" (:status (body resp))))
    (is (= "done" (:result (body resp))))))

(deftest an-unknown-task-times-out-rather-than-failing-fast
  (testing "without a journal entry, a task Emacs does not own is still pending"
    (let [resp (elisp-stub/with-eval-elisp
                 (emacs-answering "{\"status\":\"error\",\"error\":\"Task not found\"}" (fn []))
                 #(collect/handle-swarm-collect {:task_id (str "absent-" (random-uuid))
                                                 :timeout_ms 1}))]
      (is (= "timeout" (:status (body resp)))))))

(deftest a-real-task-error-stays-terminal
  (let [resp (elisp-stub/with-eval-elisp
               (emacs-answering "{\"status\":\"error\",\"error\":\"Task crashed\"}" (fn []))
               #(collect/handle-swarm-collect {:task_id (str "vterm-" (random-uuid))
                                               :timeout_ms 5000}))]
    (is (= "error" (:status (body resp))))
    (is (= "Task crashed" (:error (body resp))))))
