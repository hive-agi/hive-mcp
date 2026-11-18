(ns hive-mcp.tools.consolidated.session-history-test
  "The `session history` verb on the session tool surface (SYNTHESIS-HISTORY
   slice (c)). The walk itself is proven in hive-mcp.crystal.history-test; here
   the verb's wiring: registered, advertised, project resolution, and the
   no-port refusal. The port is passed as a value, never redefined."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.crystal.history :as h]
            [hive-mcp.dispatch.handler :as dispatch]
            [hive-mcp.dns.result :as result]
            [hive-mcp.tools.consolidated.session :as session]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private d1 "20261116090000-00000001")
(def ^:private d2 "20261117090000-00000002")

(defn- port []
  (reify h/ISummaryHistory
    (-summaries [_ pid]
      (if (= pid "proj-x")
        [{:id d1 :project-id "proj-x" :tags ["session-summary"]}
         {:id d2 :project-id "proj-x" :tags ["session-summary"]}]
        []))
    (-superseded-by [_ id] (if (= id d2) [d1] []))
    (-entry [_ id] {:id id :content (str "wrap " id)})))

(deftest history-is-a-session-verb
  (is (contains? session/handlers :history))
  (is (dispatch/handler? (:history session/handlers)))
  (is (some #{"history"} (get-in session/tool-def [:inputSchema :properties "command" :enum])))
  (testing "its params are advertised, so the project root forwards them"
    (is (contains? (get-in session/tool-def [:inputSchema :properties]) "project_id"))
    (is (contains? (get-in session/tool-def [:inputSchema :properties]) "limit"))))

(deftest history*-walks-the-named-project
  (let [r (session/history* (port) {:project_id "proj-x" :limit 5})
        v (:ok r)]
    (is (result/ok? r))
    (is (= "proj-x" (:project-id v)))
    (is (= [d2 d1] (mapv :id (:entries v))))
    (is (= :end (:stop v)))
    (is (re-find #"^session history proj-x: 2 summaries" (:text v)))))

(deftest history*-defaults-to-the-directory-project
  (let [v (:ok (session/history* (port) {:directory "/nonexistent/hive-history-probe"}))]
    (is (string? (:project-id v)))
    (is (= :empty (:stop v)))))

(deftest history*-refuses-without-a-port
  (let [r (session/history* nil {:project_id "proj-x"})]
    (is (not (result/ok? r)))
    (is (= :session/history-unavailable (:error r)))))

(deftest handle-history-answers-json
  (let [resp (session/handle-history {:project_id "no-such-project-probe" :limit 2})]
    (is (map? resp))
    (testing "either the walk (store up) or the unavailable error (no store), never a throw"
      (if (:isError resp)
        (is (re-find #"history" (str (:text resp))))
        (is (= "no-such-project-probe"
               (:project-id (json/read-str (:text resp) :key-fn keyword))))))))
