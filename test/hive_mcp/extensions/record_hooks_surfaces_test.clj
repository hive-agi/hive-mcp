(ns hive-mcp.extensions.record-hooks-surfaces-test
  "Record hooks at the exits #489 left out: the projectile `project list`
   (:project/visible) and the crystal on-kanban-done broadcast and log lines
   (:kanban/history). Each exit first shows the plaintext reaching the sink
   with no hook, then that a registered hook hides it."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.core :as channel]
            [hive-mcp.crystal.hooks :as crystal-hooks]
            [hive-mcp.crystal.persist :as persist]
            [hive-mcp.extensions.record-hooks :as hooks]
            [hive-mcp.project.scope :as project-scope]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.test.stub.swarm-host :as sh]
            [hive-mcp.tools.projectile :as projectile]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private points [:kanban/history :project/visible])

(defn- clean-hooks! []
  (doseq [p points id (hooks/registered p)] (hooks/unregister! p id)))

(use-fixtures :each (fn [t] (clean-hooks!) (try (t) (finally (clean-hooks!)))))

;; =============================================================================
;; :project/visible — projectile `project list`
;; =============================================================================

(defn- write-project! [dir cfg]
  (.mkdirs (io/file dir))
  (spit (io/file dir ".hive-project.edn") (pr-str cfg)))

(defn- hide-sec [entities] (filterv #(not= "pl-sec" (:project/id %)) entities))

(defn- with-known-projects
  "Call (f [pub sec plain]) while projectile knows the roots pub (project
   pl-pub), sec (project pl-sec, enclaved) and plain (in no project)."
  [f]
  (let [base  (str (System/getProperty "java.io.tmpdir") "/projectile-list-" (System/nanoTime))
        pub   (str base "/pub/")
        sec   (str base "/sec/")
        plain (str base "/plain/")
        answer (json/write-str (mapv (fn [r] {:root r :exists true}) [pub sec plain]))]
    (write-project! pub {:project-id "pl-pub"})
    (write-project! sec {:project-id "pl-sec" :seal/enclave "e1"})
    (.mkdirs (io/file plain))
    (try
      (sh/with-swarm-host [_ (sh/answering {:project/list-projects {:success true :result answer}})]
        (f [pub sec plain]))
      (finally
        (doseq [id ["pl-pub" "pl-sec"]] (project-scope/deregister-project-config! id))
        (project-scope/clear-config-cache!)
        (doseq [file (reverse (file-seq (io/file base)))] (io/delete-file file true))))))

(defn- listed-roots [out] (set (map #(get % "root") (json/read-str (:text out)))))

(deftest project-list-shows-every-known-project-without-a-hook
  (with-known-projects
    (fn [roots]
      (is (= (set roots) (listed-roots (projectile/handle-projectile-list-projects {})))))))

(deftest project-list-hides-what-the-visible-hook-hides
  (hooks/register! :project/visible :p/hide #'hide-sec)
  (with-known-projects
    (fn [[pub _sec plain]]
      (let [out (projectile/handle-projectile-list-projects {})]
        (is (not (:isError out)))
        (is (not (str/includes? (:text out) "sec")) "the enclaved root is not listed")
        (is (= #{pub plain} (listed-roots out)) "a public project and a root in no project stay")))))

(deftest project-list-fails-closed-on-an-unreadable-answer
  (hooks/register! :project/visible :p/hide #'hide-sec)
  (sh/with-swarm-host [_ (sh/answering {:project/list-projects {:success true :result "not json ["}})]
    (let [out (projectile/handle-projectile-list-projects {})]
      (is (true? (:isError out)))
      (is (not (str/includes? (pr-str out) "not json"))))))

;; =============================================================================
;; :kanban/history — crystal on-kanban-done
;; =============================================================================

(def ^:private secret "the enclave's task title")

(defn- done!
  "Run on-kanban-done for a task titled `secret`; the channel messages and
   log lines it emitted."
  []
  (let [sent (atom []) logged (atom [])]
    (with-redefs [ds/register-completed-task! (fn [& _] {})
                  persist/persist-wraps!      (fn [& _] {:results [{:id "n1" :success? true}]})
                  channel/server-connected?   (constantly true)
                  channel/broadcast!          (fn [m] (swap! sent conj m))]
      (log/with-merged-config
        {:min-level :trace
         :appenders {:capture {:enabled? true :min-level :trace
                               :fn (fn [d] (swap! logged conj (force (:msg_ d))))}}}
        (crystal-hooks/on-kanban-done {:id "t1" :title secret :project-id "p1"})))
    {:sent @sent :logged @logged}))

(deftest kanban-done-broadcasts-the-title-without-a-hook
  (is (= secret (:title (first (:sent (done!)))))))

(deftest kanban-done-broadcast-and-log-pass-the-history-hook
  (hooks/register! :kanban/history :k/hint #(if (= "t1" (:task-id %)) (assoc % :title "(sealed task)") %))
  (let [{:keys [sent logged]} (done!)]
    (is (= "(sealed task)" (:title (first sent))))
    (is (= "t1" (:task-id (first sent))) "the completion itself is still announced")
    (is (not-any? #(str/includes? (str %) secret) logged)))
  (testing "a dropped row is announced without its title"
    (hooks/register! :kanban/history :k/hint (constantly nil))
    (let [{:keys [sent logged]} (done!)]
      (is (= 1 (count sent)))
      (is (not (contains? (first sent) :title)))
      (is (not (str/includes? (pr-str sent) secret)))
      (is (not-any? #(str/includes? (str %) secret) logged))))
  (testing "a hook that throws announces no title"
    (hooks/register! :kanban/history :k/hint (fn [_] (throw (ex-info "locked" {}))))
    (let [{:keys [sent]} (done!)]
      (is (not (str/includes? (pr-str sent) secret))))))
