(ns hive-mcp.agent.ling.spawned-by-test
  "Every spawn path records :slave/spawned-by, decided once by the spawn plan
   (`resolve-spawned-by`), not only the `agent spawn` tool handler. A spawn
   with no known spawner records none and says so at warn.

   The registry is a recording ISpawnStore stub installed through the
   spawn-store port, so the test needs no swarm addon on the classpath."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.terminal :as term]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.ling.spawn :as spawn]
            [hive-mcp.agent.ling.spawn-store :as ss]
            [hive-mcp.agent.ling.start-preflight :as sp]
            [hive-mcp.agent.ling.terminal-registry :as terminal-registry]
            [hive-mcp.context.request :as request-ctx]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord RecordingStore [rows]
  ss/ISpawnStore
  (add-slave! [_ id attrs] (swap! rows assoc id attrs) {:added id})
  (remove-slave! [_ id] (swap! rows dissoc id) nil)
  (update-slave! [_ id u] (swap! rows update id merge u) nil)
  (claims-for-slave [_ _] []))

(def ^:private ^:dynamic *rows* nil)

(use-fixtures :each
  (fn [f]
    (let [rows (atom {})]
      (ss/set-store! (->RecordingStore rows))
      (try (binding [*rows* rows] (f))
           (finally (ss/reset-store!))))))

(def ^:private trust-tmp
  (reify sp/IFolderTrust
    (trusted-roots [_] #{"/tmp"})
    (checkout-root [_ _] nil)))

(defn- stub-terminal []
  (reify term/ITerminalAddon
    (terminal-id [_] :claude)
    (terminal-spawn! [_ ctx _opts] (:id ctx))
    (terminal-dispatch! [_ _ _] true)
    (terminal-status [_ _ ds-status] ds-status)
    (terminal-kill! [_ ctx] {:killed? true :id (:id ctx)})
    (terminal-interrupt! [_ ctx] {:success? true :ling-id (:id ctx)})))

(defn- with-claude-terminal [f]
  (let [prior (terminal-registry/get-terminal-addon :claude)]
    (terminal-registry/register-terminal! :claude (stub-terminal))
    (try (binding [sp/*folder-trust* trust-tmp] (f))
         (finally
           (if prior
             (terminal-registry/register-terminal! :claude prior)
             (terminal-registry/deregister-terminal! :claude))))))

(defn- spawned-by-of [id]
  (get-in @*rows* [id :slave/spawned-by]))

(defn- recorded? [id]
  (contains? @*rows* id))

(defn- in-request [caller-id f]
  (request-ctx/with-request-context
    (request-ctx/make-request-ctx {:caller-id caller-id})
    (f)))

(defn- spawn! [id opts]
  (ling/create-ling! id (merge {:cwd "/tmp/sb" :spawn-mode :claude} opts)))

(deftest create-ling-outside-the-tool-handler-records-the-request-caller
  (with-claude-terminal
    (fn []
      (testing "a direct create-ling! (dag, workflows, swarm spawn) reads the caller from the request"
        (in-request "ling-parent-7" #(spawn! "sb-direct" {}))
        (is (recorded? "sb-direct"))
        (is (= "ling-parent-7" (spawned-by-of "sb-direct"))))
      (testing "a session-less coordinator lane is root-level: \"\""
        (in-request "coordinator" #(spawn! "sb-root" {}))
        (is (= "" (spawned-by-of "sb-root"))))
      (testing "a coordinator session is recorded as that session"
        (in-request "coordinator:abc123" #(spawn! "sb-session" {}))
        (is (= "coordinator:abc123" (spawned-by-of "sb-session"))))
      (testing "an explicit :spawned-by (captured by a scheduler) wins over the request"
        (in-request "someone-else" #(spawn! "sb-explicit" {:spawned-by "dag-starter"}))
        (is (= "dag-starter" (spawned-by-of "sb-explicit"))))
      (testing "an explicit :parent does not become the spawner"
        (in-request "real-caller" #(spawn! "sb-parent" {:parent "other-ling"}))
        (is (= "real-caller" (spawned-by-of "sb-parent")))))))

(deftest unknown-spawner-is-not-recorded-and-is-logged
  (with-claude-terminal
    (fn []
      (let [warned (atom [])]
        (log/with-merged-config
          {:min-level :trace
           :appenders {:capture {:enabled? true :min-level :warn
                                 :fn (fn [{:keys [level vargs]}]
                                       (when (= :warn level)
                                         (swap! warned conj (pr-str vargs))))}}}
          (spawn! "sb-unknown" {}))
        (is (recorded? "sb-unknown"))
        (is (nil? (spawned-by-of "sb-unknown"))
            "no request in flight: nothing recorded, never a silent \"\"")
        (is (some #(re-find #"no spawner known" %) @warned))))))

(deftest resolve-spawned-by-decides-in-one-place
  (is (= "x" (spawn/resolve-spawned-by "l" {:spawned-by "x"} (constantly "y"))))
  (is (= "" (spawn/resolve-spawned-by "l" {:spawned-by ""} (constantly "y"))))
  (is (= "y" (spawn/resolve-spawned-by "l" {} (constantly "y"))))
  (is (nil? (spawn/resolve-spawned-by "l" {} (constantly nil)))))
